#' Generates posterior predictions for objects fitted by \code{\link{bnec}}
#'
#' Generates posterior predictions for objects fitted by \code{\link{bnec}}.
#' \code{object} should be of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#'
#' @name posterior_predict
#' @order 1
#'
#' @param object An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}.
#' @param newdata An optional \code{\link[base]{data.frame}} to predict over.
#' Where it is \code{NULL}, the default, predictions are made for the data the
#' fit was made from, each row with its own values. See Details for a fit whose
#' formula has a \code{disp(~...)} term naming a column other than the
#' predictor.
#' @param ... Additional arguments to
#' \code{\link[brms]{posterior_predict}}.
#'
#' @details A \code{disp(~z)} term, with \code{z} a column other than the
#' predictor, models the dispersion parameter on \code{z}. The spread of a
#' predicted reading then depends on \code{z}, so a \code{newdata} has to say
#' which value of \code{z} each prediction is for, and one without the column
#' is refused with a message naming it. \code{\link{bnec_newdata}} writes
#' such a column at one fixed value, the first level of a factor or the median
#' of a numeric column, and marks it as a placeholder. Given that
#' \code{newdata}, \code{posterior_predict()} replaces the placeholder draw by
#' draw: each posterior draw takes the value, or the joint values where the
#' term names several columns, of a row sampled at random from the data the
#' fit was made from. The predicted readings are then those of a new
#' observation from the mix of \code{z} in the design, rather than of one
#' level of it. The sampling uses the session's random number stream, as the
#' simulation of the readings themselves does, so \code{\link[base]{set.seed}}
#' before the call reproduces both. A \code{newdata} that holds the column
#' without the mark is used as given, and so is a marked column whose values
#' have been changed from the placeholder, as in a grid from
#' \code{bnec_newdata()} with \code{z} set to the level to predict for.
#'
#' @return See \code{?brms::posterior_predict}.
#'
#' @examples
#' \dontrun{
#' library(bayesnec)
#' # Uses default `resolution` and `x_range` to generate `newdata` internally
#' posterior_predict(manec_example)
#' # Provide user-specified `newdata`
#' nd_ <- data.frame(x = seq(0, 3, length.out = 200))
#' ppreds <- posterior_predict(manec_example, ecx_val = 50, newdata = nd_,
#'                             make_newdata = FALSE)
#' ncol(ppreds) == 200 # cols are x, rows are iterations
#' # Posterior predictions for raw input data
#' nec4param <- pull_out(manec_example, model = "nec4param")
#' preds <- posterior_predict(nec4param, make_newdata = FALSE)
#' x <- pull_brmsfit(nec4param)$data$x
#' plot(sort(x), preds[1, order(x)], type = "l", col = alpha("black", 0.1),
#'      ylim = c(-8, 5))
#' for (i in seq_len(nrow(preds))[-1]) {
#'   lines(sort(x), preds[i, order(x)], type = "l", col = alpha("black", 0.1))
#' }
#' }
NULL

#' @rdname posterior_predict
#' @order 2
#'
#' @method posterior_predict bayesnecfit
#'
#' @inherit posterior_predict description return examples
#'
#' @importFrom brms posterior_predict
#'
#' @export
posterior_predict.bayesnecfit <- function(object, newdata = NULL, ...) {
  disp_posterior_predict(pull_brmsfit(object), object$bayesnecformula,
                         newdata = newdata, ...)
}

#' @rdname posterior_predict
#' @order 3
#'
#' @method posterior_predict bayesmanecfit
#'
#' @inherit posterior_predict description return examples
#'
#' @importFrom brms posterior_predict
#'
#' @export
posterior_predict.bayesmanecfit <- function(object, newdata = NULL, ...) {
  mod_fits <- object$mod_fits
  model_set <- names(mod_fits)
  pred_list <- lapply(mod_fits, function(x, ...) {
    disp_posterior_predict(x$fit, x$bayesnecformula, newdata = newdata, ...)
  }, ...)
  sample_size <- min(sapply(pred_list, nrow))
  # The index realised when the object was built, not a fresh draw here, so the
  # weighting is the same on every call. What each component contributes still
  # varies, because posterior_predict() simulates new observations -- as it does
  # for a single brmsfit. Only the averaging is fixed here. See #216.
  draw_index <- pull_draw_index(object, model_set, sample_size)
  do_wrapper(model_set, w_pred_list_calc, pred_list, draw_index, fct = "rbind")
}

#' Refuse a newdata that lacks a column a disp(~...) term names
#'
#' @param formula An object of class \code{\link{bayesnecformula}}.
#' @param newdata A \code{\link[base]{data.frame}}, or \code{NULL}.
#'
#' @details Raised before \pkg{brms} sees the \code{newdata}, where the
#' failure names the variable but not the term it comes from or what to do
#' about it. Not marginalised instead: a \code{newdata} written by the user
#' says what it is for, and filling in a column it left out would be a choice
#' made on the user's behalf (RF, #443).
#'
#' @return \code{invisible(NULL)}, called for its side effect.
#'
#' @noRd
check_newdata_disp <- function(formula, newdata) {
  if (is.null(newdata)) {
    return(invisible(NULL))
  }
  absent <- setdiff(disp_covariates(formula), names(newdata))
  if (length(absent) == 0) {
    return(invisible(NULL))
  }
  one <- length(absent) == 1
  stop("`newdata` has no column ",
       paste0("\"", absent, "\"", collapse = ", "),
       ", which the disp() term of this fit names. brms computes the",
       " dispersion parameter on every row of `newdata`, for the expected",
       " response as well as for predicted readings, so ",
       if (one) "the column is" else "the columns are", " required. Add ",
       if (one) "it" else "them", " at the values to predict for, or build",
       " `newdata` with bnec_newdata(), which marks ",
       if (one) "it" else "them", " as a placeholder that posterior_predict()",
       " and predict() replace with values drawn from the fitted data.",
       call. = FALSE)
}

#' The marked disp(~...) columns of a newdata that still hold the placeholder
#'
#' @param newdata A \code{\link[base]{data.frame}}.
#' @param formula The \code{\link{bayesnecformula}} of the fit.
#' @param data The data the fit was made from, \code{fit$data}.
#'
#' @details A column is resolved by draws only where it is named in the
#' \code{"bnec_disp_placeholders"} attribute, is a \code{disp(~...)} column of
#' this fit, and holds on every row the value \code{disp_fixed_value()} gives
#' it. The last condition is the one that needs a reason. Assigning to a column
#' keeps a data frame's attributes, so a user who takes a grid from
#' \code{bnec_newdata()} and sets \code{z} to the level to predict for would
#' otherwise have that choice replaced by draws without a message. A changed
#' value is a value the user chose, and it is used as given, as an unmarked
#' column is.
#'
#' @return A \code{\link[base]{character}} vector, possibly empty.
#'
#' @noRd
disp_placeholder_columns <- function(newdata, formula, data) {
  marked <- intersect(attr(newdata, "bnec_disp_placeholders"),
                      disp_covariates(formula))
  unchanged <- vapply(marked, function(v) {
    fixed <- disp_fixed_value(data[[v]])
    same <- if (is.numeric(fixed)) {
      newdata[[v]] == fixed
    } else {
      as.character(newdata[[v]]) == as.character(fixed)
    }
    isTRUE(all(same))
  }, logical(1))
  marked[unchanged]
}

#' Predicted readings from one brmsfit, with disp(~...) placeholders resolved
#'
#' @param fit An object of class \code{\link[brms]{brmsfit}}.
#' @param formula The \code{\link{bayesnecformula}} it was fitted with.
#' @param newdata A \code{\link[base]{data.frame}}, or \code{NULL}.
#' @param ... Further arguments to \code{\link[brms]{posterior_predict}}.
#'
#' @details Without \code{newdata} the call is passed to \pkg{brms}
#' unchanged, and predicts for the fitted rows with their own values. With a
#' \code{newdata} that marks no placeholder it is passed on as given, so a fit
#' without a \code{disp(~...)} column predicts exactly as before #443.
#'
#' @return A \code{\link[base]{matrix}} of draws by rows of \code{newdata}.
#'
#' @importFrom brms posterior_predict
#'
#' @noRd
disp_posterior_predict <- function(fit, formula, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(posterior_predict(fit, ...))
  }
  check_newdata_disp(formula, newdata)
  marked <- disp_placeholder_columns(newdata, formula, fit$data)
  if (length(marked) == 0) {
    return(posterior_predict(fit, newdata = newdata, ...))
  }
  disp_marginal_draws(fit, newdata, marked, ...)$draws
}

#' The predict() counterpart of disp_posterior_predict()
#'
#' @inheritParams disp_posterior_predict
#' @param ... Further arguments to \code{\link[brms]{predict.brmsfit}}.
#'
#' @details Where placeholders are resolved, the draws are summarised as
#' \code{\link[brms]{predict.brmsfit}} summarises its own, with
#' \code{\link[brms]{posterior_summary}} and the same defaults for
#' \code{summary}, \code{robust} and \code{probs}, so the result has the
#' same form either way.
#'
#' @return See \code{?brms::predict.brmsfit}.
#'
#' @importFrom brms posterior_summary
#' @importFrom stats predict
#'
#' @noRd
disp_predict <- function(fit, formula, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(predict(fit, ...))
  }
  check_newdata_disp(formula, newdata)
  marked <- disp_placeholder_columns(newdata, formula, fit$data)
  if (length(marked) == 0) {
    return(predict(fit, newdata = newdata, ...))
  }
  dots <- list(...)
  summary <- if (is.null(dots[["summary"]])) TRUE else dots[["summary"]]
  robust <- if (is.null(dots[["robust"]])) FALSE else dots[["robust"]]
  probs <- if (is.null(dots[["probs"]])) c(0.025, 0.975) else dots[["probs"]]
  dots[c("summary", "robust", "probs")] <- NULL
  draws <- do.call(disp_marginal_draws,
                   c(list(fit, newdata, marked), dots))$draws
  if (!isTRUE(summary)) {
    return(draws)
  }
  posterior_summary(draws, probs = probs, robust = robust)
}

#' Predicted readings marginalised over the fitted values of disp(~...) columns
#'
#' @param fit An object of class \code{\link[brms]{brmsfit}}.
#' @param newdata The grid, holding each column in \code{columns} at a
#' placeholder value.
#' @param columns The placeholder columns to resolve.
#' @param ndraws,draw_ids The posterior draws to predict for, as
#' \code{\link[brms]{posterior_predict}} takes them.
#' @param ... Further arguments to \code{\link[brms]{posterior_predict}}.
#'
#' @details Each posterior draw is paired with one row of the fitted data,
#' sampled with replacement and with equal probability, and that row's values
#' of \code{columns} are put on the whole grid for that draw. A draw therefore
#' predicts one reading at a value of the columns the design holds, and across
#' draws the values occur in the proportions the design holds them. A
#' \code{weights()} term does not enter: the distribution is of the rows of
#' the design.
#'
#' @return A \code{\link[base]{list}} of \code{draws}, the matrix of draws by
#' grid rows, in increasing order of draw, as \pkg{brms} orders its own; and
#' \code{rows}, the row of the fitted data each draw was paired with.
#'
#' @importFrom brms posterior_predict ndraws
#'
#' @noRd
disp_marginal_draws <- function(fit, newdata, columns, ndraws = NULL,
                                draw_ids = NULL, ...) {
  # Draw by draw, from the fitted rows, and not at one fixed value: a fixed
  # value gives the spread of a reading at that value alone, the first level of
  # a factor say, while a predicted reading on a grid bayesnec builds is meant
  # to be one from the design. Sampling a row per draw, rather than averaging
  # the predictive distribution over the rows, keeps the output one reading per
  # draw, the form posterior_predict() returns everywhere else. RF, on #443.
  #
  # The draws are grouped by the value they were paired with and brms is called
  # once per group, with draw_ids and that value written on the grid. Exact,
  # and holds the draws-by-grid output and one group's block of it at a time.
  # Stacking the grid once per distinct value and predicting every draw over
  # all of them would be one call, but would hold draws x grid x values, which
  # for a numeric column with a distinct value on every row is the size of the
  # data times the grid. The number of calls is at most the smaller of the
  # number of draws and the number of distinct values, at about 0.34 s each on
  # a Gamma nec3param fit with 400 draws and a 100-point grid.
  #
  # The rows are sampled from the session's stream and not from a seed stored
  # on the object, which is where they differ from the model-averaging index
  # (#216). That index is fixed so that repeated calls average the same draws;
  # this sampling is part of simulating a new reading, which posterior_predict()
  # draws afresh on every call, so set.seed() before the call reproduces both
  # together, as it does for brms alone.
  if (is.null(draw_ids)) {
    n_total <- ndraws(fit)
    if (is.null(ndraws)) {
      draw_ids <- seq_len(n_total)
    } else {
      if (ndraws < 1 || ndraws > n_total) {
        stop("Argument 'ndraws' should be between 1 and the maximum number",
             " of draws (", n_total, ").", call. = FALSE)
      }
      draw_ids <- sample.int(n_total, ndraws)
    }
  }
  # brms returns the draws of a draw_ids call in increasing order whatever
  # order they were given in, so the groups are only reassembled correctly if
  # each group's ids are increasing. Sorting once here makes them so, and
  # returns the rows in the order brms would have.
  draw_ids <- sort(as.integer(draw_ids))
  obs <- fit$data[, columns, drop = FALSE]
  rows <- sample.int(nrow(obs), length(draw_ids), replace = TRUE)
  # Encode each column as integer value IDs before joining them. Joining the
  # original strings with a separator would merge distinct tuples if a value
  # itself contained that separator. match() also distinguishes doubles that
  # have the same printed value but differ in their stored value.
  key <- do.call(paste, c(lapply(obs, function(col) {
    match(col, unique(col))
  }), sep = ":"))
  value_row <- match(key, key)[rows]
  out <- matrix(NA, nrow = length(draw_ids), ncol = nrow(newdata))
  for (v in sort(unique(value_row))) {
    sel <- which(value_row == v)
    nd <- newdata
    for (col in columns) {
      nd[[col]] <- obs[[col]][v]
    }
    out[sel, ] <- posterior_predict(fit, newdata = nd,
                                    draw_ids = draw_ids[sel], ...)
  }
  list(draws = out, rows = rows)
}
