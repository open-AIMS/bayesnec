#' bayesnec standard ggplot2 plotting method
#'
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}} standard \pkg{ggplot2}
#' plotting method.
#'
#' @name autoplot
#' @order 1
#'
#' @param object An object of class \code{\link{bayesnecfit}},
#' \code{\link{bayesmanecfit}}, \code{\link{bayesnecgroupfit}} or
#' \code{\link{bayesnecjointfit}}.
#' @param ... Additional arguments to be passed to \code{\link{ggbnec_data}}.
#' @param nec Should NEC values be added to the plot? Defaults to TRUE.
#' @param ecx Should ECx values be added to the plot? Defaults to FALSE..
#' @param xform A function to apply to the returned estimated concentration
#' values.
#' @param group An optional character string naming a categorical variable in
#' the data supplied to \code{\link{bnec}}. Observations are joined through
#' their per-level means when the variable occurs as a group-level term in the
#' fitted formula; a level recorded at one predictor value is marked at its
#' mean instead. A variable not fitted by the model can be shown only with
#' \code{group_aes = "colour"}; the curve and credible band are pooled over
#' that variable, and the legend identifies it as not fitted. A
#' \code{bayesnecgroupfit} uses its fitted grouping variable automatically and
#' draws one panel per level, as does a \code{\link{bayesnecjointfit}}, whose
#' levels are coefficients of one posterior rather than separate fits.
#' @param group_aes How should the grouping selected by \code{group} be shown?
#' \code{"line"} (the default) draws grey per-level means. \code{"colour"}
#' also maps the grouping to the observation fill and the per-level mean marks.
#' The argument is ignored when \code{group} is \code{NULL}. Only
#' \code{"colour"} is available when \code{group} is not a group-level term
#' in the fitted formula.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @examples
#' \dontrun{
#' library(brms)
#' nec4param <- pull_out(manec_example, "nec4param")
#' autoplot(nec4param)
#' autoplot(nec4param, nec = FALSE)
#' autoplot(nec4param, ecx = TRUE, ecx_val = 50)
#'
#' # plot model averaged predictions
#' autoplot(manec_example)
#' # plot all panels together
#' autoplot(manec_example, ecx = TRUE, ecx_val = 50, all_models = TRUE)
#' }
#' \dontrun{
#' # plots multiple models, one at a time, with interactive prompt
#' autoplot(manec_example, ecx = TRUE, ecx_val = 50, all_models = TRUE,
#'          multi_facet = FALSE)
#' }
NULL

#' @rdname autoplot
#' @order 2
#'
#' @method autoplot bayesnecfit
#'
#' @inherit autoplot description return examples
#'
#' @importFrom dplyr mutate
#' @importFrom chk chk_lgl
#' @importFrom rlang .env
#'
#' @export
autoplot.bayesnecfit <- function(object, ..., nec = TRUE, ecx = FALSE,
                                 xform = identity, group = NULL,
                                 group_aes = c("line", "colour")) {
  x <- object
  chk_lgl(nec)
  chk_lgl(ecx)
  group_aes <- match.arg(group_aes)
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  summ <- summary(x, ecx = FALSE) |>
    suppressWarnings() |>
    suppressMessages()
  dat <- ggbnec_data(x, add_nec = nec, add_ecx = ecx,
                     xform = xform, group = group, ...)
  group_label <- plot_group_label(dat, group, group_aes)
  dat |>
    mutate(model = x$model, tag = rownames(.env$summ$nec_vals)) |>
    ggbnec(nec = nec, ecx = ecx, group = !is.null(group),
           group_aes = group_aes, group_label = group_label,
           group_fitted = !identical(attr(dat, "group_fitted"), FALSE))
}

#' @rdname autoplot
#' @order 2
#'
#' @param all_models Should all individual models be plotted separately\
#' (defaults to FALSE) or should model averaged predictions be plotted instead?
#' @param plot Should output \code{\link[ggplot2]{ggplot}} output be plotted?
#' Only relevant if \code{all = TRUE} and \code{multi_facet = FALSE}.
#' @param ask Indicates if the user is prompted before a new page is plotted.
#' Only relevant if \code{plot = TRUE} and \code{multi_facet = FALSE}.
#' @param newpage Indicates if the first set of plots should be plotted to a
#' new page. Only relevant if \code{plot = TRUE} and
#' \code{multi_facet = FALSE}.
#' @param multi_facet Should all plots be plotted in one single panel via
#' facets? Defaults to TRUE.
#'
#' @method autoplot bayesmanecfit
#'
#' @inherit autoplot description return examples
#'
#' @importFrom dplyr mutate left_join
#' @importFrom purrr map_dfr
#' @importFrom tibble rownames_to_column
#' @importFrom grDevices devAskNewPage
#' @importFrom chk chk_lgl
#' @importFrom rlang .env
#'
#' @export
autoplot.bayesmanecfit <- function(object, ..., nec = TRUE, ecx = FALSE,
                                   xform = identity,
                                   all_models = FALSE, plot = TRUE, ask = TRUE,
                                   newpage = TRUE, multi_facet = TRUE,
                                   group = NULL,
                                   group_aes = c("line", "colour")) {
  x <- object
  chk_lgl(nec)
  chk_lgl(ecx)
  group_aes <- match.arg(group_aes)
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  chk_lgl(all_models)
  chk_lgl(plot)
  chk_lgl(ask)
  chk_lgl(newpage)
  chk_lgl(multi_facet)
  if (all_models) {
    all_fits <- lapply(x$success_models, pull_out, manec = x) |>
      suppressMessages() |>
      suppressWarnings()
    if (multi_facet) {
      names(all_fits) <- x$success_models
      nec_labs <- map_dfr(all_fits, function(x) {
        summ <- summary(x, ecx = FALSE) |>
          suppressWarnings() |>
          suppressMessages()
        summ$nec_vals |>
          data.frame() |>
          rownames_to_column(var = "tag")
      }, .id = "model")
      plot_data <- lapply(all_fits, function(fit) {
        ggbnec_data(fit, add_nec = nec, add_ecx = ecx,
                    xform = xform, group = group, ...)
      })
      group_label <- plot_group_label(plot_data[[1]], group, group_aes)
      group_fitted <- !identical(
        attr(plot_data[[1]], "group_fitted"), FALSE
      )
      map_dfr(plot_data, identity, .id = "model") |>
        left_join(y = nec_labs, by = "model") |>
        ggbnec(nec = nec, ecx = ecx, group = !is.null(group),
               group_aes = group_aes, group_label = group_label,
               group_fitted = group_fitted)
    } else {
      if (plot) {
        default_ask <- devAskNewPage()
        on.exit(devAskNewPage(default_ask))
        devAskNewPage(ask = FALSE)
      }
      plots <- vector(mode = "list", length = length(all_fits))
      for (i in seq_along(all_fits)) {
        summ_i <- summary(all_fits[[i]], ecx = FALSE) |>
          suppressWarnings() |>
          suppressMessages()
        dat_i <- ggbnec_data(all_fits[[i]], add_nec = nec, add_ecx = ecx,
                             xform = xform, group = group, ...)
        group_label <- plot_group_label(dat_i, group, group_aes)
        plots[[i]] <- dat_i |>
          mutate(model = x$success_models[i],
                 tag = rownames(.env$summ_i$nec_vals)) |>
          ggbnec(nec = nec, ecx = ecx, group = !is.null(group),
                 group_aes = group_aes, group_label = group_label,
                 group_fitted = !identical(
                   attr(dat_i, "group_fitted"), FALSE
                 ))
        plot(plots[[i]], newpage = newpage || i > 1)
        if (i == 1) {
          devAskNewPage(ask = ask)
        }
      }
      invisible(plots)
    }
  } else {
    summ <- summary(x, ecx = FALSE) |>
      suppressWarnings() |>
      suppressMessages()
    dat <- ggbnec_data(x, add_nec = nec, add_ecx = ecx, xform = xform,
                       group = group, ...)
    group_label <- plot_group_label(dat, group, group_aes)
    dat |>
      mutate(model = "Model averaged predictions",
             tag = rownames(.env$summ$nec_vals)) |>
      ggbnec(nec = nec, ecx = ecx, group = !is.null(group),
             group_aes = group_aes, group_label = group_label,
             group_fitted = !identical(attr(dat, "group_fitted"), FALSE))
  }
}

#' @param brms_fit A \code{\link[brms]{brmsfit}} object.
#' @param bayesnecformula A \code{\link{bayesnecformula}} formula object.
#'
#' @return A \code{\link[base]{data.frame}}.
#'
#' @param group An optional character string naming a categorical variable in
#' the data supplied to \code{\link{bnec}}.
#' @param retained_data A data frame of columns omitted from
#' \code{brms_fit$data}, stored in fitted-row order on the bayesnec object.
#'
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom stats model.frame
#'
#' @noRd
prep_raw_data <- function(brms_fit, bayesnecformula, group = NULL,
                          retained_data = NULL) {
  r_df <- brms_fit$data
  mod_dat <- model.frame(bayesnecformula, data = r_df)
  if (!is.null(group) &&
      (!is.character(group) || length(group) != 1 || is.na(group) ||
       !nzchar(group))) {
    stop("`group` must be NULL or one non-empty column name.", call. = FALSE)
  }
  group_fitted <- NULL
  group_values <- NULL
  if (!is.null(group)) {
    group_vars <- attr(mod_dat, "bnec_group")
    group_vars <- group_vars[!is.na(group_vars)]
    available <- union(names(r_df), names(retained_data))
    if (!group %in% available) {
      stop("`group` must name a group-level variable in the fitted formula ",
           "or a column retained from the data supplied to bnec(). Available ",
           "columns are: ", paste0("\"", available, "\"", collapse = ", "),
           ".", call. = FALSE)
    }
    group_fitted <- group %in% group_vars
    group_values <- if (group %in% names(r_df)) {
      r_df[[group]]
    } else {
      retained_data[[group]]
    }
    if (!group_fitted &&
        !(is.factor(group_values) || is.character(group_values) ||
          is.logical(group_values))) {
      stop("`group` must be categorical when it is not a group-level ",
           "variable in the fitted formula; convert \"", group,
           "\" to a factor before fitting.", call. = FALSE)
    }
    if (anyNA(group_values)) {
      stop("`group` contains missing values in \"", group,
           "\"; remove or define them before fitting.", call. = FALSE)
    }
  }
  y_var <- attr(mod_dat, "bnec_pop")[["y_var"]]
  x_var <- attr(mod_dat, "bnec_pop")[["x_var"]]
  family <- brms_fit$family
  rate_var <- unname(attr(mod_dat, "bnec_pop")["rate_var"])
  if (family$family == "binomial" | family$family == "beta_binomial") {
    trials_var <- attr(mod_dat, "bnec_pop")[["trials_var"]]
    r_df[[y_var]] <- r_df[[y_var]] / r_df[[trials_var]]
  } else if (!is.na(rate_var)) {
    # Rate scale, matching the grid -- see the same branch in plot.R.
    r_df[[y_var]] <- r_df[[y_var]] / r_df[[rate_var]]
  }
  out <- r_df |>
    mutate(x_e = NA, y_e = NA, y_ci = NA, x_r = .data[[x_var]],
           y_r = .data[[y_var]])
  keep <- c("x_e", "y_e", "y_ci", "x_r", "y_r")
  if (!is.null(group)) {
    # A stable output name lets downstream code use the same mapping whichever
    # source column was selected. The source name remains on the result as an
    # attribute for code that labels the grouping.
    out$group <- group_values
    keep <- c(keep, "group")
  }
  out <- out[, keep, drop = FALSE]
  if (!is.null(group)) {
    attr(out, "group_fitted") <- group_fitted
  }
  out
}

#' Label and validate a grouping selected for autoplot
#'
#' A grouping absent from the fitted formula describes observations only. It
#' is labelled as such and is not allowed to draw the fitted-group mean lines,
#' which would otherwise suggest that the model estimated a per-level curve.
#'
#' @noRd
plot_group_label <- function(data, group, group_aes) {
  if (is.null(group)) {
    return(NULL)
  }
  if (identical(attr(data, "group_fitted"), FALSE)) {
    if (!identical(group_aes, "colour")) {
      stop("A `group` that is not a group-level variable in the fitted ",
           "formula can only use `group_aes = \"colour\"`.", call. = FALSE)
    }
    return(paste0(group, " (not fitted)"))
  }
  group
}

#' @param data A \code{\link[base]{data.frame}}.
#' @param nec_vals A \code{\link[base]{numeric}} vector containing the mean,
#' and 95% credible intervals of NEC values, already on the axis scale.
#'
#' @return A \code{\link[base]{data.frame}}.
#'
#' @noRd
bind_nec <- function(data, nec_vals) {
  data$nec_vals <- NA
  data$nec_labs <- NA
  data$nec_labs_l <- NA
  data$nec_labs_u <- NA
  df <- data[1:3, ]
  df[ ] <- NA

  # The transformation is applied by to_axis_scale() before this is called, and
  # the xform argument that stood here was always identity. It is removed
  # rather than left: a decreasing transformation applied at this point would
  # reorder the three entries while the marks read off the record stayed where
  # they were, so a ">=" would end up on what had become a lower bound.
  cens <- attr(nec_vals, "censored_summary")

  df$nec_vals <- nec_vals
  # A censored entry is the end of the prediction range, not a quantile, and
  # the annotation says so. The vertical line is still drawn there, because
  # that is where the estimate is known to be at least: what the prefix stops
  # is reading the number off the axis as though it were the estimate itself.
  df$nec_labs[1] <- censored_label(nec_vals, cens, 1)
  df$nec_labs_l[1] <- censored_label(nec_vals, cens, 2)
  df$nec_labs_u[1] <- censored_label(nec_vals, cens, 3)
  rbind(data, df)
}

#' One annotation label, marked where the entry is a bound
#'
#' @param values A summarised estimate.
#' @param cens Its \code{"censored_summary"} attribute, or \code{NULL}.
#' @param i The entry to label.
#'
#' @return A \code{\link[base]{character}} value.
#' @noRd
censored_label <- function(values, cens, i) {
  lab <- rounded(values[[i]], 2)
  if (is.null(cens) || !nzchar(cens$bound[i])) {
    return(lab)
  }
  paste0(cens$bound[i], lab)
}

#' @param data A \code{\link[base]{data.frame}}.
#' @param ecx_vals A \code{\link[base]{numeric}} vector containing the mean,
#' and 95% credible intervals of ECx values.
#'
#' @return A \code{\link[base]{data.frame}}.
#'
#' @noRd
bind_ecx <- function(data, ecx_vals) {
  data$ecx_vals <- NA
  data$ecx_int <- NA
  data$ecx_labs <- NA
  data$ecx_labs_l <- NA
  data$ecx_labs_u <- NA
  df <- data[1:3, ]
  df[ ] <- NA
  cens <- attr(ecx_vals, "censored_summary")
  df$ecx_vals <- ecx_vals
  df$ecx_int[1] <- attr(ecx_vals, "ecx_val")
  df$ecx_labs[1] <- censored_label(ecx_vals, cens, 1)
  df$ecx_labs_l[1] <- censored_label(ecx_vals, cens, 2)
  df$ecx_labs_u[1] <- censored_label(ecx_vals, cens, 3)
  rbind(data, df)
}

#' Creates the data.frame for plotting with \code{\link{autoplot}}.
#'
#' @param x An object of class \code{\link{bayesnecfit}} or
#' \code{\link{bayesmanecfit}}, as returned by function \code{\link{bnec}},
#' a \code{\link{bayesnecgroupfit}} returned by \code{\link{bnec_group}}, or
#' a \code{\link{bayesnecjointfit}} returned by \code{\link{bnec_joint}}.
#' @param add_nec Should NEC values be added to the plot? Defaults to TRUE.
#' @param add_ecx Should ECx values be added to the plot? Defaults to FALSE.
#' @param xform A function to apply to the returned estimated concentration
#' values.
#' @param group An optional character string naming a categorical variable in
#' the data supplied to \code{\link{bnec}}. When supplied, its values are
#' returned in a column named \code{group}, with the source name in attribute
#' \code{"group_var"}. Attribute \code{"group_fitted"} states whether the
#' fit represents the grouping: either the fitted formula includes it as a
#' group-level variable, or independently fitted \code{bayesnecgroupfit}
#' panels use it. A
#' \code{bayesnecgroupfit} returns its fitted grouping variable automatically.
#' @param ... Additional arguments to be passed to \code{\link{ecx}}. By
#' default, function \code{\link{ecx}} returns EC10.
#'
#' @return A \code{\link[base]{data.frame}}. When \code{group} is supplied, or
#' \code{x} is a \code{bayesnecgroupfit} or a \code{bayesnecjointfit}, the
#' frame includes a \code{group} column and a \code{"group_var"} attribute.
#' Those two classes also include a \code{panel} column containing the level
#' drawn in each panel.
#'
#' @examples
#' \donttest{
#' library(bayesnec)
#' options(mc.cores = 2)
#' data(manec_example)
#'
#' ggbnec_data(manec_example)
#' ggbnec_data(manec_example, add_ecx = TRUE, ecx_val = 50)
#' }
#'
#' @export
ggbnec_data <- function(x, add_nec = TRUE, add_ecx = FALSE,
                        xform = identity, ..., group = NULL) {
  UseMethod("ggbnec_data")
}

#' Creates the data.frame for plotting with \code{\link{autoplot}}.
#'
#' @inheritParams ggbnec_data
#'
#' @param x An object of class \code{\link{bayesnecfit}}, as returned by
#' function \code{\link{bnec}}.
#'
#' @inherit ggbnec_data return examples
#' 
#' @importFrom dplyr mutate
#' @importFrom brms conditional_effects
#' @importFrom rlang .data
#'
#' @noRd
#'
#' @export
ggbnec_data.bayesnecfit <- function(x, add_nec = TRUE, add_ecx = FALSE,
                                    xform = identity, ..., group = NULL) {
  chk_lgl(add_nec)
  chk_lgl(add_ecx)
  if(!inherits(xform, "function")){ 
    stop("xform must be a function.")} 
  brms_fit <- x$fit
  plot_obj <- brms_fit |>
    conditional_effects(method = "posterior_epred") |>
    plot(plot = FALSE)
  e_df <- plot_obj[[1]]$data
  e_df <- data.frame(x_e = c(e_df$effect1__, rev(e_df$effect1__)),
                     y_e = c(e_df$estimate__, rep(NA, nrow(e_df))),
                     y_ci = c(e_df$lower__, rev(e_df$upper__)),
                     x_r = NA, y_r = NA)
  r_df <- prep_raw_data(brms_fit, x$bayesnecformula, group = group,
                        retained_data = x$retained_data)
  if (!is.null(group)) {
    e_df$group <- r_df$group[rep(NA_integer_, nrow(e_df))]
  }
  bdat <- model.frame(x$bayesnecformula, data = x$fit$data, run_par_checks = TRUE)
  out <- rbind(e_df, r_df)
  if (!pop_var_is_transformed(bdat, "x_var")) {
    out <- out |>
      mutate(x_e = xform(.data$x_e), x_r = xform(.data$x_r))
  }
  x_grid_raw <- x$pred_vals$data$x
  if (add_nec) {
    # to_axis_scale() moves the estimates onto the recorded scale and keeps
    # the record's marks, which is all bind_nec() reads: the annotation is
    # built from the moved values, so the bounds inside the record are not
    # consulted and are left on the scale they were computed on.
    out <- bind_nec(out, to_axis_scale(x$ne, bdat, x$bayesnecformula,
                                       x_grid_raw, xform))
  }
  if (add_ecx) {
    ecx_vals <- to_axis_scale(plot_ecx(x, x$fit$family$family, list(...)),
                              bdat, x$bayesnecformula, x_grid_raw, xform)
    out <- bind_ecx(out, ecx_vals)
  }
  if (!is.null(group)) {
    attr(out, "group_var") <- group
    attr(out, "group_fitted") <- attr(r_df, "group_fitted")
  }
  out
}

#' Creates the data.frame for plotting with \code{\link{autoplot}}.
#'
#' @inheritParams ggbnec_data
#'
#' @param x An object of class \code{\link{bayesmanecfit}}, as returned by
#' function \code{\link{bnec}}.
#'
#' @inherit ggbnec_data return examples
#'
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#'
#' @noRd
#'
#' @export
ggbnec_data.bayesmanecfit <- function(x, add_nec = TRUE, add_ecx = FALSE,
                                      xform = identity, ..., group = NULL) {
  chk_lgl(add_nec)
  chk_lgl(add_ecx)
  # Matching the bayesnecfit method. Without it a non-function xform reached
  # mutate() and failed with "could not find function \"xform\"", which names
  # neither the argument nor what it should have been. See #278.
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  e_df <- x$w_pred_vals$data
  e_df <- data.frame(x_e = c(e_df$x, rev(e_df$x)),
                     y_e = c(e_df$Estimate, rep(NA, nrow(e_df))),
                     y_ci = c(e_df$Q2.5, rev(e_df$Q97.5)),
                     x_r = NA, y_r = NA)
  r_df <- prep_raw_data(x$mod_fits[[1]]$fit,
                        x$mod_fits[[1]]$bayesnecformula, group = group,
                        retained_data = x$retained_data)
  if (!is.null(group)) {
    e_df$group <- r_df$group[rep(NA_integer_, nrow(e_df))]
  }
  bdat <- model.frame(x$mod_fits[[1]]$bayesnecformula, 
                      data = x$mod_fits[[1]]$fit$data, 
                      run_par_checks = TRUE)
  manec_formula <- x$mod_fits[[1]]$bayesnecformula
  x_grid_raw <- x$w_pred_vals$data$x
  out <- rbind(e_df, r_df)
  if (!pop_var_is_transformed(bdat, "x_var")) {
    out <- out |>
      mutate(x_e = xform(.data$x_e), x_r = xform(.data$x_r))
  }
  if (add_nec) {
    out <- bind_nec(out, to_axis_scale(x$w_ne, bdat, manec_formula,
                                       x_grid_raw, xform))
  }
  if (add_ecx) {
    ecx_vals <- to_axis_scale(
      plot_ecx(x, x$mod_fits[[1]]$fit$family$family, list(...)),
      bdat, manec_formula, x_grid_raw, xform
    )
    out <- bind_ecx(out, ecx_vals)
  }
  if (!is.null(group)) {
    attr(out, "group_var") <- group
    attr(out, "group_fitted") <- attr(r_df, "group_fitted")
  }
  out
}

#' Creates the data.frame for plotting a grouped set of fits
#'
#' @inheritParams ggbnec_data
#'
#' @param x An object of class \code{\link{bayesnecgroupfit}}, as returned by
#' \code{\link{bnec_group}}.
#'
#' @inherit ggbnec_data return examples
#'
#' @method ggbnec_data bayesnecgroupfit
#'
#' @export
ggbnec_data.bayesnecgroupfit <- function(x, add_nec = TRUE, add_ecx = FALSE,
                                         xform = identity, ..., group = NULL) {
  panel_group <- is.null(group) || identical(group, x$group_var)
  pieces <- lapply(seq_along(x$fits), function(i) {
    out <- ggbnec_data(x$fits[[i]], add_nec = add_nec, add_ecx = add_ecx,
                       xform = xform, ...,
                       group = if (panel_group) NULL else group)
    out$panel <- factor(rep(x$levels[i], nrow(out)), levels = x$levels)
    if (panel_group) {
      out$group <- out$panel
    }
    out
  })
  group_fitted <- if (panel_group) TRUE else attr(pieces[[1]], "group_fitted")
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  attr(out, "group_var") <- if (panel_group) x$group_var else group
  attr(out, "group_fitted") <- group_fitted
  attr(out, "panel_var") <- x$group_var
  out
}

#' @rdname autoplot
#' @order 4
#'
#' @method autoplot bayesnecgroupfit
#'
#' @inherit autoplot description return examples
#'
#' @importFrom chk chk_lgl
#'
#' @export
autoplot.bayesnecgroupfit <- function(object, ..., nec = TRUE, ecx = FALSE,
                                      xform = identity, group = NULL,
                                      group_aes = c("line", "colour")) {
  chk_lgl(nec)
  chk_lgl(ecx)
  group_aes <- match.arg(group_aes)
  if (!inherits(xform, "function")) {
    stop("xform must be a function.")
  }
  dat <- ggbnec_data(object, add_nec = nec, add_ecx = ecx,
                     xform = xform, group = group, ...)
  tags <- vapply(object$fits, function(fit) {
    summ <- summary(fit, ecx = FALSE) |>
      suppressWarnings() |>
      suppressMessages()
    rownames(summ$nec_vals)[1]
  }, character(1))
  names(tags) <- object$levels
  panel_labs <- paste0(object$group_var, " = ", object$levels)
  names(panel_labs) <- object$levels
  level <- as.character(dat$panel)
  dat$model <- factor(unname(panel_labs[level]), levels = unname(panel_labs))
  dat$tag <- unname(tags[level])
  show_group <- !is.null(group) && !identical(group, object$group_var)
  group_label <- if (show_group) {
    plot_group_label(dat, group, group_aes)
  } else {
    NULL
  }
  ggbnec(dat, nec = nec, ecx = ecx, group = show_group,
         group_aes = group_aes, group_label = group_label,
         group_fitted = !identical(attr(dat, "group_fitted"), FALSE))
}

#' ggbnec
#'
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}} standard \pkg{ggplot2}
#' plotting method.
#'
#' @inheritParams autoplot.bayesnecfit
#'
#' @param x A \code{\link[base]{data.frame}} created by function
#' \code{\link{ggbnec_data}}.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' @importFrom ggplot2 ggplot geom_polygon aes geom_line geom_point stat_summary
#' @importFrom ggplot2 geom_vline geom_text theme_classic facet_wrap theme
#' @importFrom ggplot2 element_text element_blank element_rect labs
#' @importFrom ggplot2 scale_colour_discrete scale_fill_discrete
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @noRd
ggbnec <- function(x, nec = TRUE, ecx = FALSE, group = FALSE,
                   group_aes = c("line", "colour"), group_label = NULL,
                   group_fitted = TRUE) {
  group_aes <- match.arg(group_aes)
  out <- ggplot() +
    geom_polygon(data = x |> filter(!is.na(.data$y_ci)),
                 mapping = aes(x = .data$x_e, y = .data$y_ci),
                 fill = "grey75", alpha = 0.5)
  raw <- x |> filter(!is.na(.data$y_r))
  if (group && group_fitted) {
    # Character values omit unused factor levels. Keeping them would add NA to
    # the selected names below and could pass an absent level into a layer.
    n_x <- tapply(raw$x_r, as.character(raw$group), function(value) {
      length(unique(value))
    })
    spanning <- names(n_x)[n_x > 1]
    single_x <- names(n_x)[n_x <= 1]
    if (length(spanning) > 0) {
      spanning_data <- raw[
        as.character(raw$group) %in% spanning, , drop = FALSE
      ]
      if (group_aes == "colour") {
        out <- out + stat_summary(
          data = spanning_data,
          mapping = aes(x = .data$x_r, y = .data$y_r,
                        group = .data$group, colour = .data$group),
          fun = mean, geom = "line", linewidth = 0.3
        )
      } else {
        out <- out + stat_summary(
          data = spanning_data,
          mapping = aes(x = .data$x_r, y = .data$y_r,
                        group = .data$group),
          fun = mean, geom = "line", colour = "grey40", linewidth = 0.3
        )
      }
    }
    if (length(single_x) > 0) {
      single_x_data <- raw[
        as.character(raw$group) %in% single_x, , drop = FALSE
      ]
      if (group_aes == "colour") {
        out <- out + stat_summary(
          data = single_x_data,
          mapping = aes(x = .data$x_r, y = .data$y_r,
                        group = .data$group, fill = .data$group),
          fun = mean, geom = "point", shape = 23, size = 2
        )
      } else {
        out <- out + stat_summary(
          data = single_x_data,
          mapping = aes(x = .data$x_r, y = .data$y_r,
                        group = .data$group),
          fun = mean, geom = "point", shape = 23, fill = "white", size = 2
        )
      }
    }
  }
  out <- out +
    geom_line(data = x |> filter(!is.na(.data$y_e)),
              mapping = aes(x = .data$x_e, y = .data$y_e),
              colour = "black", linetype = 2)
  if (group && group_aes == "colour") {
    group_levels <- if (is.factor(raw$group)) {
      levels(droplevels(raw$group))
    } else {
      sort(unique(as.character(raw$group)), na.last = NA)
    }
    out <- out +
      geom_point(data = raw,
                 mapping = aes(x = .data$x_r, y = .data$y_r,
                               fill = .data$group), shape = 21) +
      scale_colour_discrete(limits = group_levels) +
      scale_fill_discrete(limits = group_levels) +
      labs(colour = group_label, fill = group_label)
  } else {
    out <- out +
      geom_point(data = raw,
                 mapping = aes(x = .data$x_r, y = .data$y_r),
                 fill = "grey30", shape = 21)
  }
  if (nec) {
    ltys <- rep(c(1, 2, 2), length(unique(x$model)))
    lwds <- rep(c(0.5, 0.2, 0.2), length(unique(x$model)))
    out <- out +
      geom_vline(data = x |> filter(!is.na(.data$nec_vals)),
                 mapping = aes(xintercept = .data$nec_vals),
                 linetype = ltys, colour = "grey50",
                 lwd = lwds) +
      geom_text(data = x |> filter(!is.na(.data$nec_labs)),
                mapping = aes(
                  label = paste0(
                    .data$tag, ": ", .data$nec_labs, " (", .data$nec_labs_l,
                    "-", .data$nec_labs_u, ")"
                  )
                ), x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, size = 3,
                colour = "grey50")
  }
  if (ecx) {
    ltys <- rep(c(1, 2, 2), length(unique(x$model)))
    lwds <- rep(c(0.5, 0.2, 0.2), length(unique(x$model)))
    out <- out +
      geom_vline(data = x |> filter(!is.na(.data$ecx_vals)),
                 mapping = aes(xintercept = .data$ecx_vals),
                 linetype = ltys, colour = "dodgerblue4",
                 lwd = lwds) +
      geom_text(data = x |> filter(!is.na(.data$ecx_labs)),
                mapping = aes(label = paste0("EC[", .data$ecx_int, "]", ": ",
                                             .data$ecx_labs, " (",
                                             .data$ecx_labs_l, "-",
                                             .data$ecx_labs_u, ")")),
                x = Inf, y = Inf, hjust = 1.1, vjust = 5.5, size = 3,
                colour = "dodgerblue4")
  }
  out +
    scale_x_continuous(labels = function(x) signif(x, 2)) +
    theme_classic() +
    facet_wrap(~.data$model, scales = "free", ncol = 2) +
    theme(strip.text = element_text(hjust = 0),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = NA, fill = NA)) +
    labs(x = "Predictor",
         y = "Response")
  
}
