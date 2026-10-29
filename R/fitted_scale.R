#' Validate the xform a caller supplied
#'
#' @param xform \code{NULL} or a function.
#'
#' @return \code{NULL}, invisibly, or an error.
#' @noRd
check_xform <- function(xform) {
  if (!is.null(xform) && !inherits(xform, "function")) {
    stop("xform must be a function, or NULL for the recorded scale.",
         call. = FALSE)
  }
  invisible(NULL)
}

#' Whether an estimator returns its estimate on the recorded scale
#'
#' \code{xform = NULL} asks for the recorded scale. That differs from the
#' fitted scale only where \code{crf()} transforms the predictor inline, so on
#' any other fit \code{NULL} gives what \code{identity} gives.
#'
#' @param object A fitted object.
#' @param xform The \code{xform} the caller supplied, or \code{NULL}.
#'
#' @return A \code{\link[base]{logical}} value.
#' @noRd
returns_recorded <- function(object, xform) {
  is.null(xform) && !is.null(inline_x_transform(object))
}

#' The function a parameter-read estimator applies to its draws
#'
#' For \code{\link{nec}}, \code{\link{curve_params}} and the no-effect row of
#' \code{\link{summary}}, whose draws are on the fitted scale. A supplied
#' \code{xform} is returned as it is. \code{NULL} gives the inverse of the
#' \code{crf()} transformation, or \code{identity} where there is none.
#'
#' @param object A fitted object.
#' @param xform The \code{xform} the caller supplied, or \code{NULL}.
#' @param extrapolate The caller's \code{extrapolate}. A numeric limit widens
#' the grid the inverse is read off, so that every draw inside it has a value.
#'
#' @return A \code{\link[base]{function}}.
#' @noRd
recorded_xform <- function(object, xform, extrapolate = FALSE) {
  if (!is.null(xform)) {
    return(xform)
  }
  tr <- inline_x_transform(object)
  if (is.null(tr)) {
    return(identity)
  }
  grid <- recorded_grid(object)
  if (is.numeric(extrapolate)) {
    lims <- range(c(grid, extrapolate[is.finite(extrapolate)]))
    grid <- sort(unique(c(grid, seq(lims[1], lims[2],
                                    length.out = length(grid)))))
  }
  recorded_x_map(scale_source_fit(object)$bayesnecformula, grid, tr)
}

#' The prediction grid a fit's inverse is read off, on the recorded scale
#'
#' The grids stored with the fit, joined to 1000 points over the observed
#' predictor. A fit-time \code{x_range} can store a grid reaching beyond the
#' observations, and an estimate identified there must still be inverted. A
#' hurdle pair joins both components' grids, since the growth component's can
#' differ from the survival component's.
#'
#' @param object A fitted object.
#'
#' @return A \code{\link[base]{numeric}} vector, increasing.
#'
#' @importFrom stats model.frame
#' @noRd
recorded_grid <- function(object) {
  parts <- if (is_bayesnechurdlefit(object)) {
    list(object$growth, object$survival)
  } else {
    list(object)
  }
  stored <- unlist(lapply(parts, function(p) ne_grid_data(p)$x))
  observed <- unlist(lapply(parts, function(p) {
    fit <- scale_source_fit(p)
    bdat <- model.frame(fit$bayesnecformula, fit$fit$data)
    fit$fit$data[[attr(bdat, "bnec_pop")[["x_var"]]]]
  }))
  observed <- observed[is.finite(observed)]
  grid <- c(stored, seq(min(observed), max(observed), length.out = 1000))
  sort(unique(grid[is.finite(grid)]))
}

#' The map from the fitted scale back to the recorded scale
#'
#' The closed-form inverse \code{x_inverse_text()} names, where it reproduces
#' the grid from its own transformation. Otherwise, linear interpolation
#' between the grid's points on the two scales, after
#' \code{refine_inverse_grid()} has added points where the grid is coarse on
#' the fitted scale.
#'
#' @param formula A \code{\link{bayesnecformula}}.
#' @param x_grid_raw The prediction grid, on the recorded scale.
#' @param tr The list \code{inline_x_transform()} returns.
#'
#' @return A \code{\link[base]{function}}. One read off the grid carries
#' attribute \code{"grid_inverse"}, the transformation it inverts, and gives
#' \code{NA} beyond the grid; see \code{map_draws()}.
#'
#' @importFrom stats approx
#' @noRd
recorded_x_map <- function(formula, x_grid_raw, tr) {
  fitted_grid <- suppressWarnings(sub_x_transformation(x_grid_raw, formula))
  keep <- is.finite(fitted_grid) & is.finite(x_grid_raw)
  inv_text <- x_inverse_text(tr$call)
  if (!is.null(inv_text)) {
    inv <- eval(str2lang(inv_text), baseenv())
    # Check the round trip, so that a formula whose environment redefines a
    # function such as log() is not inverted by the base one.
    back <- suppressWarnings(inv(as.numeric(fitted_grid[keep])))
    if (isTRUE(all.equal(back, as.numeric(x_grid_raw[keep]),
                         tolerance = 1e-8))) {
      return(inv)
    }
  }
  if (sum(keep) < 2) {
    stop("The prediction grid holds fewer than two points at which ",
         tr$label, " is finite, so the estimate cannot be returned on the ",
         "scale of ", tr$variable, ". Pass the inverse of ", tr$label,
         " as xform.", call. = FALSE)
  }
  # An infinite fitted endpoint still identifies a finite recorded value.
  keep <- !is.na(fitted_grid) & is.finite(x_grid_raw)
  o <- order(x_grid_raw[keep])
  o <- o[!duplicated(x_grid_raw[keep][o])]
  grid <- refine_inverse_grid(
    as.numeric(x_grid_raw[keep][o]), as.numeric(fitted_grid[keep][o]),
    function(x) suppressWarnings(as.numeric(sub_x_transformation(x, formula)))
  )
  steps <- diff(grid$fitted)
  if (!isTRUE(all(steps > 0) || all(steps < 0))) {
    stop("The transformation ", tr$label, " is not strictly monotone over ",
         "the prediction grid, so it has no inverse to return the estimate ",
         "to the scale of ", tr$variable, ". Pass xform, or xform = identity ",
         "for the fitted scale.", call. = FALSE)
  }
  finite <- is.finite(grid$fitted)
  endpoint_fitted <- grid$fitted[!finite]
  endpoint_raw <- grid$raw[!finite]
  fitted_grid <- grid$fitted[finite]
  raw_grid <- grid$raw[finite]
  out <- function(values) {
    # Build from `values` to keep the attributes the estimators set.
    res <- values
    res[] <- NA_real_
    finite_v <- is.finite(values)
    res[finite_v] <- approx(x = fitted_grid, y = raw_grid,
                            xout = as.numeric(values[finite_v]),
                            rule = 1)$y
    for (i in seq_along(endpoint_fitted)) {
      at_endpoint <- !is.na(values) & values == endpoint_fitted[i]
      res[at_endpoint] <- endpoint_raw[i]
    }
    res
  }
  attr(out, "grid_inverse") <- tr
  out
}

#' Add grid points where the grid is coarse on the fitted scale
#'
#' A grid evenly spaced on the recorded scale is coarse on a scale such as
#' \code{log(x)} near its foot, and interpolation error grows with the square
#' of the step. Each step wider than the median step on the fitted scale is
#' divided evenly on the recorded scale, for at most 20 passes or until the
#' grid reaches 20000 points. An infinite fitted endpoint is retained while
#' its interval is subdivided, so finite draws near it can be interpolated.
#'
#' @param raw The grid on the recorded scale, increasing.
#' @param fitted The same points on the fitted scale.
#' @param transform The function taking \code{raw} to \code{fitted}.
#'
#' @return A \code{\link[base]{list}} with elements \code{raw} and
#' \code{fitted}.
#'
#' @importFrom stats median
#' @noRd
refine_inverse_grid <- function(raw, fitted, transform) {
  span <- abs(diff(fitted))
  target <- median(span[is.finite(span) & span > 0])
  if (!is.finite(target) || target <= 0) {
    return(list(raw = raw, fitted = fitted))
  }
  for (i in seq_len(20)) {
    span <- abs(diff(fitted))
    wide <- which(span > target)
    if (length(wide) == 0 || length(raw) >= 20000) {
      break
    }
    extra <- unlist(lapply(wide, function(k) {
      n <- min(ceiling(span[k] / target), 100)
      seq(raw[k], raw[k + 1], length.out = n + 1)[-c(1, n + 1)]
    }))
    extra <- setdiff(unique(extra), raw)
    if (length(extra) == 0) {
      break
    }
    extra_fitted <- transform(extra)
    ok <- is.finite(extra_fitted)
    raw <- c(raw, extra[ok])
    fitted <- c(fitted, extra_fitted[ok])
    o <- order(raw)
    raw <- raw[o]
    fitted <- fitted[o]
  }
  list(raw = raw, fitted = fitted)
}

#' Apply a map to posterior draws, refusing a draw it cannot place
#'
#' A map read off the grid gives \code{NA} beyond it. A censored draw has no
#' value in any summary, so it may be \code{NA}. Any other draw would be
#' dropped from the quantiles without a word, so the call stops, or with
#' \code{refuse = FALSE} says so and leaves the draw \code{NA}.
#'
#' @param values A \code{\link[base]{numeric}} vector of draws.
#' @param map The function from \code{recorded_xform()}.
#' @param cens The draws' censoring record, or \code{NULL}.
#' @param what A \code{\link[base]{character}} naming the estimate.
#' @param refuse \code{TRUE} to stop on such a draw, \code{FALSE} for a
#' message.
#'
#' @return \code{map(values)}.
#' @noRd
map_draws <- function(values, map, cens = NULL, what = "estimate",
                      refuse = TRUE) {
  out <- map(values)
  tr <- attr(map, "grid_inverse")
  if (is.null(tr)) {
    return(out)
  }
  lost <- is.na(out) & !is.na(values)
  if (!is.null(cens) && length(cens$above) == length(values)) {
    lost <- lost & !cens$above & !cens$below
  }
  if (any(lost)) {
    text <- paste0(
      "No closed-form inverse of ", tr$label, " is known, so the ", what,
      " is returned on the scale of ", tr$variable, " by interpolating on ",
      "the prediction grid, and ", sum(lost), " of ", length(values),
      " draws lie beyond that grid. ",
      if (refuse) "" else "Those draws are NA, and so is their summary. ",
      "Pass the inverse of ", tr$label, " as xform, or xform = identity for ",
      "the fitted scale."
    )
    if (refuse) stop(text, call. = FALSE) else message(text)
  }
  out
}

#' The xform a method received through its dots
#'
#' For the summary methods, which pass their dots to \code{\link{ecx}}. The
#' dots are matched against the generic's formals, so that \code{xform} is
#' found by position and by partial name as the method will find it.
#'
#' @param generic The generic the dots are passed to, such as \code{ecx}.
#' @param dots The \code{...} of the calling method, as a list.
#'
#' @return A function, or whatever was supplied, or \code{NULL} where nothing
#' was.
#' @noRd
dots_xform <- function(generic, dots) {
  # A placeholder fills the object slot, so that the dots are matched from the
  # second formal on. An error here is left to the per-fit call, which raises
  # it with the caller's own call in the message.
  matched <- tryCatch(
    match.call(generic, as.call(c(list(quote(f), quote(object)), dots))),
    error = function(e) NULL
  )
  if (is.null(matched)) NULL else matched[["xform"]]
}

#' Whether a plot draws its estimates as the estimators return them by default
#'
#' Where the predictor is transformed inline, the axis is the recorded scale,
#' and \code{xform} left at \code{NULL} or \code{identity} asks for nothing
#' more. The estimates are then taken from the estimators' default, so the
#' figure and \code{\link{nec}} or \code{\link{ecx}} report the same numbers.
#'
#' @param bdat A model frame carrying the \code{bnec_pop} attribute.
#' @param xform The \code{xform} the caller supplied, or \code{NULL}.
#'
#' @return A \code{\link[base]{logical}} value.
#' @noRd
plot_reads_recorded <- function(bdat, xform) {
  pop_var_is_transformed(bdat, "x_var") &&
    (is.null(xform) || identical(xform, identity))
}

#' The inline transformation of the predictor, where there is one
#'
#' Read from the \code{crf()} term by \code{crf_x_call()}, the parse
#' \code{sub_x_transformation()} applies to every estimate, so an estimate is
#' inverted exactly where it was put on a transformed scale.
#'
#' @param object A fitted object or a \code{prebayesnecfit}. A model set, a
#' group and a hurdle pair are read from their first equation, their first
#' level and their growth component, all of which share the one formula.
#'
#' @return \code{NULL} where the predictor is not transformed inline, otherwise
#' a \code{\link[base]{list}} holding \code{call}, the transformation as a
#' call, \code{label}, the same as text, and \code{variable}, the name of the
#' recorded predictor.
#' @noRd
inline_x_transform <- function(object) {
  # A list element that is not a fit at all is left to the caller's own
  # validation, which runs after this and names the classes it accepts.
  fit <- scale_source_fit(object)
  if (!is.list(fit) || is.null(fit$bayesnecformula)) {
    return(NULL)
  }
  formula <- fit$bayesnecformula
  # Caught: a formula that cannot be parsed fails in whatever the estimator
  # computes next, and is reported there.
  x_call <- tryCatch(crf_x_call(formula), error = function(e) NULL)
  # inherits() rather than is.call(), as sub_x_transformation() tests it, so
  # that the two agree on which predictors are transformed.
  if (!inherits(x_call, "call")) {
    return(NULL)
  }
  # I(x) is the predictor itself: sub_x_transformation() evaluates it to the
  # same numbers with an AsIs class, so no estimate is transformed.
  if (identical(x_call[[1]], as.name("I")) && length(x_call) == 2 &&
      is.name(x_call[[2]])) {
    return(NULL)
  }
  # More than one variable is refused by simplify_formula() before fitting and
  # by sub_x_transformation() after, so there is no fit to describe.
  x_vars <- all.vars(x_call)
  if (length(x_vars) != 1) {
    return(NULL)
  }
  list(call = x_call, label = deparse1(x_call), variable = x_vars)
}

#' @noRd
scale_source_fit <- function(object) {
  if (inherits(object, "bayesnecgroupfit")) {
    return(scale_source_fit(object$fits[[1]]))
  }
  if (inherits(object, "bayesnechurdlefit")) {
    return(scale_source_fit(object$growth))
  }
  representative_fit(object)
}

#' The inverse of an inline transformation, as code
#'
#' A closed form is given for a one-argument \code{log}, \code{log10},
#' \code{log2}, \code{log1p}, \code{sqrt} or \code{exp} of the predictor,
#' alone or shifted by a numeric constant. Anything else returns \code{NULL},
#' and \code{recorded_x_map()} interpolates on the grid instead. Do not add a
#' form whose inverse is not exact: it would be applied without a message.
#'
#' @param x_call The transformation as a call, such as \code{log(x + 1)}.
#'
#' @return A \code{\link[base]{character}} string, or \code{NULL}.
#' @noRd
x_inverse_text <- function(x_call) {
  if (!is.call(x_call) || length(x_call) != 2 || !is.name(x_call[[1]])) {
    return(NULL)
  }
  # I() changes nothing but the class, so I(log(x)) inverts as log(x) does.
  if (identical(x_call[[1]], as.name("I"))) {
    return(x_inverse_text(x_call[[2]]))
  }
  fun <- as.character(x_call[[1]])
  body <- switch(fun, log = "exp(x)", log10 = "10^x", log2 = "2^x",
                 log1p = "expm1(x)", sqrt = "x^2", exp = "log(x)", NULL)
  if (is.null(body)) {
    return(NULL)
  }
  arg <- x_call[[2]]
  if (is.name(arg)) {
    # A function name where base R has the inverse under one, as the
    # documentation and the vignettes write it: xform = exp.
    bare <- switch(fun, log = "exp", log1p = "expm1", exp = "log", NULL)
    return(if (is.null(bare)) paste0("function(x) ", body) else bare)
  }
  shift <- x_literal_shift(arg)
  if (is.null(shift)) {
    return(NULL)
  }
  paste0("function(x) ", body, if (shift < 0) " + " else " - ",
         deparse(abs(shift)))
}

#' The constant added to the predictor inside a transformation
#'
#' @param arg The argument of the transformation, as a call.
#'
#' @return A \code{\link[base]{numeric}} value, negative for a subtraction,
#' or \code{NULL} where \code{arg} is not the predictor plus or minus a numeric
#' literal.
#' @noRd
x_literal_shift <- function(arg) {
  if (!is.call(arg) || length(arg) != 3) {
    return(NULL)
  }
  op <- as.character(arg[[1]])
  lhs <- arg[[2]]
  rhs <- arg[[3]]
  is_constant <- function(z) is.numeric(z) && length(z) == 1 && is.finite(z)
  if (identical(op, "+") && is.name(lhs) && is_constant(rhs)) {
    return(rhs)
  }
  if (identical(op, "+") && is_constant(lhs) && is.name(rhs)) {
    return(lhs)
  }
  if (identical(op, "-") && is.name(lhs) && is_constant(rhs)) {
    return(-rhs)
  }
  NULL
}

