#' Say once per call that an estimate is on a transformed predictor scale
#'
#' Where \code{crf()} transforms the predictor inline, as \code{crf(log(x))},
#' the estimators return values on the transformed scale unless the caller
#' supplies \code{xform}, and nothing in the returned number says so. The
#' message names the transformation and the \code{xform} that inverts it.
#'
#' The first method to reach this function sets the
#' \code{bayesnec.xform_reported} option for the rest of the call, whether or
#' not it raised the message, so the calls it makes per equation, level,
#' component or fit add nothing. Those inner calls often receive no
#' \code{xform} even where the caller gave one, because the outer method
#' applies it afterwards, so they cannot decide. \code{\link{curve_params}}
#' sets the same option.
#'
#' @param object The object the caller named: a \code{\link{bayesnecfit}},
#' \code{\link{bayesmanecfit}}, \code{\link{bayesnechurdlefit}},
#' \code{\link{bayesnecgroupfit}} or \code{\link{bayesnecjointfit}}, or
#' \code{NULL} for nothing to report.
#' @param xform The function the caller supplied.
#' @param kind The \code{\link[base]{character}} name of the function whose
#' result is described. See \code{fitted_scale_text()}.
#'
#' @return The previous value of the option, invisibly, for the caller to
#' restore with \code{on.exit(options(...), add = TRUE)}.
#' @noRd
report_fitted_scale <- function(object, xform, kind) {
  # Test identical(xform, identity), not missing(xform): the group methods
  # receive xform through `...`, where missing() cannot be asked.
  if (!isTRUE(getOption("bayesnec.xform_reported", FALSE)) &&
      identical(xform, identity)) {
    tr <- inline_x_transform(object)
    if (!is.null(tr)) {
      message(fitted_scale_text(scale_subject(object), tr, kind))
    }
  }
  # Set the option last. The caller registers its restore only once this
  # returns, so an error after setting it would leave it set for the session.
  invisible(options(bayesnec.xform_reported = TRUE))
}

#' Evaluate an expression without the scale message
#'
#' For a caller that uses an estimate internally rather than returning it.
#' \code{plot()} and \code{autoplot()} put every estimate they draw onto the
#' axis scale themselves, so the message would describe a number the caller
#' never sees.
#'
#' @param expr An expression.
#'
#' @return The value of \code{expr}.
#' @noRd
without_scale_report <- function(expr) {
  quiet <- options(bayesnec.xform_reported = TRUE)
  on.exit(options(quiet), add = TRUE)
  expr
}

#' The xform a method received through its dots
#'
#' For the methods that take \code{xform} in \code{...} and pass the dots on
#' to a per-fit method. The dots are matched against that method's generic, so
#' that \code{xform} is found by position and by partial name as the method
#' will find it: \code{nec(g, FALSE, exp)} supplies \code{xform} on a group as
#' \code{nec(fit, FALSE, exp)} does on one fit.
#'
#' @param generic The generic the dots are passed to, such as \code{nec}.
#' @param dots The \code{...} of the calling method, as a list.
#'
#' @return A function, or whatever was supplied.
#' @noRd
dots_xform <- function(generic, dots) {
  # A placeholder fills the object slot, so that the dots are matched from the
  # second formal on. An error here is left to the per-fit call, which raises
  # it with the caller's own call in the message.
  matched <- tryCatch(
    match.call(generic, as.call(c(list(quote(f), quote(object)), dots))),
    error = function(e) NULL
  )
  xform <- if (is.null(matched)) NULL else matched[["xform"]]
  if (is.null(xform)) identity else xform
}

#' The first fit in a list whose predictor is transformed inline
#'
#' @param x A \code{\link[base]{list}} of fitted objects, as
#' \code{\link{compare_estimates}} takes.
#'
#' @return One element of \code{x}, or \code{NULL}.
#' @noRd
first_transformed_fit <- function(x) {
  Find(function(fit) !is.null(inline_x_transform(fit)), x)
}

#' The inline transformation of the predictor, where there is one
#'
#' Read from the \code{crf()} term by \code{crf_x_call()}, the parse
#' \code{sub_x_transformation()} applies to every estimate, so the message is
#' raised exactly where an estimate is on a transformed scale.
#'
#' Do not use \code{pop_var_is_transformed()} here, as the plotting paths do.
#' It reads the model frame, and \code{stats::model.frame()} takes the leading
#' minus of \code{crf(-x)} as the removal of a term, so it reports that
#' predictor as untransformed while \code{sub_x_transformation()} negates the
#' estimate.
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
  # Caught, because this only decides whether to print a message. A formula
  # that cannot be parsed fails in whatever the estimator computes next, and is
  # reported there.
  x_call <- tryCatch(crf_x_call(formula), error = function(e) NULL)
  # inherits() rather than is.call(), as in sub_x_transformation(), which
  # returns a parenthesised predictor such as crf((x)) unchanged.
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

#' The noun phrase a message uses for what was fitted
#'
#' The same phrases \code{\link{curve_params}} passes to
#' \code{report_scales()}.
#'
#' @noRd
scale_subject <- function(object) {
  if (inherits(object, "bayesnecgroupfit")) {
    "fitted group"
  } else if (inherits(object, "bayesnecjointfit")) {
    # Before the test on `model` below, which a composed refit sets to NA and
    # a dummy-coded one to the equation fitted at every level. Neither names
    # the object the caller passed.
    "fitted joint refit"
  } else if (inherits(object, "bayesnechurdlefit")) {
    "fitted hurdle pair"
  } else if (inherits(object, "bayesmanecfit")) {
    "fitted model set"
  } else if (is.character(object$model) && length(object$model) == 1) {
    paste(object$model, "fit")
  } else {
    "fit"
  }
}

#' The xform that inverts an inline transformation, as code to print
#'
#' A closed form is given for a one-argument \code{log}, \code{log10},
#' \code{log2}, \code{log1p}, \code{sqrt} or \code{exp} of the predictor,
#' alone or shifted by a numeric constant. Anything else returns \code{NULL},
#' and the message asks for the inverse by name rather than guessing one: a
#' wrong inverse would be followed, and would give a wrong number.
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

#' The text of the scale message
#'
#' @param what A \code{\link[base]{character}} noun phrase naming what was
#' fitted, from \code{scale_subject()}.
#' @param tr The list \code{inline_x_transform()} returns.
#' @param kind The name of the function whose result is described:
#' \code{"nec"}, \code{"ecx"} or \code{"nsec"}, whose estimate is on the
#' predictor axis; \code{"ecnsec"}, whose \code{nsec} argument has the scale,
#' since every \code{ecnsec} method applies \code{xform} to it;
#' \code{"curve_params"}; or \code{"compare_estimates"}, which takes no
#' \code{xform}.
#'
#' @return A \code{\link[base]{character}} string.
#' @noRd
fitted_scale_text <- function(what, tr, kind) {
  inv <- x_inverse_text(tr$call)
  pass <- function(fun, purpose, lead = "Pass") {
    if (is.null(inv)) {
      paste0(lead, " the inverse of ", tr$label, " as xform to ", fun, " ",
             purpose, ".")
    } else {
      paste0(lead, " xform = ", inv, " to ", fun, " ", purpose, ".")
    }
  }
  opening <- paste0("The ", what, " transforms its predictor inline as ",
                    tr$label)
  recorded <- paste("on the scale of", tr$variable)
  switch(
    kind,
    curve_params = paste0(
      opening, ", so nec and ec50 are on that transformed scale, as the ",
      "values nec() and ecx() return are. ",
      pass("curve_params()", paste("to return them", recorded)),
      " top, bot and the shape parameters are not on the predictor axis and ",
      "are unaffected."
    ),
    ecnsec = paste0(
      opening, ". ecnsec() reads nsec ", recorded, ", after applying xform ",
      "to it, and nsec() returns its estimate on the transformed scale unless ",
      "given an xform. Where nsec is on the transformed scale, ",
      pass("ecnsec()", paste("to read it", recorded), lead = "pass")
    ),
    compare_estimates = paste0(
      opening, ", so the estimates compare_estimates() compares, and their ",
      "differences, are on that transformed scale. compare_estimates() takes ",
      "no xform. ",
      pass("nec(), ecx() or nsec()", paste("to return an estimate", recorded))
    ),
    paste0(
      opening, ", so the estimate ", kind, "() returns is on that ",
      "transformed scale. ",
      pass(paste0(kind, "()"), paste("to return it", recorded))
    )
  )
}
