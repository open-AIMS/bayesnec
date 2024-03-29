#' Model.frame methods in bayesnec.
#'
#' Retrieve data.frame used to fit models via \code{\link{bnec}}, or directly
#' from a \code{\link{bayesnecformula}}.
#' \code{formula} should be of class \code{\link{bayesnecfit}},
#' \code{\link{bayesmanecfit}} or \code{\link{bayesnecformula}}.
#'
#' @name model.frame
#' @order 1
#'
#' @param formula An model object of class \code{\link{bayesnecfit}},
#' \code{\link{bayesmanecfit}}, or a formula of class
#' \code{\link{bayesnecformula}}.
#' @param ... Unused if \code{formula} is a \code{\link{bayesnecfit}} or a
#' \code{\link{bayesmanecfit}}. Else, if \code{formula} is a
#' \code{\link{bayesnecformula}}, additional arguments to be passed to 
#' \code{\link{check_formula}}.
#'
#' @details If \code{formula} is a \code{\link{bayesnecformula}} and it
#' contains transformations to variables x and y, these are evaluated and
#' returned as part of the \code{\link[base]{data.frame}}.
#'
#' @return If \code{formula} is a \code{\link{bayesnecfit}} or a
#' \code{\link{bayesmanecfit}}, a \code{\link[base]{data.frame}} containing
#' the data used to fit the model.
#'
#' If, instead, \code{formula} is a \code{\link{bayesnecformula}},
#' a \code{\link[base]{data.frame}} with additional attributes
#' detailing the population-level variables (attribute \code{"bnec_pop"})
#' (response y, predictor x, and, if binomial a formula, trials) and, if
#' applicable, the group-level variables (attribute \code{"bnec_group"}).
#'
#' @examples
#' \donttest{
#' library(bayesnec)
#' # if input is of class `bayesnecfit` or `bayesmanecfit`
#' model.frame(manec_example, model = "nec4param")
#' nec4param <- pull_out(manec_example, "nec4param")
#' model.frame(nec4param)
#' # if input is of class `bayesnecformula`
#' nec3param <- function(beta, nec, top, x) {
#'   top * exp(-exp(beta) * (x - nec) *
#'     ifelse(x - nec < 0, 0, 1))
#' }
#' 
#' data <- data.frame(x = seq(1, 20, length.out = 10), tr = 100, wght = c(1, 2),
#'                    group_1 = sample(c("a", "b"), 10, replace = TRUE),
#'                    group_2 = sample(c("c", "d"), 10, replace = TRUE))
#' data$y <- nec3param(beta = -0.2, nec = 4, top = 100, data$x)
#' 
#' f_1 <- y ~ crf(x, "nec3param")
#' f_2 <- "y | trials(tr) ~ crf(sqrt(x), \"nec3param\")"
#' f_3 <- y | trials(tr) ~ crf(x, "nec3param") + ogl(group_1) + pgl(group_2)
#' f_4 <- y | trials(tr) ~ crf(x, "nec3param") + (nec + top | group_1)
#' 
#' m_1 <- model.frame(bnf(f_1), data)
#' attr(m_1, "bnec_pop")
#' model.frame(bnf(f_2), data)
#' m_3 <- model.frame(bnf(f_3), data)
#' attr(m_3, "bnec_group")
#' model.frame(bnf(f_4), data)
#' }
NULL

#' @rdname model.frame
#' @order 2
#'
#' @method model.frame bayesnecfit
#'
#' @inherit model.frame description return examples details
#'
#' @importFrom stats model.frame
#'
#' @export
model.frame.bayesnecfit <- function(formula, ...) {
  model.frame(formula$fit)
}

#' @rdname model.frame
#' @order 3
#'
#' @param model A valid model string.
#'
#' @method model.frame bayesmanecfit
#'
#' @inherit model.frame description return examples details
#'
#' @importFrom stats model.frame
#' 
#' @importFrom chk chk_character
#'
#' @export
model.frame.bayesmanecfit <- function(formula, model, ...) {
  chk_character(model)  
  pull_out(formula, model) |>
    suppressMessages() |>
    suppressWarnings() |>
    model.frame()
}
