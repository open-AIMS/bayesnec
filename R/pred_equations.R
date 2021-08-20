#' @noRd
pred_neclin <- function(b_slope, b_nec, b_top, x) {
  b_top - exp(b_slope) * (x - b_nec) * ifelse(x - b_nec < 0, 0, 1)
}

#' @noRd
pred_nec3param <- function(b_beta, b_nec, b_top, x) {
  b_top * exp(-exp(b_beta) * (x - b_nec) *
    ifelse(x - b_nec < 0, 0, 1))
}

#' @noRd
pred_nec4param <- function(b_beta, b_bot, b_nec, b_top, x) {
  b_bot + (b_top - b_bot) * exp(-exp(b_beta) * (x - b_nec) *
    ifelse(x - b_nec < 0, 0, 1))
}

#' @noRd
pred_nechorme <- function(b_top, b_slope, b_beta, b_nec, x) {
  (b_top + exp(b_slope) * x) * exp(-exp(b_beta) * (x - b_nec) *
    ifelse(x - b_nec < 0, 0, 1))
}

#' @noRd
pred_neclinhorme <- function(b_top, b_slope, b_beta, b_nec, x) {
  (b_top + exp(b_slope) * x) - exp(b_beta) * (x - b_nec) *
    ifelse(x - b_nec < 0, 0, 1)
}

#' @noRd
pred_nechorme4 <- function(b_beta, b_bot, b_slope, b_nec, b_top, x) {
  b_bot + ((b_top + exp(b_slope) * x) - b_bot) * exp(-exp(b_beta) *
    (x - b_nec) * ifelse(x - b_nec < 0, 0, 1))
}

#' @noRd
pred_necsigm <- function(b_beta, b_top, b_nec, b_d, x) {
  b_top * exp(-exp(b_beta) * ifelse(x - b_nec < 0, 0, (x - b_nec)^exp(b_d)) *
    ifelse(x - b_nec < 0, 0, 1))
}

#' @noRd
pred_ecxlin <- function(b_top, b_slope, x) {
  b_top - exp(b_slope) * x
}

#' @noRd
pred_ecxexp <- function(b_top, b_beta, x) {
  b_top * exp(-exp(b_beta) * x)
}

#' @noRd
pred_ecxsigm <- function(b_top, b_beta, b_d, x) {
  b_top * exp(-exp(b_beta) * x^exp(b_d))
}

#' @noRd
pred_ecx4param <- function(b_top, b_bot, b_ec50, b_beta, x) {
  b_top + (b_bot - b_top) /
    (1 + exp((b_ec50 - x) * exp(b_beta)))
}

#' @noRd
pred_ecxwb1 <- function(b_bot, b_top, b_beta, b_ec50, x) {
  b_bot + (b_top - b_bot) *
    exp(-exp(exp(b_beta) * (x - b_ec50)))
}

#' @noRd
pred_ecxwb2 <- function(b_bot, b_top, b_beta, b_ec50, x) {
   b_bot + (b_top - b_bot) *
    (1 - exp(-exp(-exp(b_beta) * (x - b_ec50))))
}

#' @noRd
pred_ecxwb1p3 <- function(b_top, b_beta, b_ec50, x) {
  0 + (b_top - 0) *
    exp(-exp(exp(b_beta) * (x - b_ec50)))
}

#' @noRd
pred_ecxwb2p3 <- function(b_top, b_beta, b_ec50, x) {
  0 + (b_top - 0) *
    (1 - exp(-exp(-exp(b_beta) * (x - b_ec50))))
}

#' @noRd
pred_ecxll5 <- function(b_bot, b_top, b_beta, b_ec50, b_f, x) {
  b_bot + (b_top - b_bot) / (1 + exp(exp(b_beta) * (x - b_ec50))) ^ exp(b_f)
}

#' @noRd
pred_ecxll4 <- function(b_bot, b_top, b_beta, b_ec50, x) {
  b_bot + (b_top - b_bot) / (1 + exp(exp(b_beta) * (x - b_ec50)))
}

#' @noRd
pred_ecxll3 <- function(b_top, b_beta, b_ec50, x) {
  0 + (b_top - 0) / (1 + exp(exp(b_beta) * (x - b_ec50)))
}

#' @noRd
pred_ecxhormebc5 <- function(b_bot, b_top, b_beta, b_ec50, b_slope, x) {
  b_bot + (b_top - b_bot + exp(b_slope) * x) /
    (1 + exp(exp(b_beta) * (x - b_ec50)))
}

#' @noRd
pred_ecxhormebc4 <- function(b_top, b_beta, b_ec50, b_slope, x) {
  0 + (b_top - 0 + exp(b_slope) * x) /
    (1 + exp(exp(b_beta) * (x - b_ec50)))
}

#' @noRd
pred_nechormepwr <- function(b_top, b_slope, b_beta, b_nec, x) {
  (b_top + x ^ (1 / (1 + exp(b_slope)))) *
    exp(-exp(b_beta) * (x - b_nec) * ifelse(x - b_nec < 0, 0, 1))
}

#' @noRd
pred_nechorme4pwr <- function(b_beta, b_bot, b_slope, b_nec, b_top, x) {
  b_bot + ((b_top + x ^ (1 / (1 + exp(b_slope)))) - b_bot) *
    exp(-exp(b_beta) * (x - b_nec) * ifelse(x - b_nec < 0, 0, 1))
}

#' @noRd
pred_nechormepwr01 <- function(b_top, b_slope, b_beta, b_nec, x) {
  (1 / (1 + ((1 / b_top) - 1) * exp(-exp(b_slope) * x))) * exp(-exp(b_beta) *
    (x - b_nec) * ifelse(x - b_nec < 0, 0, 1))
}
