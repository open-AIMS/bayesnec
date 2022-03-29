library(bayesnec)
library(brms)
library(formula.tools)

test_that("checks that gsub statement is replacing all wanted x correctly", {
  # neclin
  rhs_neclin <- bayesnec:::bf_neclin
  neclin_rhs <- deparse1(rhs_neclin[[1]][[3]])
  new_neclin <- substitute_x_in_formula("new_x", neclin_rhs)
  expect_identical(
    new_neclin, "top - exp(slope) * (new_x - nec) * step(new_x - nec)"
  )

  # nec3param
  rhs_nec3param <- bayesnec:::bf_nec3param
  nec3param_rhs <- deparse1(rhs_nec3param[[1]][[3]])
  new_nec3param <- substitute_x_in_formula("new_x", nec3param_rhs)
  expect_identical(
    new_nec3param, "top * exp(-exp(beta) * (new_x - nec) * step(new_x - nec))"
  )

  # nec4param
  rhs_nec4param <- bayesnec:::bf_nec4param
  nec4param_rhs <- deparse1(rhs_nec4param[[1]][[3]])
  new_nec4param <- substitute_x_in_formula("new_x", nec4param_rhs)
  expect_identical(
    new_nec4param,
    "bot + (top - bot) * exp(-exp(beta) * (new_x - nec) * step(new_x - nec))"
  )

  # nechorme
  rhs_nechorme <- bayesnec:::bf_nechorme
  nechorme_rhs <- deparse1(rhs_nechorme[[1]][[3]])
  new_nechorme <- substitute_x_in_formula("new_x", nechorme_rhs)
  expect_identical(
    new_nechorme,
    "(top + exp(slope) * new_x) * exp(-exp(beta) * (new_x - nec) * step(new_x - nec))"
  )

  # nechorme4
  rhs_nechorme4 <- bayesnec:::bf_nechorme4
  nechorme4_rhs <- deparse1(rhs_nechorme4[[1]][[3]])
  new_nechorme4 <- substitute_x_in_formula("new_x", nechorme4_rhs)
  expect_identical(
    new_nechorme4,
    "bot + ((top + exp(slope) * new_x) - bot) * exp(-exp(beta) * (new_x - nec) * step(new_x - nec))"
  )

  # neclinhorme
  rhs_neclinhorme <- bayesnec:::bf_neclinhorme
  neclinhorme_rhs <- deparse1(rhs_neclinhorme[[1]][[3]])
  new_neclinhorme <- substitute_x_in_formula("new_x", neclinhorme_rhs)
  expect_identical(
    new_neclinhorme,
    "(top + exp(slope) * new_x) - exp(beta) * (new_x - nec) * step(new_x - nec)"
  )

  # necsigm
  rhs_necsigm <- bayesnec:::bf_necsigm
  necsigm_rhs <- deparse1(rhs_necsigm[[1]][[3]])
  new_necsigm <- substitute_x_in_formula("new_x", necsigm_rhs)
  expect_identical(
    new_necsigm,
    "top * exp(-exp(beta) * (step(new_x - nec) * (new_x - nec))^exp(d) * step(new_x - nec))"
  )

  # nechormepwr
  rhs_nechormepwr <- bayesnec:::bf_nechormepwr
  nechormepwr_rhs <- deparse1(rhs_nechormepwr[[1]][[3]])
  new_nechormepwr <- substitute_x_in_formula("new_x", nechormepwr_rhs)
  expect_identical(
    new_nechormepwr,
    "(top + new_x^(1/(1 + exp(slope)))) * exp(-exp(beta) * (new_x - nec) * step(new_x - nec))"
  )

  # nechormepwr01
  rhs_nechormepwr01 <- bayesnec:::bf_nechormepwr01
  nechormepwr01_rhs <- deparse1(rhs_nechormepwr01[[1]][[3]])
  new_nechormepwr01 <- substitute_x_in_formula("new_x", nechormepwr01_rhs)
  expect_identical(
    new_nechormepwr01,
    "(1/(1 + ((1/top) - 1) * exp(-exp(slope) * new_x))) * exp(-exp(beta) * (new_x - nec) * step(new_x - nec))"
  )

  # nechorme4pwr
  rhs_nechorme4pwr <- bayesnec:::bf_nechorme4pwr
  nechorme4pwr_rhs <- deparse1(rhs_nechorme4pwr[[1]][[3]])
  new_nechorme4pwr <- substitute_x_in_formula("new_x", nechorme4pwr_rhs)
  expect_identical(
    new_nechorme4pwr,
    "bot + ((top + new_x^(1/(1 + exp(slope)))) - bot) * exp(-exp(beta) * (new_x - nec) * step(new_x - nec))"
  )

  # ecxlin
  rhs_ecxlin <- bayesnec:::bf_ecxlin
  ecxlin_rhs <- deparse1(rhs_ecxlin[[1]][[3]])
  new_ecxlin <- substitute_x_in_formula("new_x", ecxlin_rhs)
  expect_identical(
    new_ecxlin, "top - exp(slope) * new_x"
  )
  
  # ecxexp
  rhs_ecxexp <- bayesnec:::bf_ecxexp
  ecxexp_rhs <- deparse1(rhs_ecxexp[[1]][[3]])
  new_ecxexp <- substitute_x_in_formula("new_x", ecxexp_rhs)
  expect_identical(
    new_ecxexp, "top * exp(-exp(beta) * new_x)"
  )
  
  # ecxsigm
  rhs_ecxsigm <- bayesnec:::bf_ecxsigm
  ecxsigm_rhs <- deparse1(rhs_ecxsigm[[1]][[3]])
  new_ecxsigm <- substitute_x_in_formula("new_x", ecxsigm_rhs)
  expect_identical(
    new_ecxsigm, "top * exp(-exp(beta) * new_x^exp(d))"
  )
  
  # ecx4param
  rhs_ecx4param <- bayesnec:::bf_ecx4param
  ecx4param_rhs <- deparse1(rhs_ecx4param[[1]][[3]])
  new_ecx4param <- substitute_x_in_formula("new_x", ecx4param_rhs)
  expect_identical(
    new_ecx4param, "top + (bot - top)/(1 + exp((ec50 - new_x) * exp(beta)))"
  )
  
  # ecxwb1
  rhs_ecxwb1 <- bayesnec:::bf_ecxwb1
  ecxwb1_rhs <- deparse1(rhs_ecxwb1[[1]][[3]])
  new_ecxwb1 <- substitute_x_in_formula("new_x", ecxwb1_rhs)
  expect_identical(
    new_ecxwb1, "bot + (top - bot) * exp(-exp(exp(beta) * (new_x - ec50)))"
  )

  # ecxwb1p3
  rhs_ecxwb1p3 <- bayesnec:::bf_ecxwb1p3
  ecxwb1p3_rhs <- deparse1(rhs_ecxwb1p3[[1]][[3]])
  new_ecxwb1p3 <- substitute_x_in_formula("new_x", ecxwb1p3_rhs)
  expect_identical(
    new_ecxwb1p3, "0 + (top - 0) * exp(-exp(exp(beta) * (new_x - ec50)))"
  )

  # ecxwb2
  rhs_ecxwb2 <- bayesnec:::bf_ecxwb2
  ecxwb2_rhs <- deparse1(rhs_ecxwb2[[1]][[3]])
  new_ecxwb2 <- substitute_x_in_formula("new_x", ecxwb2_rhs)
  expect_identical(
    new_ecxwb2,
    "bot + (top - bot) * (1 - exp(-exp(-exp(beta) * (new_x - ec50))))"
  )
  
  # ecxwb2p3
  rhs_ecxwb2p3 <- bayesnec:::bf_ecxwb2p3
  ecxwb2p3_rhs <- deparse1(rhs_ecxwb2p3[[1]][[3]])
  new_ecxwb2p3 <- substitute_x_in_formula("new_x", ecxwb2p3_rhs)
  expect_identical(
    new_ecxwb2p3,
    "0 + (top - 0) * (1 - exp(-exp(-exp(beta) * (new_x - ec50))))"
  )
  
  # ecxll5
  rhs_ecxll5 <- bayesnec:::bf_ecxll5
  ecxll5_rhs <- deparse1(rhs_ecxll5[[1]][[3]])
  new_ecxll5 <- substitute_x_in_formula("new_x", ecxll5_rhs)
  expect_identical(
    new_ecxll5,
    "bot + (top - bot)/(1 + exp(exp(beta) * (new_x - ec50)))^exp(f)"
  )
  
  # ecxll4
  rhs_ecxll4 <- bayesnec:::bf_ecxll4
  ecxll4_rhs <- deparse1(rhs_ecxll4[[1]][[3]])
  new_ecxll4 <- substitute_x_in_formula("new_x", ecxll4_rhs)
  expect_identical(
    new_ecxll4,
    "bot + (top - bot)/(1 + exp(exp(beta) * (new_x - ec50)))"
  )
  
  # ecxll3
  rhs_ecxll3 <- bayesnec:::bf_ecxll3
  ecxll3_rhs <- deparse1(rhs_ecxll3[[1]][[3]])
  new_ecxll3 <- substitute_x_in_formula("new_x", ecxll3_rhs)
  expect_identical(
    new_ecxll3, "0 + (top - 0)/(1 + exp(exp(beta) * (new_x - ec50)))"
  )
  
  # ecxhormebc5
  rhs_ecxhormebc5 <- bayesnec:::bf_ecxhormebc5
  ecxhormebc5_rhs <- deparse1(rhs_ecxhormebc5[[1]][[3]])
  new_ecxhormebc5 <- substitute_x_in_formula("new_x", ecxhormebc5_rhs)
  expect_identical(
    new_ecxhormebc5,
    "bot + (top - bot + exp(slope) * new_x)/(1 + exp(exp(beta) * (new_x - ec50)))"
  )
  
  # ecxhormebc4
  rhs_ecxhormebc4 <- bayesnec:::bf_ecxhormebc4
  ecxhormebc4_rhs <- deparse1(rhs_ecxhormebc4[[1]][[3]])
  new_ecxhormebc4 <- substitute_x_in_formula("new_x", ecxhormebc4_rhs)
  expect_identical(
    new_ecxhormebc4,
    "0 + (top - 0 + exp(slope) * new_x)/(1 + exp(exp(beta) * (new_x - ec50)))"
  )  
})
