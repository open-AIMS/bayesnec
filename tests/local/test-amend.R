library(bayesnec)
library(dplyr)

random_filename <- function(nchar) {
  paste0(c(round(runif(nchar) * 15), sample(letters, nchar),
         sample(LETTERS, nchar))[sample(1:nchar * 3, nchar)], collapse = "")
}

add_na <- function(x, n = 3) {
  x_b <- x
  x_b[sample(seq_along(x), n)] <- NA
  x_b
}

logit <- function(x) {
  log(x / (1 - x))
}

data("nec_data")
manec_example <- nec_data %>%
  dplyr::mutate(y = logit(y)) %>%
  bnec(x_var = "x", y_var = "y", model = c("nec4param", "ecx4param"),
       data = ., iter = 200, warmup = 150, chains = 2)

manec_gauss_id_3 <- amend(manec_example, add = "ecxlin") %>%
  suppressWarnings %>%
  suppressMessages

test_that("models drop and add work correctly", {
  amend(manec_gauss_id_3, drop = "nec4param") %>%
    suppressWarnings %>%
    expect_message("Fitted models are:  ecx4param ecxlin")
  expect_equal(names(manec_gauss_id_3$mod_fits), c("nec4param", "ecx4param", "ecxlin"))
})

test_that("new loo_controls are incorporated", {
  get_new_method <- function(x) {
    attributes(x$mod_stats$wi)$method
  }
  expect_equal(get_new_method(manec_example), "pseudobma")
  my_ctrls <- list(weights = list(method = "stacking"))
  manec_example_stack <- amend(manec_example, loo_controls = my_ctrls) %>%
    suppressWarnings %>%
    expect_message
  expect_equal(get_new_method(manec_example_stack), "stacking")
  my_ctrls <- list(fitting = list(moment_match = TRUE),
                   weights = list(method = "stacking"))
  manec_example_stack2 <- amend(manec_example, loo_controls = my_ctrls) %>%
    suppressWarnings %>%
    expect_message
  expect_equal(get_new_method(manec_example_stack2), "stacking")
  # changing original to moment match alters weights
  my_ctrls <- list(fitting = list(moment_match = TRUE))
  manec_example_stack3 <- amend(manec_example, loo_controls = my_ctrls) %>%
    suppressWarnings %>%
    expect_message
  expect_equal(get_new_method(manec_example_stack3), "pseudobma")
  expect_false(
    all(manec_example$mod_stats$wi == manec_example_stack$mod_stats$wi)
  )
  expect_false(
    all(manec_example$mod_stats$wi == manec_example_stack2$mod_stats$wi)
  )
  expect_false(
    all(manec_example_stack$mod_stats$wi == manec_example_stack2$mod_stats$wi)
  )
  expect_false(
    all(manec_example$mod_stats$wi == manec_example_stack3$mod_stats$wi)
  )
})
