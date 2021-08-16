library(bayesnec)
library(dplyr)

data("nec_data")
other_data <- nec_data
colnames(other_data) <- c("a", "b")
nec_data <- nec_data %>% 
  mutate(count = as.integer(round(y*20)),
         trials = as.integer(20))

test_that("Check for data when using formula syntax", {
  expect_error(bnec(y ~ model(x, "ecxlin")), "You must supply data if using formula syntax.")
  expect_warning(
    expect_error(
      bnec(x = nec_data$x, y = nec_data$count, model = "ecxlin",
           trials_var = "trials"),
      "If data are passed using x, trials_var must contain a numeric vector indicating the number of trials for each observation."
    )
  )
})
