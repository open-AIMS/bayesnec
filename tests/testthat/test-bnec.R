library(bayesnec)
library(dplyr)

data("nec_data")
other_data <- nec_data
colnames(other_data) <- c("a", "b")
nec_data <- nec_data %>% 
  mutate(count = as.integer(round(y * 20)),
         trials = as.integer(20),
         log.x = log(x))

test_that("Check for data when using formula syntax", {
  expect_error(bnec(y ~ crf(x, "ecxlin")), "argument \"data\" is missing")
})

test_that("Check models inappropriate for negative x are dropped", {
 bnec(y ~ crf(log.x, "nechorme4pwr"), data = nec_data) %>% 
    expect_message("Dropping the model\\(s\\) nechorme4pwr as they are not valid for data with negative predictor \\(x\\) values\\.") %>%
    expect_error("No valid models have been supplied for this data type.")
})
