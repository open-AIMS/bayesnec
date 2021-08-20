library(bayesnec)
library(dplyr)

data("nec_data")
other_data <- nec_data
colnames(other_data) <- c("a", "b")
nec_data <- nec_data %>% 
  mutate(count = as.integer(round(y*20)),
         trials = as.integer(20),
         log.x = log(x))

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

test_that("Check models inappropriate for negative x are dropped", {
 bnec(data=nec_data, x_var = "log.x", y_var = "y", model = "nechorme4pwr") %>% 
    expect_message("Dropping the model\\(s\\) nechorme4pwr as they are not valid for data with negative predictor \\(x\\) values\\.")  %>%  
    expect_error("No valid models have been supplied for this data type.") 

})
