params <-
list(EVAL = FALSE)

## ----include = FALSE----------------------------------------------------------
stopifnot(require(knitr))
knitr::opts_chunk$set(
  warning = FALSE, message = FALSE,
  eval = ifelse(isTRUE(exists("params")), params$EVAL, FALSE)
)
library(bayesnec)

## ---- warning = FALSE, message = FALSE----------------------------------------
#  library(ggplot2)

## ---- eval=FALSE--------------------------------------------------------------
#  library(brms)
#  library(bayesnec)
#  data(nec_data)
#  
#  # Fit a set of models
#  exmp <- bnec(data = nec_data, x_var = "x", y_var = "y", model = "all")
#  
#  # bayesmanecfit
#  class(exmp)

## ---- eval=FALSE--------------------------------------------------------------
#  exmp_nec <- pull_out(exmp, model = "nec")
#  exmp_ecx <- pull_out(exmp, model = "ecx")

## ---- eval=FALSE--------------------------------------------------------------
#  post_comp <- compare_posterior(list("all" = exmp, "ecx" = exmp_ecx,
#                                      "nec" = exmp_nec),
#                                 comparison = "ecx", ecx_val = 10)
#  names(post_comp)
#  # "posterior_list"
#  # "posterior_data"
#  # "diff_list"
#  # "diff_data"
#  # "prob_diff"

## ---- eval=FALSE--------------------------------------------------------------
#  ggplot(data = post_comp$posterior_data, mapping = aes(x = value)) +
#    geom_density(mapping = aes(group = model, colour = model, fill = model),
#                 alpha = 0.3) +
#    theme_classic()

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example4a.jpeg")

## ---- eval=FALSE--------------------------------------------------------------
#  ggplot(data = post_comp$diff_data, mapping = aes(x = diff)) +
#    geom_density(mapping = aes(group = comparison, colour = comparison,
#                               fill = comparison), alpha = 0.3) +
#    theme_classic()

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example4b.jpeg")

## ---- eval=FALSE--------------------------------------------------------------
#  post_comp$prob_diff
#  #  all.ecx all.nec ecx.nec
#  #1 0.57375    0.46   0.361

## ---- eval=FALSE--------------------------------------------------------------
#  post_comp_fitted <- compare_posterior(list("all" = exmp, "ecx" = exmp_ecx,
#                                             "nec" = exmp_nec),
#                                        comparison = "fitted")

## ---- eval=FALSE--------------------------------------------------------------
#  head(post_comp_fitted$posterior_data)
#  #   model          x  Estimate      Q2.5     Q97.5
#  # 1   all 0.03234801 0.8865920 0.8609242 0.8987086
#  # 2   all 0.09741274 0.8868260 0.8676755 0.8987086
#  # 3   all 0.16247747 0.8870972 0.8712948 0.8987086
#  # 4   all 0.22754220 0.8873789 0.8733017 0.8987346
#  # 5   all 0.29260692 0.8877217 0.8746777 0.8988511
#  # 6   all 0.35767165 0.8880612 0.8760193 0.8988571
#  
#  head(post_comp_fitted$diff_data)
#  #   comparison diff.Estimate   diff.Q2.5 diff.Q97.5          x
#  # 1    all-ecx  -0.005726618 -0.03212779 0.01120134 0.03234801
#  # 2    all-ecx  -0.005424647 -0.02608475 0.01123945 0.09741274
#  # 3    all-ecx  -0.005025740 -0.02334368 0.01109118 0.16247747
#  # 4    all-ecx  -0.004719021 -0.02135375 0.01119652 0.22754220
#  # 5    all-ecx  -0.004280943 -0.02037682 0.01119120 0.29260692
#  # 6    all-ecx  -0.003893997 -0.01951922 0.01128307 0.35767165
#  

## ---- eval=FALSE--------------------------------------------------------------
#  ggplot(data = post_comp_fitted$posterior_data) +
#    geom_line(mapping = aes(x = x, y = Estimate, color = model), size = 0.5) +
#    geom_ribbon(mapping = aes(x = x, ymin = Q2.5, ymax = Q97.5, fill = model),
#                alpha = 0.3)

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example4c.jpeg")

## ----eval = FALSE-------------------------------------------------------------
#  ggplot(data = post_comp_fitted$diff_data) +
#    geom_line(mapping = aes(x = x, y = diff.Estimate, color = comparison),
#              size = 0.5) +
#    geom_ribbon(mapping = aes(x = x, ymin  =diff.Q2.5, ymax = diff.Q97.5,
#                fill = comparison), alpha = 0.3)

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example4d.jpeg")

## ----eval = FALSE-------------------------------------------------------------
#  ggplot(data = post_comp_fitted$prob_diff) +
#    geom_line(mapping = aes(x = x, y = prob, color = comparison), size = 0.5)

## ----echo = FALSE, out.width = "100%"-----------------------------------------
#  knitr::include_graphics("example4e.jpeg")

