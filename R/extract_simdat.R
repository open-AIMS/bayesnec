#    Copyright 2020 Australian Institute of Marine Science
#
#    Licensed under the Apache License, Version 2.0 (the "License");
#    you may not use this file except in compliance with the License.
#    You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.

#'extract_simdat
#'
#' Extracts a range of statistics from a list of bayesnecfit model fits.
#'
#' @param  modfit a bayesnecfit, as returned by fit_bayesnec
#'
#' @export
#' @return A list of model simulation statistics including iter, thin, warmup and chains
extract_simdat <- function(modfit){
  outlist <- list(iter = modfit$fit$fit@sim$iter,
                  thin = modfit$fit$fit@sim$thin,
                  warmup = modfit$fit$fit@sim$warmup, 
                  chains = modfit$fit$fit@sim$chains)
  return(outlist)
}

