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

#' fit_bayesMod
#'
#' Fits a bmrs model using the supplied mod.dat,  bf and priors, using the family specifed as family.type
#'
#' @param mod.dat The model data to use
#' 
#' @param bform The model formula
#'
#' @param  priors The model priors
#'
#' @param family.type the family to use in the model fit 
#' 
#' @param iter the number of interations 
#'
#' @param  chains The number of chains
#'
#' @param cores The number of cores
#'
#' @export
#' @return The fitted bayes model output

fit_bayesMod <- function(mod.dat, bform, priors, family.type, iter, chains, cores){
  
  brmsfit <- brms::brm(bform, data = mod.dat, prior = priors,
                       family = family.type, iter = iter,
                       chains = chains, cores = cores)
  return(brmsfit)
}

