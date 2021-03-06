% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bnec.R
\name{bnec}
\alias{bnec}
\title{bnec}
\usage{
bnec(
  data,
  x_var,
  y_var,
  model = "all",
  trials_var = NA,
  family = NULL,
  priors,
  x_range = NA,
  precision = 1000,
  sig_val = 0.01,
  iter = 10000,
  warmup = floor(iter/10) * 9,
  inits,
  pointwise,
  sample_prior = "yes",
  loo_controls = list(method = "pseudobma"),
  random = NA,
  random_vars = NA,
  ...
)
}
\arguments{
\item{data}{A \code{\link[base]{data.frame}} containing the data to use for
the model.}

\item{x_var}{A \code{\link[base]{character}} indicating the column heading
containing the concentration (x) variable.}

\item{y_var}{A \code{\link[base]{character}} indicating the column heading
containing the response (y) variable.}

\item{model}{A \code{\link[base]{character}} vector indicating the model(s)
to fit. See Details for more information.}

\item{trials_var}{The column heading indicating the column for the number
of "trials" for binomial or beta_binomial2 response data.
If not supplied, the model may run but will not be the model you intended!}

\item{family}{Either a \code{\link[base]{character}} string, a function, or
an object of class \code{\link[stats]{family}} defining the statistical
distribution (family) to use for the y (response) data. See details.}

\item{priors}{An object of class \code{\link[brms]{brmsprior}} which
specifies user-desired prior distributions of model parameters.
If missing, \code{\link{bnec}} will figure out a baseline prior for each
parameter. It can also be specified as a named \code{\link[base]{list}}
where each name needs to correspond to the same string as "model". See
details.}

\item{x_range}{A range of x values over which to consider extracting ECx.}

\item{precision}{The length of the x vector used for posterior predictions,
and over which to extract ECx values. Large values will be slower but more
precise.}

\item{sig_val}{Probability value to use as the lower quantile to test
significance of the predicted posterior values
against the lowest observed concentration (assumed to be the control), to
estimate NEC as an interpolated NOEC value from smooth ECx curves.}

\item{iter}{The number of iterations to be passed to
\code{\link[brms]{brm}}. Defaults to 2e3 to be consistent with brms
defaults.}

\item{warmup}{A positive integer specifying number of warmup (a.k.a.
burnin) iterations. This also specifies the number of iterations used for
stepsize adaptation, so warmup samples should not be used for inference.
The number of warmup should not be larger than "iter" and the default is
"floor(iter / 5) * 4".}

\item{inits}{Optional. Initialisation values. Must be a
\code{\link[base]{list}} of "n" names lists, where "n" corresponds to the
number of chains, and names correspond to the parameter names of a given
model.}

\item{pointwise}{A flag indicating whether to compute the full log-likelihood matrix
at once or separately for each observation. The latter approach is usually considerably slower but requires
much less working memory. Accordingly, if one runs into memory issues, pointwise = TRUE is the way to go,
but will not work for the custom family beta_binomial2}

\item{sample_prior}{Indicate if samples from priors should be drawn additionally to the posterior samples.
Options are "no", "yes" (the default), and "only".
Among others, these samples can be used to calculate Bayes factors for point hypotheses via hypothesis.}

\item{loo_controls}{A named \code{\link[base]{list}} containing the desired
arguments to be passed on to \code{\link[loo]{loo_model_weights}}. It sets
the default wi_method to "pseudobma". See help documentation
?loo_model_weights from package loo.}

\item{random}{= A named \code{\link[base]{list}} containing the random model
formula to apply to model parameters.}

\item{random_vars}{= A \code{\link[base]{character}} vector containing the names of
the columns containing the variables used in the random model formula.}

\item{...}{Further arguments to \code{\link[brms]{brm}} via
\code{\link{fit_bayesnec}}.}
}
\value{
If argument model is a single string, then an object of class
\code{\link{bayesnecfit}}; if many strings or a set,
an object of class \code{\link{bayesmanecfit}}.
}
\description{
Fits a variety of NEC models using Bayesian analysis and provides a model
averaged predictions based on WAIC model weights
}
\details{
As some concentration-response data will use zero concentration
which can cause numerical estimation issues, a small offset is added (1 /
10th of the next lowest value) to zero values of concentration where
\code{x_var} are distributed on a continuous scale from 0 to infinity, or
are bounded to 0, or 1.

The argument \code{family} indicates the family to use for the response
variable in the \code{\link[brms]{brm}} call, and may currently be "Beta" /
Beta / Beta(), "binomial" / binomial / binomial(), "beta_binomial2" /
beta_binomial2, "Gamma" / Gamma / Gamma(), "gaussian" / gaussian /
gaussian(), "negbinomial" / negbinomial / negbinomial(), or "poisson" /
poisson / poisson(). Notice that families Beta and negbinomial are exported
objects of package \pkg{brms}, so the user needs to load \pkg{brms} before
calling these families.

Other families can be added as required, please raise an
\href{https://github.com/open-AIMS/bayesnec/issues}{issue} on the GitHub
development site if your required family is not currently available.

If not supplied, the appropriate distribution will be guessed based on the
characteristics of the input data through \code{\link{check_data}}. Guesses
include all of the above families but "negbinomial" and "betabinomimal2"
because these requires knowledge on whether the data is over-dispersed. As
explained below in the Return section, the user can extract the dispersion
parameter from a bnec call, and if they so wish, can refit the model using
the "negbinomial" family.

The argument \code{model} may be a character string indicating the names of
the desired model. see ?models for more details, and the list of models
available. If a recongised model name is provided a single model of the
specified type is fit, and \code{\link{bnec}} returns a model object of
class \code{\link{bayesnecfit}}.

If a vector of two or more of the available models is supplied,
\code{\link{bnec}} returns a model object of class
\code{\link{bayesmanecfit}} containing model averaged predictions for the
supplied models, providing they were successfully fitted.

Model averaging is achieved through a weighted sample of each fitted models
posterior predictions, with weights derived using the
\code{\link[brms]{loo_model_weights}} from \pkg{brms}. Individual model
fits can be extracted from the \code{mod_fits} element and can be examined
individually.

\code{model} may also be one of "all", meaning all of the available models
will be fit; "ecx" meaning only models excluding a specific NEC step
parameter will be fit; "nec" meaning only models with a specific NEC step
parameter will be fit; or "bot_free" meaning only models without a "bot"
parameter (without a bottom plateau) will be fit. Notice that
if one of these group strings is provided together with a user-specified
named list for the argument \code{priors}, the list names need to contain
the actual model names, and not the group string , e.g. if
\code{model = "ecx"} and \code{priors = my_priors} then
\code{names(my_priors)} must #' contain \code{models("ecx")}. To check
available models and associated parameters for each group,
use the function \code{\link{models}} or to check the parameters of a
specific model use the function \code{\link{show_params}}.

Models are fitted using model formula passed to \pkg{brms}.

All models provide an estimate for NEC. For model types with "nec" as a
prefix, NEC is directly estimated as parameter "nec"
in the model. Models with "ecx" as a prefix are continuous curve models,
typically used for extracting ECx values
from concentration response data. In this instance the NEC value is defined
as the concentration at which there is
a user supplied (see \code{sig_val}) percentage certainty (based on the
Bayesian posterior estimate) that the response
falls below the estimated value of the upper asymptote (top) of the
response (i.e. the response value is significantly
lower than that expected in the case of no exposure).
The default value for \code{sig_val} is 0.01, which corresponds to an alpha
value of 0.01 for a one-sided test of significance.
}
\examples{
\donttest{
library(brms)
library(bayesnec)
data(nec_data)

# A single model
exmp_a <- bnec(data = nec_data, x_var = "x", y_var = "y",
               model = "nec4param", chains = 2)
# Two models model
exmp_b <- bnec(data = nec_data, x_var = "x", y_var = "y",
               model = c("nec4param", "ecx4param"), chains = 2)                
}

}
