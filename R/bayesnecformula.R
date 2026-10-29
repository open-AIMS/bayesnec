#' Set up a model formula for use in
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}}
#'
#' Set up a model formula for use in the 
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}} package, allowing linear 
#' and non-linear (potentially multi-level) concentration-response
#' models to be defined.
#'
#' @aliases bnf
#'
#' @param formula Either a \code{\link[base]{character}} string defining an
#' R formula or an actual \code{\link[stats]{formula}} object. See details.
#' @param ... Unused.
#' @param env An \code{\link[base]{environment}} in which to resolve symbols
#' that \code{formula} names but that are not columns of the data --- a
#' variable holding the model set, or an R function needed to transform the
#' predictor. Only used when \code{formula} is a character string, because a
#' formula object already carries its own environment. Defaults to the calling
#' environment, so a character formula passed down through a wrapper function
#' is bound at the last call rather than where the string was written; where
#' that matters, convert it with \code{bnf(string, env = ...)} and pass the
#' result on.
#'
#' @importFrom stats as.formula
#'
#' @details
#'
#' See \code{methods(class = "bayesnecformula")} for an overview of
#' available methods.
#'
#' \bold{General formula syntax}
#'
#' The \code{formula} argument accepts formulas of the following syntax:
#'   
#' \code{response | aterms ~ crf(x, model) + glterms}
#'
#' \bold{The population-level term: \code{crf}}
#'
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}} uses a special internal
#' term called \code{crf}, which sets the concentration-response equation
#' to be evaluated based on some \code{x} predictor. The equation itself is
#' defined by the argument \code{"model"}: a \code{\link[base]{character}}
#' vector containing a specific model, a concatenation of specific models,
#' a single string defining a particular group of models, or a named list of
#' equation formulas returned by \code{\link{models}}. Internally
#' this argument is substituted by an actual \code{\link[brms]{brmsformula}},
#' which is then passed onto \code{\link[brms]{brm}} for model fitting.
#'
#' The \code{model} argument may be a variable rather than a literal, so that
#' the set can be assembled before the formula is written --- \code{eqs <-
#' c("nec3param", "ecxll3"); bnf(y ~ crf(x, eqs))}. It is resolved in the
#' environment the formula was written in, which for a formula supplied as a
#' character string means the environment given by \code{env}. The same applies
#' on the R side to a function used to transform the predictor inside
#' \code{crf}. Resolving that function in R does not define it in Stan: the
#' function call must also be valid Stan syntax, either because Stan provides
#' it or because the corresponding Stan function is supplied through
#' \code{stanvars}. Otherwise, transform the predictor in the data first.
#' 
#' The predictor is read as a model formula reads it. Parentheses around it
#' and a unary plus are dropped, so \code{crf((log(x)))} is
#' \code{crf(log(x))}. A formula operator at its top level, as in
#' \code{crf(-x)}, \code{crf(x - 1)}, \code{crf(log(x) + 1)} or
#' \code{crf(x^2)}, is refused, because a model formula reads it as formula
#' syntax, not as arithmetic: the model would be fitted on \code{x}, or on
#' \code{log(x)}. The operators are \code{+}, \code{-}, \code{*}, \code{/},
#' \code{:}, \code{^} and \code{\%in\%}. Where arithmetic is meant, write it
#' inside \code{I()}, as in \code{crf(I(x - 1))} or \code{crf(I(-x))}, or
#' compute it into a column of the data. Stan has no \code{I()}, so the
#' formula passed to \pkg{brms} holds the expression inside it, in
#' parentheses, including where \code{I()} is nested inside another call.
#' An operator inside a function call is arithmetic and is
#' accepted, as in \code{crf(log(x + 1))}.
#'
#' \bold{Group-level terms: \code{glterms}}
#' 
#' The user has three options to define group-level effects in a
#' \code{\link{bayesnecformula}}: 1) a general "offset" group-level effect
#' defined by the term \code{ogl} (as in e.g. \code{ogl(group_variable)}). This
#' adds an additional population-level parameter \code{ogl} to the model defined
#' by \code{crf}, analogously to an intercept-only group-level effect
#' in a classic linear model. 2) A group-level effect applied to all
#' parameters in a model at once. This is done by the special term \code{pgl},
#' (as in e.g. \code{pgl(group_variable)}), which comes in handy so the user
#' does not need to know the internal syntax and name of each parameter in the
#' model. 3) A more classic approach where the user can specify which
#' specific parameters --- NB: that requires prior knowledge on the model
#' structure and parameter names --- to vary according to a grouping variable
#' (as in e.g. \code{(bot | group_variable)}). A term naming a parameter an
#' equation does not have is left out of that equation, but each equation must
#' keep at least one group-level term on every grouping variable such a term
#' names; an \code{ogl()} or \code{pgl()} term on the variable counts. An
#' equation that keeps none would be fitted as one curve pooled over the levels
#' of the variable while the rest of the set fits the curve of a typical level,
#' so \code{\link{bnec}} excludes it before fitting, with a message, and records
#' the reason (see \code{\link{bnec_record}}). A call from which every equation
#' is excluded is refused. For example, the parameter \code{bot} exists in
#' model \code{"nec4param"} but not in \code{"nec3param"}, so if the user
#' specifies \code{model = "nec"} in \code{crf}, the term
#' \code{(bot | group_variable)} excludes \code{"nec3param"}, which would keep
#' no term on \code{group_variable}. Under
#' \code{(bot | group_variable) + (nec | group_variable)}, \code{"nec3param"}
#' is fitted with the term on \code{nec} alone, and \code{"nec4param"} with
#' both.
#'
#' The grouping inside \code{ogl()} and \code{pgl()} uses the formula operators
#' accepted on the right of the bar in a \pkg{brms} group-level term. The terms
#' are expanded before they are passed to \pkg{brms}. \code{ogl(a/b)} fits one
#' deviation per level of \code{a} and a further one per level of \code{b}
#' within each level of \code{a}, as \code{(1 | a/b)} does. \code{ogl(a:b)} fits one per
#' combination of the two. \code{ogl(a + b)} is two separate terms,
#' \code{(1 | a) + (1 | b)}, the same as \code{ogl(a) + ogl(b)}.
#' \code{ogl(a*b)} also includes \code{(1 | a:b)}.
#'
#' \bold{The scale a group-level deviation is applied on}
#'
#' \pkg{brms} declares a group-level deviation unconstrained, and under the
#' \code{"identity"} link \code{\link{bnec}} assigns, the quantity it is added
#' to often is not. Where the likelihood restricts the range of the mean ---
#' every family except \code{gaussian} --- the deviation is therefore applied
#' \emph{multiplicatively} rather than added, so that no proposal can put the
#' quantity outside the range in which the model is defined. On a \code{0} to
#' \code{1} response it scales the odds and on a positive response it scales the
#' value itself. This applies to \code{ogl}, where the deviation is on the whole
#' curve, and to a term on \code{top} or \code{bot}, which are the two
#' parameters bounded to the support of the mean. A deviation on \code{nec},
#' \code{ec50}, \code{beta}, \code{slope}, \code{d} or \code{f} is added, as
#' none of those is bounded by the likelihood. A \code{pgl} term places a
#' deviation on every parameter at once and so generates a mixture of the two.
#'
#' The deviation is centred on zero and a deviation of zero leaves the value
#' unchanged, so \code{top}, \code{bot}, \code{nec} and \code{beta} keep their
#' meanings and their own priors. A deviation on \code{top} or \code{bot} adds
#' no population-level term of its own, so the fit has exactly the parameters the
#' additive form had and \code{bot} remains the asymptote the population-level
#' curve declines towards. A prior the user supplies for \code{top} or
#' \code{bot} must bound it to the range the mean is defined on, because the
#' multiplicative form is defined only inside that range; a prior that does not
#' is refused with a message naming the bounds to add. What changes is the name the standard deviation
#' is reported under: a term on \code{bot} is summarised as
#' \code{sd(botgl_Intercept)} rather than \code{sd(bot_Intercept)}, and it is on
#' the log-odds or log scale rather than on the response scale. The generated
#' term names \code{ogl}, \code{bnecmu}, \code{topgl}, \code{botgl},
#' \code{bnectop} and \code{bnecbot} are refused as data column names, because
#' \pkg{brms} would resolve a column of that name in place of the generated term.
#' See \code{vignette("example3")}.
#'
#' \bold{Dispersion sub-models: \code{disp}}
#'
#' By default a fit holds the family's dispersion parameter constant across the
#' whole concentration-response curve. The special term \code{disp} lets that
#' parameter vary, and takes one of two forms.
#'
#' 1) \code{disp(~x)} models dispersion as a function of the \bold{predictor} —
#' an ordinary \pkg{brms} distributional formula, so anything \pkg{brms}
#' understands is allowed, e.g. \code{disp(~log(x))} or \code{disp(~s(x))}. It
#' is descriptive: it says noise is larger at one end of the dose axis.
#'
#' 2) \code{disp("power")} models dispersion as a function of the \bold{mean
#' curve} — a variance function in the GLM sense, and a statement about the
#' measurement process rather than about the dose axis. The functions available,
#' on the log link every eligible family gives its dispersion parameter, with
#' \code{m} a fixed reference value computed from the response before fitting,
#' on the scale of \code{mu} (a count divided by its number of trials or by its
#' \code{rate()} denominator):
#'
#' \tabular{lll}{
#' \bold{name} \tab \bold{form} \tab \bold{families} \cr
#' \code{"power"} \tab \code{log(dpar) = c0 + c1 * (log(mu) - log(m))} \tab
#' all eligible \cr
#' \code{"twosided"} \tab
#' \code{log(phi) = c0 + c1 * (log(mu) - log(m)) + c2 * (log(1 - mu) - log(1 - m))}
#' \tab \code{Beta}, \code{beta_binomial} \cr
#' \code{"loglinear"} \tab \code{log(dpar) = c0 + c1 * (mu - m)} \tab
#' all eligible
#' }
#'
#' Form 2) requires the mean to be modelled on its natural scale; see the
#' identity-link note in \code{\link{bnec}}. Form 1) is an ordinary
#' distributional formula and is valid under any link.
#'
#' Form 1) may name a column other than the predictor, as in \code{disp(~z)},
#' which fits a dispersion parameter for each level of a factor \code{z}, or
#' one that changes with a numeric \code{z}. \pkg{brms} needs \code{z}
#' wherever it predicts, so every prediction grid \pkg{bayesnec} builds holds
#' it. The fitted curve stored on a fit, \code{\link{ecx}},
#' \code{\link{nec}}, \code{\link{nsec}} and \code{\link{ecnsec}} take
#' \code{z} at one fixed value: the first level of a factor, or the median of
#' a numeric column. The mean of every family the term can be fitted for does
#' not involve the dispersion parameter, so that value does not change them.
#' \code{\link{bnec_newdata}} writes the same value and marks the column as a
#' placeholder. \code{\link{posterior_predict}} and \code{\link{predict}}
#' replace a marked column draw by draw with the value of a row sampled from
#' the data the fit was made from, so a predicted reading is one from the
#' design's mix of \code{z} rather than from its first level. A
#' \code{newdata} without \code{z} is refused with a message naming it. The
#' term is refused for a \code{negbinomial} growth component of
#' \code{\link{bnec_hurdle}} and for the \code{hurdle_negbinomial} family,
#' whose curves are the mean of the zero-truncated counts, or that mean
#' multiplied by \code{1 - hu}, and so depend on the shape parameter.
#'
#' Where the formula also has an \code{ogl()} term, \code{mu} in form 2) is the
#' curve before the group-level deviation is applied, not the fitted mean of
#' the group. The deviation therefore changes the mean of each group without
#' changing its dispersion parameter. The observation variance can still
#' change with the fitted mean; for Gamma it is the squared fitted mean divided
#' by the shape. A deviation on a parameter, as in \code{(bot | group)} or
#' \code{pgl(group)}, changes the curve itself, and so is included in the
#' variance function as well. Reading the fitted mean after an \code{ogl()}
#' deviation can produce a second posterior mode at a very low mean when
#' \code{"power"} on a \code{Gamma} response has \code{c1 > 1}. This occurred
#' in the zinc fit reported in #468.
#'
#' A dispersion sub-model requires a family with a free dispersion parameter.
#' For \code{poisson}, \code{bernoulli} and \code{binomial} the variance is a
#' deterministic function of the mean and there is nothing to model;
#' over-dispersion there is remedied by changing family (\code{poisson} to
#' \code{negbinomial}, \code{binomial} to \code{beta_binomial}) or by an
#' observation-level group-level effect. Those are exactly the families the
#' \code{\link{dispersion}} diagnostic applies to, so the two are complements
#' rather than alternatives.
#'
#' Of the two-block families, \code{hurdle_gamma}, \code{zero_inflated_beta}
#' and \code{hurdle_negbinomial} take a dispersion sub-model on their positive
#' block, a \code{Gamma}, \code{Beta} or zero-truncated \code{negbinomial}
#' distribution, and \code{m} is computed from the positive responses; see
#' \code{\link{bnec}}. \code{hurdle_poisson} has no dispersion parameter to
#' model.
#'
#' \bold{What the exponent means depends on the family}, because each family
#' already imposes its own mean-variance link. The form above is written for
#' the dispersion parameter itself, which is what \pkg{brms} fits:
#'
#' Writing \code{r = mu / m} for the response relative to the reference, and
#' taking \code{"power"} for concreteness:
#'
#' \tabular{llll}{
#' \bold{family} \tab \bold{parameter} \tab \bold{implied variance} \tab
#' \bold{constant CV at} \cr
#' \code{gaussian} \tab \code{sigma} \tab \code{(exp(c0) * r^c1)^2} \tab
#' \code{c1 = 1} \cr
#' \code{Gamma} \tab \code{shape} \tab \code{mu^2 / (exp(c0) * r^c1)} \tab
#' \code{c1 = 0} \cr
#' \code{negbinomial} \tab \code{shape} \tab
#' \code{mu + mu^2 / (exp(c0) * r^c1)} \tab \code{--} \cr
#' \code{Beta}, \code{beta_binomial} \tab \code{phi} \tab
#' \code{mu * (1 - mu) / (1 + exp(c0) * r^c1)} \tab \code{--}
#' }
#'
#' In every case \code{c1 = 0} returns the constant-dispersion model, which is
#' where the default prior on \code{c1} is centred.
#'
#' Under \code{"power"} that prior is \code{normal(0, 2)} restricted to a range
#' of \code{c1} that depends on the family. The range is derived from a
#' variance proportional to \code{mu^b} with \code{b} between 0 and 2, which
#' runs from a constant variance to a constant coefficient of variation:
#'
#' \tabular{lll}{
#' \bold{family} \tab \bold{range of c1} \tab \bold{at the two ends} \cr
#' \code{Gamma} \tab 0 to 2 \tab constant CV; constant variance \cr
#' \code{gaussian} \tab 0 to 1 \tab constant variance; constant CV \cr
#' \code{negbinomial} \tab 0 to 1 \tab
#' overdispersion proportional to \code{mu^2}; to \code{mu} \cr
#' \code{Beta}, \code{beta_binomial} \tab unbounded \tab \code{--}
#' }
#'
#' Outside the range for a Gamma or gaussian response, the variance function
#' describes a variance that rises as the mean falls, or a coefficient of
#' variation that falls as the mean falls. For a negative binomial response
#' above 1, the overdispersion term would grow more slowly than the Poisson
#' part of the variance. The beta families are not bounded, because their
#' variance already falls towards both ends of the unit interval through
#' \code{mu * (1 - mu)}, and their precision can plausibly rise in either
#' direction, which is what \code{"twosided"} models. Neither slope of
#' \code{"twosided"} is bounded, nor is the slope of \code{"loglinear"}, which
#' is in units of one over the response. A two-block family takes the range of
#' its positive block's family.
#'
#' The range is a hard bound on the parameter, as the support of the response
#' is for \code{top} and \code{bot}. A posterior for \code{c1} concentrated
#' against one end of it is evidence that the data do not follow the range; a
#' prior on \code{c1} supplied through the \code{prior} argument of
#' \code{\link{bnec}} replaces the default, bounds included. Under
#' \code{prior_type = "regularizing"} the prior is centred on the slope the
#' replicates show rather than on zero; see \code{\link{bnec}}.
#'
#' Both \code{"power"} and \code{"twosided"} take \code{log(mu)}, so a fitted
#' mean that reaches zero is undefined. This rules them out for a response on
#' the real line — a specific growth rate, yield or increment can be negative —
#' and \code{\link{bnec}} refuses the combination rather than failing at
#' initialisation, pointing at \code{"loglinear"} or \code{disp(~x)} instead.
#'
#' Worked guidance — how to decide whether a dispersion sub-model is warranted,
#' why the covariate is centred, and how the forms differ in practice — is given
#' in \code{vignette("example1")}.
#'
#' \bold{Further brms terms (largely untested)}
#'
#' Currently \code{\link{bayesnecformula}} is quite agnostic about additional
#' terms that are valid for a \code{\link[brms]{brmsformula}}. These are
#' \code{aterms} and \code{pterms} (see \code{?\link[brms]{brmsformula}}).
#' The only capability that \code{\link{bayesnecformula}} does not allow is
#' the addition of \code{pterms} outside of the term \code{crf}. Although
#' \code{pterms} can be passed to predictor \code{x} within \code{crf}, we
#' strongly discourage their use because those functionalities have not
#' been tested yet. If this is extremely important to your work, please
#' raise an issue on bayesnec GitHub, and we will consider further testing and 
#' development.
#' Currently, the only four \code{aterms} that have validated behaviour are:
#' 1) \code{trials()}, which is essential in binomially-distributed data, e.g.
#' \code{y | trials(trials_variable)}, 2) weights, e.g.
#' \code{y | weights(weights_variable)}, 3) \code{cens()} (see below), and
#' 4) \code{rate()} (see below), following \pkg{brms} formula syntax.
#' Any other \code{aterm} is an error rather than a warning: an aterm
#' \code{bayesnec} has not validated cannot be assumed to behave sensibly
#' through the prior generation, initial-value search and post-processing that
#' follow.
#'
#' \bold{Rates and exposure}
#'
#' \code{rate(denominator)} declares that the response is a count observed over
#' an exposure --- animals per unit time, larvae per unit area --- and is valid
#' for the \code{poisson} and \code{negbinomial} families only. Any other
#' family is an error when the family is known, the zero-inflated count
#' families included: \pkg{brms} cannot place a rate denominator on those, and
#' under \code{\link{bnec}}'s identity link there is no offset that would do
#' the same job. The check necessarily needs a family to check against, so it
#' fires on every path that fits --- \code{\link{bnec}} always supplies one ---
#' but not on a bare \code{\link{make_brmsformula}(formula, data)} call, which
#' has no family to validate and builds the formula unchecked.
#'
#' Because \code{\link{bnec}} fits on \code{link = "identity"}, which it
#' assigns unless a link argument is written, \pkg{brms}
#' writes the denominator multiplicatively on the response scale rather than as
#' a log offset on the linear predictor, so the mean \emph{is} the rate and
#' "top", "bot" and "nec" stay directly interpretable as counts per unit
#' exposure. The prediction grid holds the denominator at 1, so a fitted curve,
#' an \code{\link{ecx}} and an \code{\link{nsec}} are all read on the rate
#' scale; plots put the observations on that same scale by dividing through.
#' \code{offset} is deliberately not offered as an alternative --- under an
#' identity link it would be additive on the mean, which is not what is wanted.
#'
#' The denominator must be a column of the data. An expression such as
#' \code{rate(hours / 24)} is an error, because \pkg{bayesnec} records the
#' denominator as a plain column and reads that column when it builds the
#' default priors, pins the prediction grid at a denominator of 1, and puts the
#' observations on the rate scale for a plot; an expression is not recorded
#' there, so each of them would divide by \code{hours} while \pkg{brms} divided
#' by the quotient. Compute the column before the call and name it in
#' \code{rate()}.
#'
#' Please note that \pkg{brms} does not implement design weights as in other
#' standard \pkg{base} functions. From their help page, \pkg{brms} "takes the
#' weights literally, which means that an observation with weight 2 receives 2
#' times more weight than an observation with weight 1. It also means that
#' using a weight of 2 is equivalent to adding the corresponding observation
#' twice to the data frame". Other \code{aterms} might be added, though we
#' cannot attest to their functionality within
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}}, i.e. checks will
#' be done outside via \code{\link[brms]{brm}}.
#'
#' \bold{Censored responses: \code{cens}}
#'
#' A response that is rounded at the recording resolution, reported below a
#' limit of detection, or deliberately coarsened at a boundary is censored: the
#' true value is known to lie in an interval rather than at a point. The
#' \code{cens()} \code{aterm} states this, e.g.
#' \code{y | cens(censoring_variable)}, or
#' \code{y | cens(censoring_variable, upper_bound)} for interval censoring.
#' \code{censoring_variable} is a variable in the data taking the values
#' \code{"none"}, \code{"left"}, \code{"right"} or \code{"interval"}
#' (equivalently \code{0}, \code{-1}, \code{1}, \code{2}); it is not a constant,
#' because in a concentration-response dataset only some rows are censored.
#'
#' \bold{The response value carries the bound.} For a left-censored row the
#' number in the response column is the value the truth is known to be at or
#' below --- not zero, and not a substitute for the unknown truth. Because of
#' this, a censored row is exempt from the boundary shifts that
#' \code{\link{bnec}} otherwise applies to zeros in Gamma and Beta responses:
#' the bound has already been declared and moving it would restate it. A row
#' declared censored at a value the family excludes --- left-censored at 0 under
#' Gamma or Beta, right-censored at 1 under Beta --- is an error rather than a
#' shift, because the censored likelihood is degenerate there.
#'
#' **NB:** \code{aterms} other than \code{trials()}, \code{weights()} and
#' \code{cens()} are currently omitted from \code{\link{model.frame}} output. If
#' you need other \code{aterms} as part of that output please raise an issue on
#' our GitHub page.
#'
#' \bold{Validation of formula}
#' Please note that the function only checks for the input nature of the
#' \code{formula} argument and adds a new class. This function **does not**
#' perform any validation on the model nor checks on its adequacy to work with
#' other functions in the package. For that please refer to the function
#' \code{\link{check_formula}} which requires the dataset associated with the
#' formula.
#'
#' @return An object of class \code{\link{bayesnecformula}} and
#' \code{\link[stats]{formula}}.
#'
#' @seealso
#'   \code{\link{check_formula}},
#'   \code{\link{model.frame}},
#'   \code{\link{models}},
#'   \code{\link{show_params}},
#'   \code{\link{make_brmsformula}}
#'
#' @examples
#' library(bayesnec)
#' 
#' bayesnecformula(y ~ crf(x, "nec3param"))
#' # or use shot alias bnf
#' bayesnecformula(y ~ crf(x, "nec3param")) == bnf(y ~ crf(x, "nec3param"))
#' bnf(y | trials(tr) ~ crf(sqrt(x), "nec3param"))
#' bnf(y | trials(tr) ~ crf(x, "nec3param") + ogl(group_1) + pgl(group_2))
#' # dispersion as a function of the predictor
#' bnf(y ~ crf(x, "nec3param") + disp(~x))
#' # dispersion as a function of the fitted mean
#' bnf(y ~ crf(x, "nec3param") + disp("power"))
#' bnf(y | trials(tr) ~ crf(x, "nec3param") + (nec + top | group_1))
#'
#' \donttest{
#' # The predictor expression is passed to Stan by brms. Precompute a
#' # transformation unless its function is also defined for Stan.
#' try(bnf(y | trials(tr) ~ crf(scale(x, scale = TRUE), "nec3param")))
#' }
#' @export
bayesnecformula <- function(formula, ..., env = parent.frame()) {
  if (is.character(formula)) {
    if (!is.environment(env)) {
      # Refuse rather than pass through: as.formula(..., env = NULL) is legal
      # and would resolve symbols in the global environment only. A formula
      # object ignores env, because it already records its environment.
      stop("Argument `env` must be an environment; you supplied ",
           class(env)[1], ".", call. = FALSE)
    }
    # A character formula has no environment of its own. as.formula()'s
    # default would give this function's frame, whose lexical parent is the
    # package namespace, so a symbol the user defined would be found only at
    # the console.
    formula <- as.formula(formula, env = env)
  } else if (!inherits(formula, "formula")) {
    stop("Your formula must be either a valid character or a formula object.")
  }
  allot_class(formula, c("formula", "bayesnecformula"))
}

#' @export
bnf <- function(formula, ..., env = parent.frame()) {
  bayesnecformula(formula = formula, ..., env = env)
}

#' Check if input model formula is appropriate to use with
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}}
#'
#' Perform a series of checks to ensure that the input formula is valid
#' for usage within \code{\link[bayesnec:bayesnec-package]{bayesnec}}.
#'
#' @param formula An object of class \code{\link{bayesnecformula}} as returned
#' by function \code{\link{bayesnecformula}}.
#' @param data A \code{\link[base]{data.frame}} containing the variables
#' specified in \code{formula}.
#' @param run_par_checks See details. A \code{\link[base]{logical}} defining
#' whether random terms for specific parameters should be checked against the
#' underlying concentration-response model defined in \code{formula}.
#' Defaults to \code{FALSE}.
#'
#' @return A validated object of class \code{\link{bayesnecformula}} and
#' \code{\link[stats]{formula}}.
#'
#' @details This function allows the user to make sure that the input formula
#' will allow for a successful model fit with the function \code{\link{bnec}}.
#' Should all checks pass, the function returns the original formula. Otherwise
#' it will fail and requires that the user fixes it until they're able to use
#' it with \code{\link{bnec}}.
#'
#' The argument \code{run_par_checks} is irrelevant for most usages of this
#' package because it only applies if three conditions are met: 1) the user has
#' specified a group-level effect; 2) the group-level effects is parameter 
#' specific (e.g. \code{(par | group_variable)} rather than \code{pgl/ogl(group_variable)}); and 3) The user is keen to learn if the specified parameter
#' is found in the specified model (via argument \code{model} in the \code{crf} term -- see details in ?bayesnecformula).
#'
#' **NB:** \code{aterms} other than \code{trials()}, \code{weights()} and
#' \code{cens()} are currently omitted from \code{\link{model.frame}} output. If
#' you need other \code{aterms} as part of that output please raise an issue on
#' our GitHub page. See details about \code{aterms} in ?bayesnecformula.
#'
#' @seealso
#'   \code{\link{bnec}},
#'   \code{\link{bayesnecformula}}
#'
#' @examples
#' library(bayesnec)
#' nec3param <- function(beta, nec, top, x) {
#'   top * exp(-exp(beta) * (x - nec) *
#'     ifelse(x - nec < 0, 0, 1))
#' }
#' 
#' data <- data.frame(x = seq(1, 20, length.out = 10), tr = 100, wght = c(1, 2),
#'                    group_1 = sample(c("a", "b"), 10, replace = TRUE),
#'                    group_2 = sample(c("c", "d"), 10, replace = TRUE))
#' data$y <- nec3param(beta = -0.2, nec = 4, top = 100, data$x)
#' 
#' # returns error
#' # > f_1 <- y ~ crf(x, "nec3param") + z
#' # regular formula not allowed, wrap it with function bnf
#' # > check_formula(f_1, data)
#' # population-level covariates are not allowed
#' # > check_formula(bnf(f_1), data)
#'
#' \donttest{
#' # expect a series of messages for because not all
#' # nec models have the "bot" parameter
#' f_2 <- y | trials(tr) ~ crf(x, "nec") + (nec + bot | group_1)
#' check_formula(bnf(f_2), data, run_par_checks = TRUE)
#' }
#' # runs fine
#' f_3 <- "y | trials(tr) ~ crf(sqrt(x), \"nec3param\")"
#' check_formula(bnf(f_3), data)
#' f_4 <- y | trials(tr) ~ crf(x, "nec3param") + ogl(group_1) + pgl(group_2)
#' inherits(check_formula(bnf(f_4), data), "bayesnecformula")
#'
#' @export
check_formula <- function(formula, data, run_par_checks = FALSE) {
  UseMethod("check_formula")
}

#' Check if input model formula is appropriate to use with
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}}
#'
#' Perform a series of checks to ensure that the input formula is appropriately
#' set up for usage within \code{\link[bayesnec:bayesnec-package]{bayesnec}}.
#'
#' @inheritParams check_formula
#' @inherit check_formula examples details return
#' 
#' @importFrom formula.tools lhs rhs
#'
#' @export
#' @noRd
check_formula.bayesnecformula <- function(formula, data,
                                          run_par_checks = FALSE) {
  if (!inherits(formula, "bayesnecformula")) {
    stop("Your formula must be of class bayesnecformula.")
  }
  # First, because several checks below, split_calls() and brms read names
  # back out of deparsed text, where a name needing backticks fails as a parse
  # error naming neither the column nor the term.
  check_syntactic_names(formula)
  # Second, because the same text is written into the formula given to
  # model.frame(), where an operator at the top of the predictor is formula
  # syntax.
  check_crf_formula_operator(formula)
  # Split the parsed terms, not the deparsed text: a text split on ") + "
  # breaks a predictor such as crf(sqrt(log(x) + 1)).
  split_rhs_calls <- vapply(rhs_term_calls(rhs(formula)), deparse1,
                            character(1))
  x_str <- grep("crf(", split_rhs_calls, fixed = TRUE, value = TRUE)
  if (length(x_str) == 0) {
    stop("You must specify which non-linear function to use with crf.",
         " See ?bayesnecformula")
  }
  crf_vars <- all.vars(str2lang(eval(parse(text = x_str))))
  if (length(crf_vars) != 1) {
    stop("The `crf` term in your formula can only have one variable; you",
         " specified ", paste0(crf_vars, collapse = "; "), ".")
  } else if (!crf_vars %in% names(data)) {
    stop("Predictor variable \"", crf_vars, "\" not present in data.frame.")
  }
  all_models <- get_model_from_formula(formula)
  model <- all_models[1]
  brms_bf <- get(paste0("bf_", model))
  split_random_call <- setdiff(split_rhs_calls, x_str)
  if (any(grepl("pgl(", split_random_call, fixed = TRUE))) {
    str_calls <- grep("pgl(", split_random_call, fixed = TRUE, value = TRUE)
    vars <- all.vars(str2lang(paste0(str_calls, collapse = " + ")))
    for (i in seq_along(brms_bf[[2]])) {
      for (j in seq_along(vars)) {
        if (!vars[j] %in% names(data)) {
          stop("Group-level variable(s) ",
               paste0("\"", vars[j], "\"", collapse = "; "),
               " not found in dataset.")
        } else if (any(sapply(data[, vars[j]], is.numeric))) {
          stop("Group-level variables cannot be numeric.")
        }
      }
    }
    split_random_call <- setdiff(split_random_call, str_calls)
  }
  if (any(grepl("|", split_random_call, fixed = TRUE))) {
    str_calls <- grep("|", split_random_call, fixed = TRUE, value = TRUE)
    split_str_calls <- lapply(str_calls, clean_bar_glef)
    tmp_list <- list()
    for (i in seq_along(split_str_calls)) {
      if (any(grepl(" \\+ ", split_str_calls[[i]][2]))) {
        stop("Right-hand side of the \"|\" symbol in the group-level portion",
             " of your formula can only have one grouping variable. Issue",
             " found on: ", str_calls[i])
      }
      if (any(grepl(" \\+ ", split_str_calls[[i]][1]))) {
        extended_pars <- strsplit(split_str_calls[[i]][1], " \\+ ")[[1]]
        tmp_list_i <- vector(mode = "list", length = length(extended_pars))
        for (j in seq_along(tmp_list_i)) {
          tmp_list_i[[j]] <- c(extended_pars[j], split_str_calls[[i]][2])
        }
        tmp_list <- c(tmp_list, tmp_list_i)
      } else {
        tmp_list <- c(tmp_list, split_str_calls[i])
      }
    }
    split_str_calls <- tmp_list
    pars <- sapply(split_str_calls, `[[`, 1)
    vars <- sapply(split_str_calls, `[[`, 2)
    # Check the grouping of every term, not only the first.
    vars <- unique(trimws(unlist(strsplit(vars, "\\/|\\:"))))
    for (j in seq_along(vars)) {
      if (!vars[j] %in% names(data)) {
        stop("Group-level variable(s) ",
             paste0("\"", vars[j], "\"", collapse = "; "),
             " not found in dataset.")
      } else if (any(sapply(data[, vars[j]], is.numeric))) {
        stop("Group-level variables cannot be numeric.")
      }
    }
    if (run_par_checks) {
      message("Performing single parameter checks on all models...")
      for (h in seq_along(all_models)) {
        tmp_brms_bf <- get(paste0("bf_", all_models[h]))
        if (!all(pars %in% names(tmp_brms_bf[[2]]))) {
          to_flag <- pars[!pars %in% names(tmp_brms_bf[[2]])]
          message("The parameter(s) ",
                  paste0("\"", to_flag, "\"", collapse = "; "),
                  " not valid parameters in ", all_models[h], ". If this",
                  " was a mistake, check ?models and ?show_params, otherwise",
                  " ignore.")
        }
      }
    }
    split_random_call <- setdiff(split_random_call, str_calls)
  }
  if (any(grepl("ogl(", split_random_call, fixed = TRUE))) {
    str_calls <- grep("ogl(", split_random_call, fixed = TRUE, value = TRUE)
    vars <- all.vars(str2lang(paste0(str_calls, collapse = " + ")))
    for (i in seq_along(brms_bf[[2]])) {
      for (j in seq_along(vars)) {
        if (!vars[j] %in% names(data)) {
          stop("Group-level variable(s) ",
               paste0("\"", vars[j], "\"", collapse = "; "),
               " not found in dataset.")
        } else if (any(sapply(data[, vars[j]], is.numeric))) {
          stop("Group-level variables cannot be numeric.")
        }
      }
    }
    split_random_call <- setdiff(split_random_call, str_calls)
  }
  if (any(grepl("disp(", split_random_call, fixed = TRUE))) {
    str_calls <- grep("disp(", split_random_call, fixed = TRUE, value = TRUE)
    # Structural checks only. Whether the term is valid for the family, and
    # whether the response admits a log(mu) form, cannot be settled here
    # because check_formula() is not given the family -- those run in
    # wrangle_model_formula() via check_disp_spec().
    disp_spec <- parse_disp_term(formula)
    if (disp_spec$route == "A") {
      vars <- all.vars(str2lang(disp_spec$value))
      for (j in seq_along(vars)) {
        if (!vars[j] %in% names(data)) {
          stop("Dispersion sub-model variable(s) ",
               paste0("\"", vars[j], "\"", collapse = "; "),
               " not found in dataset.", call. = FALSE)
        }
      }
    } else if (!is.character(disp_spec$value) ||
                 !disp_spec$value %in% names(disp_functions)) {
      stop("The disp() term must be given either a one-sided formula, e.g.",
           " disp(~x), or the name of a variance function, one of: ",
           paste0("\"", names(disp_functions), "\"", collapse = ", "),
           ". See ?bayesnecformula", call. = FALSE)
    }
    split_random_call <- setdiff(split_random_call, str_calls)
  }
  if (length(split_random_call) > 0) {
    stop("Term(s) ", paste0(split_random_call, collapse = "; "), "; are not",
         " allowed in a bayesnec formula. See ?bayesnecformula")
  }
  lhs_vars <- all.vars(lhs(formula))
  exist_all_left <- all(lhs_vars %in% names(data))
  if (!exist_all_left) {
    to_flag <- paste0(setdiff(lhs_vars, names(data)), collapse = "; ")
    stop("Variable(s) ", to_flag, " not present in data.frame")
  }
  lhs_calls <- gsub("\\) \\+ ", ") impossiblestr ", deparse1(lhs(formula)))
  split_lhs_calls <- strsplit(lhs_calls, " \\| | impossiblestr ")[[1]]
  no_resp <- split_lhs_calls[-1]
  if (length(no_resp) > 0) {
    # Refuse any aterm bayesnec has not validated. Passed through, it can fit
    # and then fail in post-processing with a brms error about a missing
    # variable.
    validated <- "trials\\(|weights\\(|cens\\(|rate\\("
    if (isTRUE(attr(formula, "bayesnec_internal_truncation"))) {
      validated <- paste0(validated, "|trunc\\(")
    }
    unvalidated <- no_resp[!grepl(validated, no_resp)]
    if (length(unvalidated) > 0) {
      stop("You have specified brms special aterms bayesnec does not support: ",
           paste0(unvalidated, collapse = ", "),
           ". The supported aterms are trials, weights, cens and rate.",
           " See ?bayesnecformula.", call. = FALSE)
    }
    cens_calls <- grep("cens\\(", no_resp, value = TRUE)
    if (length(cens_calls) > 0 &&
          length(all.vars(str2lang(cens_calls[1]))) == 0) {
      # e.g. cens("left"), which brms recycles over every row. That is legal but
      # is almost never what is meant in a concentration-response dataset, where
      # only some rows are censored, so flag it rather than silently declaring
      # the whole response censored.
      warning("The cens() term in your formula, ", cens_calls[1], ", contains",
              " no variable, so every row of your response will be treated as",
              " censored. Censoring is normally indicated by a variable in",
              " your data taking the values \"none\", \"left\", \"right\" or",
              " \"interval\". See ?bayesnecformula.", call. = FALSE)
    }
  }
  formula
}

#' The terms of a formula's right-hand side
#'
#' @param rhs_call The right-hand side of a formula, as a call.
#'
#' @return A \code{\link[base]{list}} of calls and symbols, one for each term
#' joined by a top-level \code{+}, in the order written. A term in
#' parentheses, such as \code{(nec | group)}, is kept whole.
#'
#' @noRd
rhs_term_calls <- function(rhs_call) {
  rhs_terms <- list()
  while (is.call(rhs_call) && length(rhs_call) == 3 &&
           identical(rhs_call[[1]], quote(`+`))) {
    rhs_terms <- c(list(rhs_call[[3]]), rhs_terms)
    rhs_call <- rhs_call[[2]]
  }
  c(list(rhs_call), rhs_terms)
}

#' Refuse a formula variable whose name is not a syntactic R name
#'
#' @param formula An object of class \code{\link{bayesnecformula}}.
#'
#' @details A column read from a spreadsheet or built with
#' \code{data.frame(check.names = FALSE)} can have a name such as
#' \code{odd name}, written in a formula as \code{`odd name`}.
#' \code{\link{check_formula}}, \code{split_calls()} and \pkg{brms} each
#' rebuild part of the model by parsing deparsed formula text, where such a
#' name is deparsed without its backticks, so the call ends in a parse error
#' naming neither the column nor the term. \pkg{brms} itself fails on such a
#' name, so it cannot be made to work, and it is refused once, before anything
#' else reads the formula.
#'
#' @return \code{NULL}, invisibly. Called for its error.
#'
#' @importFrom formula.tools lhs rhs
#'
#' @noRd
check_syntactic_names <- function(formula) {
  lhs_parts <- hurdle_lhs_parts(lhs(formula))
  rhs_terms <- rhs_term_calls(rhs(formula))
  term_vars <- function(term) {
    # Check only the predictor of crf(), not its model argument: a model set
    # held in a variable is evaluated by get_model_from_formula() and never
    # reaches brms, so crf(x, `my models`) fits.
    # Use match.call() rather than term[[2]], so that crf(model = m, x = conc)
    # is read correctly; a call crf() cannot match is refused later by
    # check_formula().
    if (is.call(term) && identical(term[[1]], quote(crf))) {
      x_arg <- tryCatch(match.call(crf, term)$x, error = function(e) NULL)
      if (!is.null(x_arg)) {
        return(all.vars(x_arg))
      }
    }
    # Likewise disp() given anything but a one-sided formula: parse_disp_term()
    # evaluates it as the name of a variance function, so it names no column.
    # disp(~ ...) names columns and is checked, by parse_disp_term()'s own test.
    if (is.call(term) && identical(term[[1]], quote(disp))) {
      disp_arg <- if (length(term) > 1) term[[2]] else NULL
      if (!(is.call(disp_arg) && identical(disp_arg[[1]], quote(`~`)))) {
        return(character(0))
      }
    }
    all.vars(term)
  }
  formula_terms <- c(list(lhs_parts$response), lhs_parts$aterms, rhs_terms)
  term_labels <- c("the response", vapply(c(lhs_parts$aterms, rhs_terms),
                                          deparse1, character(1)))
  vars <- lapply(formula_terms, term_vars)
  all_vars <- unique(unlist(vars))
  # make.names() is R's own definition of a syntactic name, so it catches a
  # reserved word such as `if` as well as a space or a leading digit, and it
  # also gives the replacement the message suggests. The suggestion is made
  # with unique = TRUE, because "a b" and "a-b" would otherwise both be told to
  # become "a.b".
  bad <- all_vars[all_vars != make.names(all_vars)]
  if (length(bad) == 0) {
    return(invisible(NULL))
  }
  where <- vapply(bad, function(v) {
    in_term <- vapply(vars, function(tv) v %in% tv, logical(1))
    paste0("\"", v, "\" in ", paste0(term_labels[in_term],
                                        collapse = " and "))
  }, character(1))
  stop("The formula names variable(s) that are not syntactic R names: ",
       paste0(where, collapse = "; "), ". bayesnec and brms both rebuild the",
       " model from the text of the formula, where a name that has to be",
       " written in backticks cannot be parsed. Rename the column(s) in the",
       " data and in the formula, for example with make.names(), which gives ",
       paste0("\"", make.names(bad, unique = TRUE), "\"", collapse = ", "),
       ".",
       call. = FALSE)
}

#' Refuse a crf() predictor with a formula operator at its top level
#'
#' @param formula An object of class \code{\link{bayesnecformula}}.
#'
#' @details Every path reads the predictor as a model formula does, where an
#' operator at its top level is formula syntax, not arithmetic: \code{x - 1}
#' and \code{x^2} are read as \code{x}, and the minus in \code{-x} removes the
#' term. The model would then be fitted on an expression the user did not
#' write. Under \code{I()}, or inside a function call, the operator is
#' arithmetic, so the refusal offers the \code{I()} form.
#'
#' Enclosing parentheses and unary pluses are removed before the test, because
#' the formula removes them as well. A binary operator in a predictor naming
#' more than one variable is left to \code{check_formula()}, which refuses
#' the predictor for naming them.
#'
#' Called beside \code{check_syntactic_names()}, wherever that is called, so
#' that it is raised once per call, before anything is fitted.
#'
#' @return \code{NULL}, invisibly. Called for its error.
#'
#' @importFrom formula.tools rhs
#'
#' @noRd
check_crf_formula_operator <- function(formula) {
  rhs_call <- rhs(formula)
  # Parentheses around a whole crf() term also disappear when the model frame
  # reads the formula. Inspect inside them so (crf(-x)) cannot bypass the check.
  collect_rhs_terms <- function(expr) {
    if (is.call(expr) && identical(expr[[1]], quote(`(`))) {
      return(collect_rhs_terms(expr[[2]]))
    }
    if (is.call(expr) && length(expr) == 3 &&
        identical(expr[[1]], quote(`+`))) {
      return(c(collect_rhs_terms(expr[[2]]), collect_rhs_terms(expr[[3]])))
    }
    list(expr)
  }
  rhs_terms <- collect_rhs_terms(rhs_call)
  is_crf <- vapply(rhs_terms, function(term) {
    is.call(term) && identical(term[[1]], quote(crf))
  }, logical(1))
  if (!any(is_crf)) {
    return(invisible(NULL))
  }
  term <- rhs_terms[[which(is_crf)[1]]]
  # match.call() rather than term[[2]], so that crf(model = m, x = -x) is read
  # as check_syntactic_names() reads it. A term crf() cannot match is refused
  # for what it is by check_formula().
  x_arg <- tryCatch(match.call(crf, term)$x, error = function(e) NULL)
  unparen <- function(expr) {
    while (is.call(expr) && identical(expr[[1]], quote(`(`))) {
      expr <- expr[[2]]
    }
    expr
  }
  top <- x_arg
  plus_wrapped <- FALSE
  repeat {
    top <- unparen(top)
    if (!(is.call(top) && length(top) == 2 &&
          identical(top[[1]], quote(`+`)))) {
      break
    }
    plus_wrapped <- TRUE
    top <- top[[2]]
  }
  if (!is.call(top) || !is.name(top[[1]])) {
    return(invisible(NULL))
  }
  negated <- identical(top[[1]], quote(`-`)) && length(top) == 2
  # The operators stats::terms() reads as formula syntax.
  formula_ops <- c("+", "-", "*", "/", ":", "^", "%in%")
  operator <- length(top) == 3 && as.character(top[[1]]) %in% formula_ops &&
    length(all.vars(top)) == 1
  if (!negated && !operator) {
    return(invisible(NULL))
  }
  # The remedy is the user's own term with the predictor wrapped in I(), in the
  # slot the predictor was written in, so that it can be copied as it stands.
  args <- as.list(term)[-1]
  arg_names <- names(args)
  if (is.null(arg_names)) {
    arg_names <- rep("", length(args))
  }
  slot <- if ("x" %in% arg_names) {
    match("x", arg_names)
  } else {
    match("", arg_names)
  }
  remedy <- term
  remedy[[slot + 1L]] <- call("I", top)
  if (negated) {
    stop("The predictor in ", deparse1(term),
         if (plus_wrapped) " contains a negation outside I()" else
           " is written with a leading minus",
         ", which a model formula reads as the operator that removes a",
         " term, so the model would be fitted on ",
         deparse1(unparen(top[[2]])), ", not on ", deparse1(top),
         ". Write the negation inside I(), as in ",
         deparse1(remedy), ", or compute it into a column of the data and",
         " name that column in crf().", call. = FALSE)
  }
  # terms() refuses some of these forms outright, such as x * 2, and then
  # there is no single expression to name.
  reading <- tryCatch(formula_x_reading(top), error = function(e) NULL)
  stop("The predictor in ", deparse1(term), " has the formula operator ",
       as.character(top[[1]]), " at its top level, which a model formula",
       " reads as formula syntax, not as arithmetic",
       if (!is.null(reading)) {
         paste0(", so the model would be fitted on ", deparse1(reading),
                ", not on ", deparse1(top))
       },
       ". Where arithmetic is meant, write it inside I(), as in ",
       deparse1(remedy), ", or compute it into a column of the data and",
       " name that column in crf().", call. = FALSE)
}

#' @rdname model.frame
#' @order 4
#'
#' @param data A \code{\link[base]{data.frame}} containing the variables
#' specified in \code{formula}.
#'
#' @method model.frame bayesnecformula
#'
#' @inherit model.frame description return examples details
#'
#' @importFrom stats model.frame na.omit
#'
#' @export
model.frame.bayesnecformula <- function(formula, data, ...) {
  if (!inherits(data, "data.frame")) {
    stop("Argument data is not a data.frame.")
  }
  pre_list <- simplify_formula(formula, data, ...)
  data <- model.frame(pre_list$formula, data = data)
  bnec_pop <- na.omit(pre_list$pop_vars)
  attr(data, "bnec_pop") <- bnec_pop
  bnec_group <- pre_list$group_vars
  attr(data, "bnec_group") <- bnec_group
  data
}

#' @noRd
#' @importFrom formula.tools lhs rhs
#' @importFrom stats as.formula terms
simplify_formula <- function(formula, data, ...) {
  formula <- check_formula(formula, data, ...)
  formula_lhs <- lhs(formula)
  if (length(formula_lhs) < 3) {
    y_call <- formula_lhs
    t_call <- 1
    t_var <- NA
    c_call <- 1
    c_vars <- character(0)
    ra_call <- 1
    ra_var <- character(0)
  } else if (length(formula_lhs) == 3) {
    y_call <- formula_lhs[[2]]
    split_terms <- split_calls(formula_lhs[[3]])
    t_call <- split_terms$t_call
    t_var <- split_terms$t_var
    c_call <- split_terms$c_call
    c_vars <- split_terms$c_vars
    ra_call <- split_terms$ra_call
    ra_var <- split_terms$ra_var
  }
  y_var <- all.vars(y_call)
  x_str <- grep("crf(", labels(terms(formula)), fixed = TRUE, value = TRUE)
  x_call <- str2lang(eval(parse(text = x_str)))
  x_var <- all.vars(x_call)
  # Variables named in a disp(~...) term are predictors of the dispersion
  # parameter, not grouping variables. Without this they would fall through to
  # r_vars below and silently acquire a group-level effect on every curve
  # parameter. They are not added to short_form either: brm() is handed the
  # user's full data.frame, so brms resolves them itself.
  disp_spec <- parse_disp_term(formula)
  disp_vars <- if (!is.null(disp_spec) && disp_spec$route == "A") {
    all.vars(str2lang(disp_spec$value))
  } else {
    character(0)
  }
  r_vars <- intersect(names(data),
                      setdiff(all.vars(rhs(formula)), c(x_var, disp_vars)))
  if (length(r_vars) == 0) {
    r_call <- 1
    r_vars <- NA
  } else {
    r_call <- str2lang(paste0(r_vars, collapse = " + "))
  }
  # The rate slot goes between censoring and the grouping terms, in the order
  # pop_vars below lists it. retrieve_var() finds a variable by its position in
  # bnec_pop and indexes the model frame with it, so the term order here and
  # the name order there must match.
  short_form <- substitute(a ~ b + c + d + f + e, list(a = y_call, b = x_call,
                                                       c = t_call, d = c_call,
                                                       f = ra_call,
                                                       e = r_call))
  # Name the slots here rather than assume positions downstream: the trials
  # slot can be absent while the censoring slot is present.
  pop_vars <- c(y_var, x_var, t_var, c_vars, ra_var)
  names(pop_vars) <- c(rep("y_var", length(y_var)),
                       rep("x_var", length(x_var)),
                       rep("trials_var", length(t_var)),
                       names(c_vars),
                       rep("rate_var", length(ra_var)))
  # model.frame() resolves anything that is not a column of the data in the
  # formula's own environment, so attach the user's environment here;
  # as.formula()'s default would give this function's frame. Bind trials() as
  # well: short_form keeps it as a call, and bayesnec does not export it.
  list(formula = as.formula(short_form,
                            env = formula_eval_env(formula, trials = trials)),
       pop_vars = pop_vars, group_vars = r_vars)
}

#' Write the crf() predictor into an equation's right-hand side
#'
#' @param new_x The predictor's column name in the model frame, such as
#' \code{"log(x)"} or \code{"I(x - 1)"}.
#' @param brms_rhs The equation's right-hand side as text, written in
#' \code{x}.
#'
#' @return A \code{\link[base]{character}} string.
#'
#' @noRd
substitute_x_in_formula <- function(new_x, brms_rhs) {
  gsub("[x](?![[:alpha:]])", brms_x_text(new_x), brms_rhs, perl = TRUE)
}

#' The crf() predictor as text for a brms formula
#'
#' @param new_x The predictor's column name in the model frame, such as
#' \code{"log(x)"} or \code{"I(x - 1)"}.
#'
#' @details \pkg{brms} writes a non-linear formula into the Stan program as it
#' stands, and Stan has no \code{I()}, so the expression inside \code{I()} is
#' written instead, including when \code{I()} is nested inside another call.
#' An expression that is neither a name nor a function call
#' is put in parentheses, so that the equation cannot regroup it: written
#' bare into \code{(x - nec)}, \code{x > 1} would read as
#' \code{x > (1 - nec)}.
#'
#' Leave a name or a function call as it is. \code{stored_model_survival()}
#' and \code{identify_dpar_model()} match a stored fit's formula against one
#' built here, so added parentheses would match no fit made before them.
#'
#' @return A \code{\link[base]{character}} string.
#'
#' @noRd
brms_x_text <- function(new_x) {
  unwrapped <- FALSE
  strip_i <- function(expr) {
    if (!is.call(expr)) {
      return(expr)
    }
    if (identical(expr[[1]], quote(I)) && length(expr) == 2) {
      unwrapped <<- TRUE
      return(call("(", strip_i(expr[[2]])))
    }
    for (i in seq_along(expr)[-1]) {
      expr[[i]] <- strip_i(expr[[i]])
    }
    expr
  }
  expr <- strip_i(str2lang(new_x))
  # Keep grouping inside the expression, but use the existing outer form.
  while (is.call(expr) && identical(expr[[1]], quote(`(`))) {
    expr <- expr[[2]]
  }
  fun <- if (is.call(expr) && is.name(expr[[1]])) as.character(expr[[1]])
  if (is.name(expr) || (!is.null(fun) && identical(make.names(fun), fun))) {
    return(if (unwrapped) deparse1(expr) else new_x)
  }
  paste0("(", deparse1(expr), ")")
}

#' @noRd
#' @importFrom formula.tools lhs
wrangle_model_formula <- function(model, formula, data, family = NULL,
                                  model_survival = NULL, level_spec = NULL) {
  # A joint refit whose levels chose different equations is composed whole,
  # so the branches below do not apply. bnec_joint() refuses everything they
  # would add: a group-level term, a hurdle block or a disp() sub-model.
  if (!is.null(level_spec) && isTRUE(level_spec$composed)) {
    brms_bf <- compose_level_formula(formula, data, level_spec)
    if (isTRUE(level_spec$disp) && !is.null(family) && has_disp_par(family)) {
      brms_bf <- brms_bf +
        lf(as.formula(paste0(disp_dpar(family), " ~ 0 + ",
                             level_spec$group_var)))
    }
    environment(brms_bf$formula) <- formula_env(formula)
    return(brms_bf)
  }
  brms_bf <- get(paste0("bf_", model))
  brms_bf[[1]][[2]] <- lhs(formula)
  bnec_pop_vars <- attr(data, "bnec_pop")
  new_x <- names(data)[which(names(bnec_pop_vars) == "x_var")]
  brms_rhs <- deparse1(brms_bf[[1]][[3]])
  tmp <- substitute_x_in_formula(new_x, brms_rhs)
  brms_bf[[1]][[3]] <- str2lang(tmp)
  # Read before any block is appended. The blocks below add sub-formulas, such
  # as ogl, botgl, the hurdle block and the dispersion block, and a level term
  # belongs on the curve's own parameters only.
  curve_pars <- names(brms_bf[[2]])
  bnec_group_vars <- attr(data, "bnec_group")
  if (any(!is.na(bnec_group_vars))) {
    brms_bf <- add_formula_glef(model, brms_bf, formula, data,
                                family = family)
  }
  # Hurdle families get a second, derived parameter block for the hurdle
  # probability. Add it after the group-level terms so that those apply to the
  # response block only: a group-level effect shared by the two blocks would
  # break the factorisation that bnec_hurdle() relies on.
  # The second block may use a different equation, named by model_survival;
  # it defaults to the response block's.
  if (!is.null(family) && is_hurdle_family(family)) {
    hu_model <- if (is.null(model_survival)) model else model_survival
    brms_bf <- add_hu_block(brms_bf, hu_model, new_x, hurdle_dpar(family))
  }
  # A disp() term adds a sub-model for the family's dispersion parameter. Added
  # last so that the curve expression it duplicates (route B) is the one any
  # group-level terms above have already been applied to.
  disp_spec <- parse_disp_term(formula)
  # brms accepts rate() for poisson and negbinomial only. The check needs the
  # family, so it is made here rather than in check_formula(). The
  # zero-inflated count families get their own message, because they are what
  # a user reaches for next, and no offset does the same job under an identity
  # link.
  rate_var <- retrieve_var(data, "rate_var")
  if (!is.null(rate_var) && !is.null(family)) {
    fam_tag <- if (inherits(family, "family")) family$family else family
    if (!fam_tag %in% c("poisson", "negbinomial")) {
      extra <- if (grepl("^zero_inflated_", fam_tag)) {
        paste0(" brms cannot place a rate denominator on a zero-inflated",
               " count family, and under bnec()'s identity link there is no",
               " offset that would do the same job. Model the exposure as a",
               " predictor, or use family = \"", sub("^zero_inflated_", "",
                                                    fam_tag), "\".")
      } else {
        " Use family = \"poisson\" or family = \"negbinomial\"."
      }
      stop("A rate() term is only valid for the poisson and negbinomial",
           " families; you supplied \"", fam_tag, "\".", extra,
           call. = FALSE)
    }
  }
  if (!is.null(disp_spec)) {
    if (is.null(family)) {
      stop("A disp() term needs the model family to know which dispersion",
           " parameter it applies to. Pass `family` to make_brmsformula(), or",
           " build the formula through bnec().", call. = FALSE)
    }
    # The response on the scale of the mean the variance function is written
    # in; see disp_response(). rate_var is the read made for the family check
    # above. bnec() makes the same check before its model loop, and this one is
    # the backstop for make_brmsformula() and the other direct routes.
    disp_y <- disp_response(data, family, rate_var = rate_var)
    check_disp_spec(disp_spec, family, response = disp_y)
    if (shape_in_reported_mean(formula, family)) {
      check_disp_count_truncation(formula, family)
    }
    brms_bf <- add_disp_block(brms_bf, model, disp_spec, family, new_x, disp_y)
  }
  # Applied last of the blocks, so that the right-hand side a level term
  # replaces is the one every other route has finished with. There is no
  # user-facing syntax for a level term: level_spec is built by
  # bnec_joint.bayesnecgroupfit() and reaches here through fit_bayesnec().
  if (!is.null(level_spec)) {
    brms_bf <- add_formula_level_terms(brms_bf, level_spec, curve_pars, family)
  }
  # brms resolves every name in the formula that is not a column of the data,
  # sub-models included, in the environment of the top-level formula. The
  # bf_<model> template gives the global environment, so set the user's here.
  # Setting it on a sub-model formula alone has no effect.
  environment(brms_bf$formula) <- formula_env(formula)
  brms_bf
}

#' @noRd
#' @importFrom stats update terms
single_model_formula <- function(formula, model) {
  # update() drops attributes, so carry the internal ones across by hand.
  internal_truncation <- attr(formula, "bayesnec_internal_truncation")
  tested_range <- attr(formula, "bayesnec_tested_range")
  x_str <- grep("crf(", labels(terms(formula)), fixed = TRUE, value = TRUE)
  x_term <- eval(parse(text = x_str))
  new_crf <- paste0("crf(", x_term, ", model = \"", model, "\")")
  to_eval <- paste0("update(formula, ~ . - ", x_str, " + ", new_crf, ")")
  formula <- eval(parse(text = to_eval))
  attr(formula, "bayesnec_internal_truncation") <- internal_truncation
  attr(formula, "bayesnec_tested_range") <- tested_range
  bayesnecformula(formula)
}

#' @noRd
clean_bar_glef <- function(x) {
  x <- strsplit(x, split = " | ", fixed = TRUE)[[1]]
  gsub("\\(|\\)", "", x)
}

#' The grouping terms written inside ogl() or pgl()
#'
#' @param str_calls A \code{\link[base]{character}} vector of deparsed
#' \code{ogl()} or \code{pgl()} calls, as split from the right-hand side of a
#' \code{\link{bayesnecformula}}.
#'
#' @details R's formula terms expand each argument, so that
#' \code{ogl(a + b)} remains the two terms \code{(1 | a) + (1 | b)}. A nesting
#' operator is expanded by the rule \pkg{brms} applies,
#' \code{a/b} to \code{a} and \code{a:b}, and \code{a/b/c} to \code{a},
#' \code{a:b} and \code{a:b:c}. The same expansion also resolves \code{a*b}
#' to \code{a}, \code{b} and \code{a:b}, so these terms can be deduplicated.
#'
#' The nesting is expanded here rather than passed to \pkg{brms} as
#' \code{(1 | a/b)} so that duplicates can be removed across terms:
#' \pkg{brms} refuses \code{(1 | a/b) + (1 | a)} as a duplicated group-level
#' effect. The expanded terms are the ones \pkg{brms} builds from
#' \code{(1 | a/b)}, with the same group names, so the model is the same.
#'
#' Do not reduce the terms to their variable names: \code{a/b} and \code{a:b}
#' would both become \code{(1 | a) + (1 | b)}, which is a different model.
#'
#' @return A \code{\link[base]{character}} vector of grouping terms, with
#' duplicates removed.
#'
#' @noRd
gl_term_groups <- function(str_calls) {
  # Use the same formula algebra that brms applies to a bar term. Keeping an
  # expression such as a*b intact would hide its a term from deduplication.
  groups <- lapply(str_calls, function(s) {
    lapply(as.list(str2lang(s))[-1], function(arg) {
      attr(stats::terms(stats::as.formula(call("~", arg))), "term.labels")
    })
  })
  unique(unlist(groups, use.names = FALSE))
}

#' Apply a group-level deviation to one parameter multiplicatively
#'
#' @param brmform The \pkg{brms} formula being built.
#' @param par A \code{\link[base]{character}} string naming the parameter.
#' @param var A \code{\link[base]{character}} string naming the grouping
#' variable.
#' @param kind \code{"logit"} or \code{"log"}, from
#' \code{\link{par_transform_kind}}.
#'
#' @details The parameter-level form of the multiplicative deviation that
#' \code{ogl()} applies to the mean. Instead of \code{bot ~ 1 + (1 | g)},
#' which adds an unconstrained deviation to \code{bot} on its own scale and
#' lets a leapfrog step take it below zero, the curve reads an intermediate:
#'
#' \preformatted{
#'   y       ~ bnecbot + (top - bnecbot) * exp(...)
#'   bnecbot ~ bot * exp(botgl) / (1 - bot + bot * exp(botgl))
#'   bot     ~ 1
#'   botgl   ~ 0 + (1 | g)
#' }
#'
#' \code{bot} is still a population-level non-linear parameter with its own
#' prior and name, so \code{b_bot_Intercept} and everything that reads it are
#' unchanged. A zero deviation leaves \code{bot} unchanged. For an admissible
#' \code{bot}, the logit form above preserves \code{(0, 1)}; the log form
#' \code{bot * exp(botgl)} preserves positivity.
#'
#' The deviation has no population intercept. \code{bnecbot} depends on
#' \code{bot} and \code{botgl} only through their combination, so a free
#' \code{b_botgl} would be unidentified against \code{bot}, and
#' \code{b_bot_Intercept} would no longer be the asymptote that
#' \code{ecx(type = "relative")} and \code{\link{expand_nec}} read. Writing
#' the sub-formula with \code{0 +} declares no \code{b_botgl}. \code{ogl()}
#' differs here: it keeps its own intercept.
#'
#' The intermediate is built by setting the \code{nl} and \code{loop}
#' attributes, because \code{brms::nlf()} returns a list for \code{bf()} to
#' unpack rather than a formula.
#'
#' @return The modified \pkg{brms} formula.
#'
#' @importFrom stats as.formula
#' @importFrom formula.tools rhs `rhs<-`
#'
#' @noRd
add_par_gl_term <- function(brmform, par, var, kind) {
  nms <- par_gl_names(par)
  if (is.null(brmform[[2]][[nms[["dev"]]]])) {
    # First term on this parameter. Substitution is on the symbol rather than
    # on the deparsed string so that a parameter name occurring inside a longer
    # name cannot be hit by accident.
    brmform[[1]][[3]] <- eval(call("substitute", brmform[[1]][[3]],
                                   stats::setNames(list(as.name(nms[["inter"]])),
                                                   par)))
    brmform[[2]][[nms[["dev"]]]] <- as.formula(paste(nms[["dev"]], "~ 0"))
    inter_form <- as.formula(
      paste(nms[["inter"]], "~",
            ogl_transform_expr(kind, m = par, o = nms[["dev"]]))
    )
    attr(inter_form, "nl") <- TRUE
    attr(inter_form, "loop") <- TRUE
    brmform[[2]][[nms[["inter"]]]] <- inter_form
  }
  tmp_rhs <- deparse1(rhs(brmform[[2]][[nms[["dev"]]]]))
  # Test for the term, not the variable name: a name test would drop a
  # grouping whose name is a substring of one already added, such as grp
  # after grp2.
  if (!has_gl_term(tmp_rhs, var)) {
    rhs(brmform[[2]][[nms[["dev"]]]]) <-
      str2lang(paste0(tmp_rhs, " + (1 |", var, ")"))
  }
  brmform
}

#' Whether a sub-formula's right-hand side already groups by a variable
#'
#' @param rhs_str The deparsed right-hand side.
#' @param var A \code{\link[base]{character}} string naming the grouping
#' variable.
#'
#' @details \code{deparse1()} of a term appended as \code{" + (1 |", var, ")"}
#' renders as \code{(1 | var)}, with the space \pkg{R} inserts around the bar.
#' Both spellings are tested so that this does not depend on how the string was
#' built.
#'
#' @return A \code{\link[base]{logical}}.
#' @noRd
has_gl_term <- function(rhs_str, var) {
  any(vapply(c(paste0("(1 | ", var, ")"), paste0("(1 |", var, ")")),
             grepl, logical(1), x = rhs_str, fixed = TRUE))
}

#' @noRd
#' @importFrom stats terms
#' @importFrom formula.tools rhs `rhs<-`
add_formula_glef <- function(model, brmform, bnecform, data,
                             family = NULL) {
  crf_term <- grep("crf(", labels(terms(bnecform)), fixed = TRUE,
                   value = TRUE)
  to_eval <- paste0("update(bnecform, ~ . - ", crf_term, ")")
  random_call <- rhs(eval(parse(text = to_eval)))
  random_call <- gsub("\\) \\+ ", ") impossiblestr ", deparse1(random_call))
  split_random_call <- strsplit(random_call, " impossiblestr ")[[1]]
  # Decide once, before either branch, so that pgl() expands to exactly the
  # (par | group) terms written out by hand. Only top and bot are transformed;
  # see par_transform_pars().
  par_kind <- par_transform_kind(family)
  # Read before either branch, because add_par_gl_term() appends sub-formulas of
  # its own. Iterating over a list that is growing would put a group-level term
  # on the generated terms as well as on the parameters, and validating the
  # explicit terms against it would accept (botgl | group) as a parameter of the
  # model whenever a pgl term had already generated botgl.
  model_pars <- names(brmform[[2]])
  if (any(grepl("pgl(", split_random_call, fixed = TRUE))) {
    str_calls <- grep("pgl(", split_random_call, fixed = TRUE, value = TRUE)
    vars <- gl_term_groups(str_calls)
    for (p in model_pars) {
      for (j in seq_along(vars)) {
        if (par_is_transformed(p, par_kind)) {
          brmform <- add_par_gl_term(brmform, p, vars[j], par_kind)
        } else {
          brmform[[2]][[p]] <- str2lang(paste0(deparse1(brmform[[2]][[p]]),
                                               " + (1 |", vars[j], ")"))
        }
      }
    }
  }
  if (any(grepl("|", split_random_call, fixed = TRUE))) {
    str_calls <- grep("|", split_random_call, fixed = TRUE, value = TRUE)
    split_str_calls <- lapply(str_calls, clean_bar_glef)
    tmp_list <- list()
    for (i in seq_along(split_str_calls)) {
      if (any(grepl(" \\+ ", split_str_calls[[i]][1]))) {
        extended_pars <- strsplit(split_str_calls[[i]][1], " \\+ ")[[1]]
        tmp_list_i <- vector(mode = "list", length = length(extended_pars))
        for (j in seq_along(tmp_list_i)) {
          tmp_list_i[[j]] <- c(extended_pars[j], split_str_calls[[i]][2])
        }
        tmp_list <- c(tmp_list, tmp_list_i)
      } else {
        tmp_list <- c(tmp_list, split_str_calls[i])
      }
    }
    split_str_calls <- tmp_list
    pars <- sapply(split_str_calls, `[[`, 1)
    vars <- sapply(split_str_calls, `[[`, 2)
    if (!all(pars %in% model_pars)) {
      to_flag <- pars[!pars %in% model_pars]
      message("The parameter(s) ", paste0("\"", to_flag, "\"", collapse = "; "),
              " are not valid parameters in ", model, ". Ignoring...")
      # Subset by membership, not match(), which finds the first position only
      # and would keep the second of (nec | g) + (nec | h) for a missing nec.
      split_str_calls <- split_str_calls[pars %in% model_pars]
      pars <- sapply(split_str_calls, `[[`, 1)
      vars <- sapply(split_str_calls, `[[`, 2)
    }
    if (length(split_str_calls) > 0) {
      for (k in seq_along(pars)) {
        if (par_is_transformed(pars[k], par_kind)) {
          brmform <- add_par_gl_term(brmform, pars[k], vars[k], par_kind)
          next
        }
        tmp_rhs <- deparse1(rhs(brmform[[2]][[pars[k]]]))
        # See has_gl_term().
        if (!has_gl_term(tmp_rhs, vars[k])) {
          rhs(brmform[[2]][[pars[k]]]) <- str2lang(paste0(tmp_rhs, " + (1 |",
                                                          vars[k], ")"))
        }
      }
    }
  }
  if (any(grepl("ogl(", split_random_call, fixed = TRUE))) {
    str_calls <- grep("ogl(", split_random_call, fixed = TRUE, value = TRUE)
    vars <- gl_term_groups(str_calls)
    kind <- ogl_transform_kind(model, family)
    if (identical(kind, "none")) {
      # The additive offset, kept for a mean the likelihood does not
      # constrain, for a non-identity link, where the offset is already on the
      # linear predictor, and for equations whose mean can reach or pass a
      # bound, where the transform is undefined. See ogl_transform_kind().
      tmp <- paste0("ogl + ", deparse1(brmform[[1]][[3]]))
      brmform[[1]][[3]] <- str2lang(tmp)
    } else {
      # The curve becomes an intermediate and the deviation is applied to it
      # multiplicatively, so mu cannot leave its support however long the
      # leapfrog trajectory is. The deviation is zero-centred, so top, bot, nec
      # and beta keep their meanings. The nl and loop attributes are set
      # directly, because brms::nlf() returns a list rather than a formula.
      curve <- deparse1(brmform[[1]][[3]])
      brmform[[1]][[3]] <- str2lang(ogl_transform_expr(kind))
      curve_form <- stats::as.formula(paste("bnecmu ~", curve))
      attr(curve_form, "nl") <- TRUE
      attr(curve_form, "loop") <- TRUE
      # Prepended, so the intermediate is defined before the terms that read it.
      brmform[[2]] <- c(list(bnecmu = curve_form), brmform[[2]])
    }
    brmform[[2]]$ogl <- ogl ~ 1
    for (j in seq_along(vars)) {
      brmform[[2]]$ogl <- str2lang(paste0(deparse1(brmform[[2]]$ogl),
                                          " + (1 |", vars[j], ")"))
    }
  }
  brmform
}

#' Give every curve parameter a separate value per level of a factor
#'
#' @param brmform The \pkg{brms} formula being built.
#' @param level_spec The level-term specification built by
#' \code{\link{bnec_joint}}; see its \code{group_var}, \code{levels} and
#' \code{disp} elements.
#' @param curve_pars The equation's own parameters, read from the formula
#' before any block was appended to it.
#' @param family An object of class \code{\link[stats]{family}}, or
#' \code{NULL}.
#'
#' @details For a joint refit by \code{\link{bnec_joint}}. Where
#' \code{\link{add_formula_glef}} \emph{appends} a group-level term to a
#' parameter's right-hand side, this \emph{replaces} the population part of
#' it: \code{top ~ 1} becomes \code{top ~ 0 + site}, dummy coding of the
#' factor onto that parameter. \code{0 +} rather than \code{1 +} gives every
#' level a coefficient on the parameter's own scale, so the bounds
#' \code{\link{define_prior}} declares apply to each of them.
#'
#' Only the leading \code{1} is replaced, so a group-level term already
#' appended survives: \code{1 + (1 | tank)} becomes
#' \code{0 + site + (1 | tank)}.
#'
#' The dispersion parameter is treated the same way where \code{disp} is
#' \code{TRUE} and the family has one. \pkg{brms} puts a predicted dispersion
#' on the log link, so those coefficients take the log-scale prior
#' \code{\link{define_disp_prior}} uses for \code{c0}; see
#' \code{\link{add_level_defaults}}.
#'
#' @return An object of class \code{\link[brms]{brmsformula}}.
#'
#' @importFrom brms lf
#' @importFrom formula.tools rhs `rhs<-`
#' @importFrom stats as.formula
#'
#' @noRd
add_formula_level_terms <- function(brmform, level_spec, curve_pars,
                                    family = NULL) {
  var <- level_spec$group_var
  for (p in intersect(curve_pars, names(brmform[[2]]))) {
    tmp_rhs <- deparse1(rhs(brmform[[2]][[p]]))
    rhs(brmform[[2]][[p]]) <-
      str2lang(sub("^1", paste0("0 + ", var), tmp_rhs))
  }
  if (isTRUE(level_spec$disp) && !is.null(family) && has_disp_par(family)) {
    # lf() rather than nlf(), and for the reason add_disp_block() gives for
    # route (A): the right-hand side is an ordinary design matrix in the data.
    brmform <- brmform +
      lf(as.formula(paste0(disp_dpar(family), " ~ 0 + ", var)))
  }
  brmform
}

#' A per-level suffix for the parameters of a composed joint refit
#'
#' @param levels A \code{\link[base]{character}} vector of factor levels.
#'
#' @details Where the levels of a joint refit chose different equations, each
#' level's parameters are separate \pkg{brms} non-linear parameters and need
#' names of their own. \code{validate_par_formula()} refuses a name containing
#' a dot or an underscore, so \code{top_a} is illegal. Levels differing only in
#' punctuation, such as \code{"site 1"} and \code{"site-1"}, share an
#' alphanumeric stem, so the stem alone is not unique. The tag must also not
#' depend on the order of the levels, or the same data fitted twice would give
#' two sets of parameter names.
#'
#' The rank is therefore taken over the sorted level set and appended after a
#' literal \code{Lv}. A rank is all digits, so the last \code{Lv} in a tag is
#' always the separator and no two levels produce the same tag.
#'
#' @return A named \code{\link[base]{character}} vector, one tag per level.
#'
#' @noRd
level_par_tags <- function(levels) {
  levels <- as.character(levels)
  stem <- gsub("[^A-Za-z0-9]", "", levels)
  stem[!nzchar(stem)] <- "lev"
  stem <- substr(stem, 1, 12)
  rank <- match(levels, sort(unique(levels)))
  stats::setNames(paste0(stem, "Lv", rank), levels)
}

#' Rename the symbols of an equation template
#'
#' Walks the language object rather than editing its deparsed text, so that a
#' parameter name is never matched inside a longer one and the predictor is
#' replaced by an expression rather than by a string.
#'
#' @param e A \code{\link[base]{language}} object.
#' @param map A named \code{\link[base]{list}} of replacements, each itself a
#' language object.
#'
#' @return A \code{\link[base]{language}} object.
#'
#' @noRd
rename_syms <- function(e, map) {
  if (is.symbol(e)) {
    nm <- as.character(e)
    if (!is.null(map[[nm]])) {
      return(map[[nm]])
    }
    return(e)
  }
  if (is.call(e) && length(e) > 1) {
    # The function position is left alone: step() and exp() are calls, not
    # parameters, and a parameter never appears there.
    for (i in seq_along(e)[-1]) {
      e[[i]] <- rename_syms(e[[i]], map)
    }
  }
  e
}

#' Compose one equation per level into a single brms formula
#'
#' @param formula An object of class \code{\link{bayesnecformula}}.
#' @param data The model frame, carrying the \code{bnec_pop} attribute.
#' @param level_spec The level-term specification built by
#' \code{\link{bnec_joint}}; see its \code{levels}, \code{models}, \code{tags},
#' \code{inds} and \code{x_ref} elements.
#'
#' @details The general case of a joint refit. The mean is a sum of one term
#' per level, each the level's own equation multiplied by an indicator column
#' that is one on the rows of that level and zero elsewhere. Where every level
#' chose the same equation, \code{\link{add_formula_level_terms}} builds the
#' equivalent dummy coding directly, and that branch is taken instead.
#'
#' Mask the predictor, not the result. Every level's sub-expression is
#' evaluated on every row, and some equations overflow outside the range a
#' level was measured over; in Stan \code{0 * inf} is \code{NaN}, which
#' poisons the log density. Each level's \code{x} is therefore replaced by
#' \code{ind * x + (1 - ind) * x_ref}, where \code{x_ref} is one of that
#' level's own observed predictor values. On another level's rows, a level
#' then evaluates its equation at a predictor value of its own.
#'
#' @return An object of class \code{\link[brms]{brmsformula}}.
#'
#' @importFrom brms bf
#' @importFrom formula.tools lhs
#' @importFrom stats as.formula setNames
#'
#' @noRd
compose_level_formula <- function(formula, data, level_spec) {
  bnec_pop_vars <- attr(data, "bnec_pop")
  new_x <- names(data)[which(names(bnec_pop_vars) == "x_var")]
  terms <- character(0)
  pforms <- list()
  # Collected separately from pforms, which is indexed by name and would
  # silently overwrite a collision rather than reveal one.
  par_names <- character(0)
  for (l in level_spec$levels) {
    tag <- level_spec$tags[[l]]
    ind <- level_spec$inds[[l]]
    tmpl <- get(paste0("bf_", level_spec$models[[l]]))
    pars <- names(tmpl[[2]])
    map <- setNames(lapply(paste0(pars, tag), as.name), pars)
    map[["x"]] <- str2lang(
      paste0("(", ind, " * (", brms_x_text(new_x), ") + (1 - ", ind,
             ") * ", format(level_spec$x_ref[[l]], digits = 17), ")")
    )
    terms <- c(terms,
               paste0(ind, " * (", deparse1(rename_syms(tmpl[[1]][[3]], map)),
                      ")"))
    for (p in pars) {
      pforms[[paste0(p, tag)]] <- as.formula(paste0(p, tag, " ~ 1"))
      par_names <- c(par_names, paste0(p, tag))
    }
  }
  if (anyDuplicated(par_names)) {
    stop("Two levels of \"", level_spec$group_var, "\" produced the same",
         " parameter name in the joint refit (",
         paste0(unique(par_names[duplicated(par_names)]), collapse = ", "),
         "). Please report this with the level names.", call. = FALSE)
  }
  mu <- as.formula(paste0("y ~ ", paste0(terms, collapse = " + ")))
  mu[[2]] <- lhs(formula)
  do.call(bf, c(list(mu), unname(pforms), list(nl = TRUE)))
}

#' Describe the group-level structure a bayesnecformula carries
#'
#' @param formula An object of class \code{\link{bayesnecformula}}.
#' @param model A \code{\link[base]{character}} string naming a single model,
#' needed to expand a \code{pgl} term over the parameters that model actually
#' has.
#'
#' @details The counterpart of \code{\link{parse_disp_term}}. It describes the
#' group-level terms so that \code{define_prior()} can generate priors for the
#' parameters they introduce.
#'
#' The parsing repeats \code{add_formula_glef()}'s rather than sharing it, so
#' any change to the accepted term syntax has to be made in both.
#'
#' A \code{(par | group)} term naming a parameter the model does not have is
#' dropped here, because \code{add_formula_glef()} ignores it and a prior for
#' it would never reach the fit.
#'
#' \code{nlpars} names the parameters the user put a term on, not the names
#' those terms are declared under in the fit. Where the deviation is applied
#' multiplicatively the standard deviation is declared on \code{botgl} rather
#' than on \code{bot}, but that depends on the family, which this function is
#' not given. \code{\link{define_group_prior}} makes that mapping.
#'
#' @return A \code{\link[base]{list}} with elements \code{nlpars}, the
#' non-linear parameters carrying a group-level standard deviation, and
#' \code{ogl}, whether an \code{ogl} offset parameter was added; or
#' \code{NULL} when the formula carries no group-level term.
#'
#' @importFrom stats terms
#' @importFrom formula.tools rhs
#'
#' @noRd
parse_group_terms <- function(formula, model) {
  crf_term <- grep("crf(", labels(terms(formula)), fixed = TRUE, value = TRUE)
  if (length(crf_term) == 0) {
    return(NULL)
  }
  to_eval <- paste0("update(formula, ~ . - ", crf_term, ")")
  random_call <- rhs(eval(parse(text = to_eval)))
  random_call <- gsub("\\) \\+ ", ") impossiblestr ", deparse1(random_call))
  split_random_call <- strsplit(random_call, " impossiblestr ")[[1]]
  # disp() is a variance function, not a grouping term, and has its own prior
  # route through define_disp_prior(). Dropping it here keeps a formula that
  # carries only a disp() term from being reported as grouped.
  split_random_call <- split_random_call[!grepl("disp(", split_random_call,
                                                fixed = TRUE)]
  model_pars <- names(get(paste0("bf_", model))[[2]])
  nlpars <- character(0)
  has_ogl <- FALSE
  if (any(grepl("pgl(", split_random_call, fixed = TRUE))) {
    # pgl puts a group-level term on every parameter of the model at once.
    nlpars <- c(nlpars, model_pars)
  }
  if (any(grepl("ogl(", split_random_call, fixed = TRUE))) {
    has_ogl <- TRUE
    nlpars <- c(nlpars, "ogl")
  }
  bar_calls <- grep("|", split_random_call, fixed = TRUE, value = TRUE)
  bar_calls <- bar_calls[!grepl("pgl(", bar_calls, fixed = TRUE) &
                           !grepl("ogl(", bar_calls, fixed = TRUE)]
  if (length(bar_calls) > 0) {
    split_str_calls <- lapply(bar_calls, clean_bar_glef)
    for (i in seq_along(split_str_calls)) {
      pars_i <- strsplit(split_str_calls[[i]][1], " \\+ ")[[1]]
      nlpars <- c(nlpars, trimws(pars_i))
    }
    nlpars <- intersect(nlpars, c(model_pars, "ogl"))
  }
  nlpars <- unique(nlpars)
  if (length(nlpars) == 0) {
    return(NULL)
  }
  list(nlpars = nlpars, ogl = has_ogl)
}

#' The equations that keep no group-level term on a grouping variable
#'
#' @param formula An object of class \code{\link{bayesnecformula}}.
#' @param models A \code{\link[base]{character}} vector of equation names.
#'
#' @details A \code{(par | group)} term naming a parameter an equation does
#' not have is left out of that equation by \code{add_formula_glef()}. Where
#' every term on a grouping variable is left out, the equation would be fitted
#' as one curve pooled over the levels of that variable, while the rest of the
#' set fits a deviation per level, so the model weights would compare
#' different group-level structures. Such an equation is excluded before it is
#' fitted.
#'
#' The rule is applied per grouping variable: an equation must keep at least
#' one term on each variable a \code{(par | group)} term names. An
#' \code{ogl()} or \code{pgl()} term on the variable counts, because both
#' apply to every equation. A term that is only partly left out still counts:
#' under \code{(nec | batch) + (ec50 | batch)}, \code{nec3param} keeps the term
#' on \code{nec} and \code{ecxll3} the term on \code{ec50}, and both are
#' fitted.
#'
#' The rule also ensures that the data \pkg{brms} stores on every fitted
#' equation hold every grouping variable, which the prediction grid needs.
#'
#' The terms are parsed as \code{parse_group_terms()} parses them.
#'
#' @return A \code{\link[base]{data.frame}} with columns \code{model} and
#' \code{reason}, one row per equation excluded, in the form
#' \code{check_models()} records its exclusions in.
#'
#' @importFrom stats terms
#' @importFrom formula.tools rhs
#'
#' @noRd
group_term_exclusions <- function(formula, models) {
  out <- data.frame(model = character(), reason = character(),
                    stringsAsFactors = FALSE)
  crf_term <- grep("crf(", labels(terms(formula)), fixed = TRUE, value = TRUE)
  if (length(crf_term) == 0) {
    return(out)
  }
  to_eval <- paste0("update(formula, ~ . - ", crf_term, ")")
  random_call <- rhs(eval(parse(text = to_eval)))
  random_call <- gsub("\\) \\+ ", ") impossiblestr ", deparse1(random_call))
  split_random_call <- strsplit(random_call, " impossiblestr ")[[1]]
  split_random_call <- split_random_call[!grepl("disp(", split_random_call,
                                                fixed = TRUE)]
  is_gl <- grepl("pgl(", split_random_call, fixed = TRUE) |
    grepl("ogl(", split_random_call, fixed = TRUE)
  bar_calls <- split_random_call[grepl("|", split_random_call, fixed = TRUE) &
                                   !is_gl]
  if (length(bar_calls) == 0) {
    return(out)
  }
  gl_vars <- if (any(is_gl)) {
    all.vars(str2lang(paste0(split_random_call[is_gl], collapse = " + ")))
  } else {
    character(0)
  }
  # The variables of a nested or interaction grouping, a/b or a:b, are each
  # held in the stored data only if a kept term names them, so the rule is
  # read per variable rather than per grouping expression.
  bars <- lapply(bar_calls, function(term) {
    parts <- clean_bar_glef(term)
    list(term = term, pars = trimws(strsplit(parts[1], " \\+ ")[[1]]),
         vars = all.vars(str2lang(parts[2])))
  })
  bar_vars <- setdiff(unique(unlist(lapply(bars, `[[`, "vars"))), gl_vars)
  for (m in models) {
    m_pars <- names(get(paste0("bf_", m))[[2]])
    lost <- character(0)
    for (v in bar_vars) {
      on_v <- Filter(function(b) v %in% b$vars, bars)
      keeps_one <- any(vapply(on_v, function(b) any(b$pars %in% m_pars),
                              logical(1)))
      if (!keeps_one) {
        terms_v <- vapply(on_v, `[[`, character(1), "term")
        lost <- c(lost, paste0(
          "keeps no group-level term on \"", v, "\": ",
          if (length(terms_v) == 1) {
            paste0(terms_v, " names no parameter it has")
          } else {
            paste0("none of ", paste0(terms_v, collapse = ", "),
                   " names a parameter it has")
          }
        ))
      }
    }
    if (length(lost) > 0) {
      out <- rbind(out, data.frame(model = m,
                                   reason = paste0(lost, collapse = "; "),
                                   stringsAsFactors = FALSE))
    }
  }
  out
}

#' Exclude the equations that keep no group-level term on a variable
#'
#' @param model A \code{\link[base]{character}} vector of equation names.
#' @param formula An object of class \code{\link{bayesnecformula}}.
#' @param report Whether to report the exclusions by message.
#' \code{bnec_group()} reports them once for every level and sets this to
#' \code{FALSE} for each level's \code{bnec()} call.
#'
#' @details Raised where the model set is fixed for the whole call: in
#' \code{bnec()} before its model loop, in \code{bnec_group()} before its level
#' loop, and in \code{amend()} for the equations it adds. Left to
#' \code{fit_bayesnec()}, which runs inside the \code{try()} of the model loop,
#' a refusal would be printed once per equation and the call would end on the
#' advice that none of the models fitted. \code{fit_bayesnec()} keeps the test
#' as the backstop for \code{bnec_joint()} on a \code{bayesnecgroupfit}, which
#' fits through it directly. See \code{group_term_exclusions()} for the rule.
#'
#' @return A \code{\link[base]{list}} with elements \code{model}, the equations
#' kept, in their order, and \code{excluded}, the rows to add to the exclusion
#' record. Stops where the rule leaves no equation.
#'
#' @noRd
check_group_terms <- function(model, formula, report = TRUE) {
  excluded <- group_term_exclusions(formula, model)
  kept <- setdiff(model, excluded$model)
  # Refused only where this rule emptied the set. A set that check_models()
  # has already emptied is passed through, so that the refusal the caller
  # raises for it, which names the family or the predictor, is the one
  # reported.
  if (length(kept) == 0 && nrow(excluded) > 0) {
    stop(group_terms_refusal(excluded), call. = FALSE)
  }
  if (report && nrow(excluded) > 0) {
    message(group_terms_report(excluded))
  }
  list(model = kept, excluded = excluded)
}

#' The report of the equations excluded for their group-level terms
#'
#' @param excluded The rows \code{group_term_exclusions()} returns.
#'
#' @return A \code{\link[base]{character}} string.
#' @noRd
group_terms_report <- function(excluded) {
  paste0("Dropping the model(s) ", paste0(excluded$model, collapse = ", "),
         " as each would be fitted as one curve pooled over the levels of a",
         " grouping variable the formula gives a group-level term: ",
         paste0(excluded$model, " ", excluded$reason, collapse = "; "),
         ". The reasons are recorded; see ?bnec_record.")
}

#' The refusal for a set from which every equation is excluded
#'
#' @param excluded The rows \code{group_term_exclusions()} returns.
#'
#' @return A \code{\link[base]{character}} string.
#' @noRd
group_terms_refusal <- function(excluded) {
  paste0("No equation requested can be fitted with the group-level terms in",
         " the formula: ",
         paste0(excluded$model, " ", excluded$reason, collapse = "; "),
         ". An equation is fitted only where it keeps at least one term on",
         " each grouping variable a (par | group) term names, because without",
         " one it would be fitted as one curve pooled over the levels of that",
         " variable. Write a term on a parameter the equations have (see",
         " ?show_params), or use ogl() or pgl(), which apply to every",
         " equation. See ?bayesnecformula.")
}

#' @noRd
#' @importFrom stats terms
get_model_from_formula <- function(formula) {
  x_str <- grep("crf(", labels(terms(formula)), fixed = TRUE, value = TRUE)
  if (length(x_str) == 0) {
    stop("You must specify which non-linear function to use with crf")
  }
  x_str <- paste0(substr(x_str, 1, nchar(x_str) - 1), ", \"model\")")
  # Evaluate in a frame that binds crf() and inherits from the formula's own
  # environment. crf() is not exported, so environment(formula) alone cannot
  # find it, and crf() resolves its model argument in parent.frame(), which
  # is this frame.
  model <- eval(parse(text = x_str),
                envir = formula_eval_env(formula, crf = crf))
  if (is.list(model) && length(model) > 0 && !is.null(names(model)) &&
      !anyNA(names(model)) && all(nzchar(names(model))) &&
      all(vapply(model, inherits, logical(1), what = "brmsformula"))) {
    model <- names(model)
  }
  # Check here rather than leave it to expand_model_set(), which fails with
  # "'match' requires vector arguments" for anything that is not a vector,
  # such as a function or data frame the name is bound to in the caller.
  if (!is.character(model) || length(model) == 0) {
    stop("The `model` argument of crf() must be a character vector naming one",
         " or more equations or model groups, or a named list of equation",
         " formulas returned by models();",
         " here it resolved to an object of",
         " class \"", class(model)[1], "\" and length ", length(model),
         ". See ?models.", call. = FALSE)
  }
  expand_model_set(model)
}

#' @noRd
split_calls <- function(formula_part) {
  t_call <- 1
  t_var <- NA
  c_call <- 1
  c_vars <- character(0)
  ra_call <- 1
  ra_var <- character(0)
  if (length(formula_part) >= 2) {
    tmp_ <- list()
    n <- 0
    # aterms are joined by "+", so peel the chain apart from the right. The
    # `+` test matters: without it any two-argument aterm call is itself length
    # 3 and gets destructured as though it were a chain, which silently loses
    # cens(indicator, upper_bound) -- the interval-censoring form.
    while (length(formula_part) == 3 &&
             identical(formula_part[[1]], quote(`+`))) {
      n <- n + 1
      tmp_[[n]] <- formula_part[[3]]
      formula_part <- formula_part[[2]]
    }
    tmp_[[n + 1]] <- formula_part
    if (any(grepl("trials(", tmp_, fixed = TRUE))) {
      t_call <- tmp_[[grep("trials(", tmp_, fixed = TRUE)]]
      t_var <- all.vars(t_call)
    }
    if (any(grepl("cens(", tmp_, fixed = TRUE))) {
      # Taken argument by argument rather than with a single all.vars() call, so
      # that the indicator and the interval upper bound stay distinguishable.
      # Either may be a literal -- cens(indicator, 0.005) is a variable
      # indicator with a constant bound -- and a literal must drop out without
      # shifting the one after it into its slot.
      cens_args <- as.list(tmp_[[grep("cens(", tmp_, fixed = TRUE)[1]]])[-1]
      cens_args <- cens_args[seq_len(min(2, length(cens_args)))]
      c_vars <- vapply(cens_args, function(arg) {
        vars <- all.vars(arg)
        if (length(vars) > 0) vars[1] else NA_character_
      }, character(1))
      names(c_vars) <- c("cens_var", "cens_y2_var")[seq_along(c_vars)]
      c_vars <- c_vars[!is.na(c_vars)]
      # Keep the censoring variables as plain columns, not as the cens() call
      # the way trials() is kept: cens() takes two arguments, so model.frame()
      # has no single value to return for it.
      if (length(c_vars) > 0) {
        c_call <- str2lang(paste0(c_vars, collapse = " + "))
      }
    }
    if (any(grepl("rate(", tmp_, fixed = TRUE))) {
      # Kept as a plain column, as cens() is. Keeping the bare column name
      # avoids a clean_aterms() special case and lets find_transformations()
      # match it.
      ra_call_raw <- tmp_[[grep("rate(", tmp_, fixed = TRUE)[1]]]
      # Accept only a bare column. The denominator is kept as a plain column,
      # so an expression would be reduced to its first variable while
      # brms keeps the expression, and the priors, the prediction grid and the
      # plots would divide by a different quantity from the fit. Refuse it
      # here, where every route passes.
      if (length(ra_call_raw) > 1 && !is.symbol(ra_call_raw[[2]])) {
        # The call is deparsed whole rather than rebuilt around its argument.
        # check_formula()'s aterm pattern is unanchored, so a user-defined
        # myrate() reaches this branch, and a message rebuilt as "rate(...)"
        # would then name a call they did not write.
        stop("A rate() denominator must be a column of the data; you supplied",
             " ", deparse1(ra_call_raw), ". bayesnec carries the",
             " denominator as a plain column through the default priors, the",
             " prediction grid and the plotting paths, and an expression",
             " cannot be carried there. Compute the column first and name it",
             " in rate().", call. = FALSE)
      }
      ra_var <- all.vars(ra_call_raw)
      if (length(ra_var) > 0) {
        ra_var <- ra_var[1]
        names(ra_var) <- "rate_var"
        ra_call <- str2lang(ra_var)
      } else {
        ra_var <- character(0)
      }
    }
  }
  list(t_call = t_call, t_var = t_var, c_call = c_call, c_vars = c_vars,
       ra_call = ra_call, ra_var = ra_var)
}

#' The environment in which a formula's symbols are resolved
#'
#' @param formula A \code{\link[stats]{formula}}.
#'
#' @details A formula records the environment it was written in, and a symbol
#' the user put in it, such as a variable holding the model set or a function
#' transforming the predictor, must be looked up there. Evaluated without it,
#' the symbol resolves through the package namespace and then the global
#' environment, so it is found only at the console.
#'
#' @return An \code{\link[base]{environment}}.
#'
#' @noRd
formula_env <- function(formula) {
  env <- environment(formula)
  # Treat emptyenv() like NULL: nothing resolves through it, not even base.
  # Return the global environment for both.
  if (is.null(env) || identical(env, emptyenv())) globalenv() else env
}

#' An evaluation frame chained to a formula's own environment
#'
#' @param formula A \code{\link[stats]{formula}}.
#' @param ... Named objects to bind in the frame.
#'
#' @details A \code{\link{bayesnecformula}} names functions that
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}} defines internally and does
#' not export --- \code{crf} in the term itself, \code{trials} in the reduced
#' formula \code{\link{model.frame}} is built from. Evaluating in
#' \code{environment(formula)} alone therefore fails on the function, while
#' evaluating in a frame inside the namespace fails on the user's own symbols.
#' Binding the internal functions in a frame whose parent is the formula's
#' environment resolves both.
#'
#' @return An \code{\link[base]{environment}}.
#'
#' @noRd
formula_eval_env <- function(formula, ...) {
  # list2env() rather than a loop over names(), which binds nothing at all for
  # an argument passed positionally and defers the failure to a "could not find
  # function" a long way from the call. list2env() refuses an unnamed element.
  list2env(list(...), envir = new.env(parent = formula_env(formula)))
}

#' @noRd
crf <- function(x, model, arg_to_retrieve = "x") {
  mf <- match.call(expand.dots = FALSE)
  if (arg_to_retrieve == "x") {
    m <- match("x", names(mf), 0L)
    deparse(substitute(a, list(a = mf[[m]])))
  } else if (arg_to_retrieve == "model") {
    m <- match("model", names(mf), 0L)
    # Evaluate in parent.frame(), not crf()'s own frame, whose lexical chain
    # never reaches the caller. get_model_from_formula() supplies a calling
    # frame that inherits from the formula's environment.
    eval(mf[[m]], envir = parent.frame())
  } else {
    stop("arg_to_retrieve must be either \"x\" or \"model\".")
  }
}

#' @noRd
trials <- function(...) {
  identity(...)
}

#' Expose the final \code{\link[brms]{brmsformula}}
#'
#' Checks the input formula according to
#' \code{\link[bayesnec:bayesnec-package]{bayesnec}} requirements and
#' expose the final \code{\link[brms]{brmsformula}} which is to be fitted via
#' package \pkg{brms}.
#'
#' @param formula Either a \code{\link[base]{character}} string defining an
#' R formula or an actual \code{\link[stats]{formula}} object. See details.
#' @param data A \code{\link[base]{data.frame}} containing the variables
#' specified in \code{formula}.
#' @param family Optionally a \code{\link[stats]{family}} object. Only used to
#' detect hurdle families, which gain a second parameter block modelling the
#' hurdle probability. Defaults to \code{NULL}.
#'
#' @importFrom stats model.frame
#'
#' @return A named \code{\link[base]{list}}, with each element containing the
#' final \code{\link[brms]{brmsformula}} to be passed to
#' \code{\link[brms]{brm}}.
#' 
#' @seealso
#'   \code{\link{bayesnecformula}},
#'   \code{\link{check_formula}}
#'
#' @examples
#' library(bayesnec)
#' nec3param <- function(beta, nec, top, x) {
#'   top * exp(-exp(beta) * (x - nec) *
#'     ifelse(x - nec < 0, 0, 1))
#' }
#' 
#' data <- data.frame(x = seq(1, 20, length.out = 10), tr = 100, wght = c(1, 2),
#'                    group_1 = sample(c("a", "b"), 10, replace = TRUE),
#'                    group_2 = sample(c("c", "d"), 10, replace = TRUE))
#' data$y <- nec3param(beta = -0.2, nec = 4, top = 100, data$x)
#'
#' # make one single model
#' f_1 <- "y | trials(tr) ~ crf(sqrt(x), \"nec3param\")"
#' make_brmsformula(f_1, data)
#' # make an entire class of models
#' f_2 <- y ~ crf(x, "ecx") + ogl(group_1) + pgl(group_2)
#' make_brmsformula(f_2, data)
#'
#' @export
make_brmsformula <- function(formula, data, family = NULL) {
  # parent.frame() so that a character formula resolves symbols where
  # make_brmsformula() was called from rather than in its own frame.
  formula <- bnf(formula, env = parent.frame())
  # Check names before single_model_formula() rebuilds crf() from text;
  # a non-syntactic predictor would otherwise fail to parse.
  check_syntactic_names(formula)
  check_crf_formula_operator(formula)
  all_models <- get_model_from_formula(formula)
  out <- list()
  for (i in seq_along(all_models)) {
    formula_i <- single_model_formula(formula, all_models[i])
    bdat_i <- model.frame(formula_i, data = data, run_par_checks = FALSE)
    out[[i]] <- wrangle_model_formula(all_models[i], formula_i, bdat_i, family)
  }
  names(out) <- all_models
  out
}
