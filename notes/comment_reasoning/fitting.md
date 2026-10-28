# Reasoning removed from the comments on initial values and parallel fitting

The comments in `R/inits_functions.R` and `R/parallel_models.R` were shortened
under #484. This file keeps the measurements and reasoning they held that no
pull request, issue or other `notes/` file records. Where the argument itself is
recorded elsewhere, the section names where and keeps only what is missing
there.

## `R/parallel_models.R`

### The level seed in `group_level_seeds()`

The level seed replaces the stream that `future.seed = TRUE` installs in a
worker. That stream is an L'Ecuyer-CMRG substream, chosen in
`bnec_parallel_lapply()` so that any body that draws random numbers is safe in
parallel. Seeding over it replaces a substream guaranteed not to overlap any
other with an ordinary Mersenne-Twister stream. That is safe for this body and
not in general: the only draws a level makes are the initial-value search and
the single `sample.int()` of `expand_manec()`, and the seeds themselves are
drawn from one stream in the parent, so two levels start from two draws of
`sample.int()` rather than from two points in one substream. Reproducibility
across the two arrangements was judged worth that. The rest of the seeding
argument is in #347 and #360.

### The strategies of a list plan in `plan_group_levels()`

A list whose remaining strategies are all sequential promises nothing, so it is
not read as a request. Dispatching the levels through `future` would add a
round trip to reach the `lapply()` they would have reached anyway, and the
message would name a strategy that does no more than the first. A parallel third
strategy is unreachable when the second is sequential. An earlier version
treated any later strategy as the next one, which both misstated the plan in
its message and dispatched a level for no gain.

### The chunk size in `bnec_parallel_lapply()`

One element per future sends the data, the formula and the priors once per
model rather than once per worker. For 23 models over four workers that is
about six times the transfer. Load balancing and holding one fit at a time were
judged the larger effects; no timing was taken either way (#318).

### The exported frame in `narrow_environment()`

Measured on the two-equation `manec_example` on 2026-09-11: the applied
function serialised to 14.1 MiB against 75 bytes for the arguments it reads,
and the same closure over a 32 MB object was refused at
`future.globals.maxSize = 10 MiB` with an error naming `FUN`, which tells the
user nothing about the cause. #318 records the 14.166 MiB to 0.017 MiB
reduction.

## `R/inits_functions.R`

The argument for the band, the spread and the per-chain acceptance is in #312,
and `notes/scripts/init_search_audit.R` reproduces its measurements. #336
records the stream reasoning for the seed (#310), and #406 the fallback counts.

### The link scale in `init_support()`

Clamping the linear predictor to the support of the mean excludes the region
the curve occupies under a non-identity link. On a
`beta_binomial(link = "logit")` response of proportions from 0.083 to 0.932,
whose logit-scale response runs from -2.40 to 2.62, an unmapped clamp gives a
band of exactly (0, 1) and so admits only means between 0.5 and 0.73. Any link
other than `log` or `logit` leaves the band unbounded, which was the released
behaviour and cannot reject a curve the likelihood would accept.

### The half-series estimate in `successive_difference_spread()`

A single number is the wrong summary where the dispersion changes across the
predictor, which is the ordinary case for a growth or count endpoint. On the
simulated cell that decided the width of `init_limits()`, an unreplicated design
with a fivefold rise in dispersion, the whole-series estimate is 2.35 against a
true standard deviation running from 1.2 to 4.0, and every failure to cover the
asymptotes was at the lower end of the response, where the noise is largest.
Taking the larger of the two half-series estimates covers that cell completely
at a width of four, where the whole-series estimate needed five. A standard
deviation of two differences estimates nothing, so each half needs at least
three before it is read on its own.

### The estimator in `group_spread()`

Two alternatives were measured and are not used. The largest single group's
standard deviation is the least robust: on a replicated design a single
aberrant observation widened the band by a factor of 6.7, against 4.1 for the
pooled spread and 3.0 for `range(y)`, because a maximum over groups follows
whichever group that observation lands in. The standard deviation of the whole
response is the most robust, at 1.6, but grows with the size of the effect,
giving a band 2.6 to 3.8 times the response range on the replicated designs
measured against 1.1 to 1.4 for the pooled one.

On the eight-concentration unreplicated design measured, the successive
differences give a spread of 0.068 against 0.395 for `sd(y)`, and a band 1.7
times the response range against 4.4.

On a four-concentration design of six replicates, replacing one control
observation by three times the control mean widened the band by a factor of
4.1, against 3.0 for `range(y)`. The wider band contains the narrower one, so
every starting value accepted before is still accepted, and the band's ends
shift by a sixth of the change rather than by the whole of it as `max(y)` does.

The branch is chosen on degrees of freedom because one duplicated predictor
value made the spread a function of that one pair. On a 24-point series,
duplicating one value with an equal response sent the pooled estimate to zero
and the band from 1.35 to 4.83 times the response range, and five draws of a
non-equal pair gave estimates spanning a factor of seven.

### The reference for the band in `init_limits()`

The band replaced `range(y)`, for three reasons. It compares an asymptote, an
estimate of a mean, with the largest single observation. It is tied to the
prior it filters: on a declining curve both `max(y)` and the location of the
`top` prior are read from the control. On the packaged `alga` `c_proliferum`
contaminant A series under the default `"uninformative"` priors, the `top`
prior is `normal(0.1284, 0.4044)` and the threshold `max(y)` is 0.1367, 0.021
prior standard deviations from the centre, and over 2,000 draws per equation
the clause passed 46.2 to 61.4 per cent of them for each of the fourteen
equations of the declining set. And an extremum drifts outward with sample size,
so the check grew more permissive as replicates were added.

The two `regularizing_location()` anchors are the statistic #307 adopted for the
`top` and `bot` priors of the `"regularizing"` set, so the search and that prior set read the
ends of the curve from the same statistic. The `"uninformative"` set, the
default, locates `top` and `bot` at quantiles of the pooled response and is
unchanged, so the shared definition covers one of the two prior sets. On a
replicated design the anchors lie inside the range of the group means; on an
unreplicated design there are no group means and the anchors are the whole band.

### The gap in `boundary_inset()`

The case that motivated the inset: on the second block of a hurdle fit, a curve
accepted at a survival of 1e-66 made the joint log likelihood `-Inf`.

The distance to the nearest level mean alone ties the gap to the level of the
response. On a Gamma design that stops short of its asymptote, with a lowest
level mean of 76.8, it gives a floor of 7.68 on a response that says nothing
about zero. The spread alone gives 0.024 on the second block of a hurdle fit,
against the 6e-4 the design resolves, which excludes the level means the block
was primed from. On that block the cap and `range(y)` are both 6e-3.

On a `zero_inflated_poisson` design of six concentrations by eight, the floor
is 0.0375 where `range(y)` gave 0, and 0.0275 at eighty replicates. Against a
generating `bot` of 0.3, the observation-based floor was 0.1 and the
observed-value floor before that 0.375, above the asymptote. Over 200 seeds of
that design the median floor is 0.025 at eight, eighty and four hundred
replicates; the interquartile range falls from [0.0125, 0.0375] to
[0.0223, 0.0255], and the share of seeds whose top group is entirely zero, which
puts a level mean on the boundary and disables the inset, falls from 15 per
cent to none.

Where the extreme concentration of an unreplicated count design returns zero,
the zero-bounded branch of `regularizing_location()` substitutes
`min(y[y > 0]) / 10` for the `bot` location, so the floor is
`min(y[y > 0]) / 100`. That tracks the data, where the earlier constant floor
of 0.1 did not, and it is the mechanism behind the one simulated cell no width
covers.

On the hurdle block the floor is 6e-4 and the draw that failed was 1e-66; a
draw at 1e-5 would be accepted and gives a finite log likelihood. On that block
the floor is the `eps` of `survival_by_x()`, `1/(2n)`, wherever a proportion is
exactly 0 or 1.

### The search in `make_good_inits()`

Accepting each chain on its own changes the set of accepted values only through
`refine_inits()`: the released loop did not refine the set it drew before
entering the loop, and this one refines from the first round. The difference
is of the order of the fourth power of the per-chain rate, between 4e-5 and
9e-3 on the designs measured. On the `alga` series above the per-chain rate
runs from 9 to 31 per cent.

The cap of 1e4 was kept after #266 proposed a lower one on the grounds that
Stan's own initialisation is available at the first attempt. On a twenty-row,
four-dose dataset `nec4param` needed 250 attempts to succeed at one seed and
more than 1000 at two others, while `nec3param` on the same data never
succeeded. A cap of 1e3 would have turned working fits into random
initialisation on exactly the small designs where good initial values matter
most. The released loop allowed `n_trials + 1` sets; the current one allows
exactly `n_trials` rounds.

### The prior rows in `add_level_defaults()`

Measured with `brms` 2.23.0: on `top ~ 0 + grp` with two levels, `brms`
generates `vector<lower=0,upper=1>[K_top] b_top` and
`lprior += beta_lpdf(b_top | 5, 2)`, so the bounds and the density of one
class `"b"` row reach both coefficients. Replicating the row per level would
state the same prior twice.
