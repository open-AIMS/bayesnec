# Handoff: where the "never floor" recommendation lives, and what backs it

Written 2026-08-11 for a separate session working on issue #193 (`example7`
rewrite as a growth-data case study using the `alga` dataset). Branch `dev` at
`374e511c`.

**Purpose.** Someone got confused about whether "never floor" is still the
project's position, where it is asserted, and whether OECD TG 201 actually says
what the vignette implies. This note answers all three and records a fresh check
of the guideline made on 2026-08-11. All TG 201 quotes below are verbatim from
the source PDF, so this note can be read without it.

Companion notes, read in this order: `oecd_flooring_scoping.md` (the original
handoff — states the question), `oecd_tg201_verification.md` (the first
verification, §0 lists what was adopted into the vignette), then this one (the
re-check plus two arguments neither of those contains).

---

## 1. Yes, "never floor" is the current recommendation

It is stated in **exactly one vignette and one help file**. Vignettes 1–5 do not
mention flooring at all.

| Location (`.orig` / rendered) | Content |
|---|---|
| `vignettes/example6.Rmd.orig:1117` / `example6.Rmd:1535` | Three-kinds-of-zero table: structural / **floored** / genuine |
| `.orig:1125–1140` / `.Rmd:1543–1560` | The three failures flooring causes in a hurdle fit, then the TG 201 paragraph |
| `.orig:1160–1185` / `.Rmd:1578–1603` | *"What 'distorts the error distribution' means"* — the density-vs-CDF table |
| `.orig:1214` / `.Rmd:1632` | **"Recommended handling — 1. Do not floor."** Items 2–4: censor by design, censor as salvage, ratio scale |
| `.orig:879` / `.Rmd:1177` | Capping at 1 is "the same operation as flooring… at the other end" |
| `.orig:503`, `680` / `.Rmd:696`, `917` | Passing references to the `nassarius` negatives being retained |
| `R/data.R:103` → `man/nassarius.Rd:36` | "retained rather than floored because flooring is the practice `vignette("example6")` argues against" |

Related but **not** the same argument: `example1.Rmd.orig:321–335` covers
censoring (bound vs. substituted value; `F(bound)` saturates while a substituted
density pulls the curve up). Same mechanism, different threshold, never uses the
word "flooring" and makes no recommendation about it.

## 2. What the recommendation rests on

Three independent legs, strongest first.

1. **The likelihood mechanism.** Flooring contributes `f(0 | µ(x), σ)` — asserts
   something false, and shrinks as the curve descends, so it exerts a persistent
   upward pull concentrated where decline is strongest. Censoring contributes
   `F(0 | µ(x), σ)` — true, and saturates. Pure statistics, needs no external
   authority, and holds in an ordinary regression with no hurdle block at all.
   This is the load-bearing argument.
2. **General literature.** `@helsel2006` (substitution fabricates data),
   `@blasco2019` / `@martin2005` (establish zero provenance before choosing a
   model), `@warton2005` (many zeros ≠ zero inflation). All in
   `vignettes/bayesnec.bib`.
3. **OECD TG 201 Annex 5.** Supporting, not load-bearing — see §4 for why.

---

## 3. TG 201 verbatim — the passages that matter

OECD (2026), *Test No. 201: Freshwater Alga and Cyanobacteria, Growth Inhibition
Test*, doi 10.1787/9789264069923-en. Declassified 15 June 2026. Local copy at
`ignore/9789264069923-en.pdf` (present in the `/mnt/c/Rworking/bayesnec` main
checkout; `ignore/` is gitignored so it is absent from worktrees). Bib key
`oecd2026tg201`. Paragraph numbers are TG 201's own.

**¶47–48 — two endpoints, both required, both real-line.**
> "a) Average specific growth rate: this response variable is calculated on the
> basis of the logarithmic increase of biomass during the test period […] b)
> Yield: this response variable is the biomass at the end of the test minus the
> starting biomass."

> "toxicity values calculated by using these two response variables are not
> comparable […] due to the mathematical basis of the respective approaches."

(That second sentence is the same point recommendation 4 makes about ECx on a
ratio versus ECx on an increment.)

**¶50 — the starting value.**
> "using the nominally inoculated biomass as the starting value rather than a
> measured starting value, because in this way greater precision is normally
> obtained."

So `µ < 0` requires final biomass genuinely below the inoculum.

**¶51, eq. [2] — inhibition is control-anchored.**
`%I_r = (µ_C − µ_T)/µ_C × 100`, where µ_C is the control mean. 100% inhibition
⟺ µ_T = 0.

**¶54 — the regression is on µ, not on inhibition.**
> "Plot the average specific growth rate, including that of control, against the
> logarithm of the test chemical concentration and fit a smooth line through the
> data points by regression models"

**¶55 — borrowed link functions.**
> "standard methods of analysis using probit, logit, or Weibull transforms are
> intended for use on quantal (e.g. mortality or survival) data and should be
> modified to accommodate growth or biomass data."

**¶56 — fit to replicates.**
> "Regression analysis should be performed using individual replicate responses,
> not treatment group means."

**¶62 — the stimulation tail.**
> "If growth stimulation is observed, both a standard monotone
> concentration-response model and an appropriate hormesis model (e.g.
> Brain-Cousens) should be fitted. The appropriate model should then be selected
> based on goodness-of-fit diagnostic criteria"

**Annex 5, General considerations — the error model, and the contrast case.**
> "The system is distributed or homogenous and the biomass can be viewed as a
> continuum without consideration of individual cells. The variance distribution
> of the type of response for a such system relate solely to experimental factors
> (described typically by the log-normal or normal distributions of error). This
> is by contrast to typical bioassay responses with quantal data for which the
> tolerance (typically binomially distributed) of individual organisms are often
> assumed to be the dominant variance component."

> "the normalized or relative response, r, decreases monotonically from 1 (zero
> inhibition) to 0 (100 per cent inhibition). Note, that all responses have an
> error associated and that apparent negative inhibitions can be calculated as a
> result of random error only."

**Annex 5, Models — the lower asymptote is fixed.**
> "the logistic equation, the nonsymmetrical Weibul equation and the log normal
> distribution function, which are all sigmoid curves asymptotically approaching
> zero for C → 0 and one for C → infinity."

**Annex 5, Procedure — the anti-substitution instruction.**
> "Negative inhibitions may be a problem with for instance the log-normal
> distribution function likewise demanding an alternative regression function.
> **It is not recommended to assign a zero or small positive value to such
> negative values because this distorts the error distribution.**"

**Annex 5, Normalizing responses — and its alternative.**
> "Dividing the responses by the mean control response for obtaining the
> percentage of inhibition, one introduces an additional error caused by the
> error on the control mean. Unless this error is negligibly small, weighting
> factors in the regression and confidence limits should be corrected for the
> covariance with the control (Draper and Smith, 1981)."

> "An alternative procedure is not to normalize the data and fit the absolute
> responses including the control response data but introducing the control
> response value as an additional parameter to be fitted by non linear
> regression. With a usual 2 parameter regression equation, this method
> necessitates the fitting of **3** parameters"

---

## 4. The caveat on the TG 201 citation

The anti-substitution sentence is written about **negative *inhibition*** —
growth *stimulation* at low concentrations, which ¶62 defines that way. It is the
**low-concentration tail**. TG 201 nowhere discusses negative µ or negative
yield.

The reason given ("distorts the error distribution") and the remedy ("an
alternative regression function") are both general and both apply to the decline
tail, but the sentence was written about the other one. The vignette states this
caveat honestly at `.orig:1158–1162`. **Do not restate the claim without it.**

Two further limits on transferring TG 201 to invertebrate growth:

* Its error model is explicitly the *non*-individual one (Annex 5 quote above);
  individually tracked organisms are TG 201's own named contrast case.
* An algal test has **no structural zeros**. ¶46 treats a zero cell count as a
  failed inoculation and removes it as a procedural outlier. The hurdle question
  `example6` exists to answer does not arise in TG 201 at all.

---

## 5. Question 1 — is ECx anchored between the control and zero growth?

**Yes, by construction — and it is not an artefact of the inhibition scale.**
But TG 201 never states it as a rule.

Evidence, strongest first:

* **Annex 5 Models**: every named function has its lower asymptote **fixed** at
  100% inhibition, i.e. µ_T = 0. Not fitted.
* **Annex 5 Normalizing responses**: the non-normalised alternative fits
  **3** parameters — slope, EC50, and a fitted control. **There is no `bot`.**
  So the bottom is zero on either route, normalised or absolute.
* **¶51 eq. [2]**: denominator is µ_C alone; 100% inhibition ⟺ µ_T = 0.
* **¶54 / ¶56**: the curve is fitted to µ itself and ECx read off it by inverse
  estimation. Neither paragraph defines what x% is a percentage *of* — the link
  to eq. [2] is via the name `ErCx`, not by an explicit statement.

**Why "it's inhibition, not growth" does not dissolve this.** Eq. [2] is an
affine rescaling of µ_T: `%I_r = 100 × (1 − µ_T/µ_C)`. Invertible, anchored at
µ_C and 0. "Inhibition runs 0→100%" and "the growth effect scale runs from
control down to zero growth" are the same statement in different units. And the
3-parameter absolute-scale route is on the growth scale directly and still has no
lower asymptote, so the anchoring survives leaving the inhibition scale entirely.

**Consequence worth stating in the vignette:** under TG 201's model set,
*absolute and relative ECx are the same number*, because the fitted bottom is
zero. They diverge only once a free `bot` is fitted, which TG 201 never does.
`bayesnec`'s default `type = "absolute"` is the one that stays in agreement.
`ecx_x_absolute()` (`R/ecx.R:321`) sets `range_y <- c(0, max(y))`, giving
`ecx_y = top × (1 − x/100)` — eq. [2] inverted, algebraically identical. This
confirms `notes/alga_dataset.md:143`.

**The honest claim.** *TG 201 defines inhibition relative to the control with
100% inhibition at zero growth (eq. 2, ¶51), and every regression function it
names has its lower asymptote fixed at zero (Annex 5) — including the
non-normalised variant, which fits three parameters, not four. The effect scale
is therefore anchored between control and zero growth by construction. TG 201
does not state this as a rule, because it never contemplates a growth rate below
zero.* Not "OECD says the EC50 must always be estimated between control and
zero."

---

## 6. Question 2 — how can a zero-bounded model fit a real-line endpoint?

This resolves an apparent contradiction and **is not in either earlier note.**

Two independent constraints get conflated:

| | what it bounds | violated when |
|---|---|---|
| **(a) mean function range** | where the fitted curve `µ(x)` can go | the *true mean* is negative |
| **(b) response support** | where an individual observation `y` can lie | *any single measurement* is negative |

**TG 201 imposes (a) only.** Its error model is normal or log-normal *about* the
curve (Annex 5), and ¶56 fits individual replicates. A replicate measured at
µ = −0.05 against a fitted mean of +0.02 is simply a **negative residual** —
retained, contributing a density, nothing substituted. That is precisely what
Annex 5's "do not assign a zero" is protecting. Keeping a negative observation
under a curve that asymptotes at zero is the intended behaviour, not a
contradiction.

**Why a zero asymptote is biologically right for algae.** Complete inhibition
means the culture stops growing: `X_j = X_i`, so `µ = 0`. Zero *is* the true
asymptote for an **algistatic** chemical. Given ¶50 (nominal inoculum as `X_i`),
`µ < 0` requires biomass below what was put in — cells lysed or died, i.e.
**algicidal**. For the common case `bot ≡ 0` describes the biology correctly.

**Where it breaks.** A genuinely algicidal chemical drives the *true mean* below
zero. A zero-asymptote curve is then misspecified: it sits above the data at high
dose, inflates σ, and biases the EC50. TG 201 has no provision for this.

**TG 201's own answer, from the mirror-image tail — the strongest new argument
here.** It has already faced this problem at the other end. Negative inhibition
puts data above `r = 1`, beyond what its sigmoids can reach. Both responses are
to change the *function*, never the data: Annex 5's "demanding an alternative
regression function", and ¶62's instruction to fit Brain-Cousens and select on
AIC. Run that same instruction at the decline tail and it yields a model with a
free lower asymptote that can cross zero — `nec4param` / `ecx4param`.

**So `example6` recommendation 1 is TG 201's own logic applied to the opposite
tail, not a departure from it.** That is materially stronger than the current
"the reason and the remedy are both general", and it is fully sourced. Worth
adding to the vignette.

---

## 7. Implications for `bayesnec` and for #193

**The two constraints must be kept apart for `nassarius`.** Its four negative
growth values are **noise**-negative — baseline-referencing error on a
destructive assay (`R/data.R:98–101`) — not evidence the mean goes below zero. So
(a) is not obviously binding. They cause trouble via **(b)**: Gamma's support
excludes negatives, so it rejects the *observations* regardless of where the
curve sits. Different constraint, different fix. The vignette currently blurs
these.

**`bayesnec` is stricter than TG 201.** `R/check_models.R:97` drops all of
`mod_groups$zero_bounded` for Gaussian data, so TG 201's own model shape —
zero-asymptote mean with normal error — cannot be fitted under a Gaussian family.
Every Gaussian fit gets a free `bot`. For genuinely algistatic data that is
over-parameterised: `bot` is estimated from data with no lower plateau, which is
exactly the badly-identified-`bot`-deforms-the-positive-region problem in
`oecd_tg201_verification.md` §5b. TG 201's fixed-zero bottom is immune to it.
**Whether a `bot`-fixed-at-zero option is worth offering is a real design
question, not just a documentation one.** Not currently an open issue.

**Cross-reference to #173.** TG 201's normalised route divides by the observed
control mean, and Annex 5 itself warns this introduces the control-mean error and
needs a covariance correction. Its own alternative — fit the control as a
parameter — is what `bayesnec` does. So #173 and the zero-anchoring argument
converge from opposite directions. Per `oecd_flooring_scoping.md` they stay
*separate arguments* in the prose, but a cross-reference is warranted.

---

## 8. Deliberately not adopted — do not resurrect without a fresh check

From `oecd_tg201_verification.md` §0:

* **§5a, the zero-anchored ECx argument, is out of the vignette.** §5 above is a
  re-verification of its central claim and it holds — but the decision to keep it
  out was made after an earlier misreading of OECD's position, and that decision
  has not been revisited. It is available for #193 if wanted; it is not currently
  vignette content.
* The `%I_r > 100` argument.
* `ecx(ecx_val = 0, type = "direct")` as a named reportable endpoint. (Caveat if
  it is ever used: `type = "direct"` is a grid search over `x_range`, so a curve
  that never reaches zero in range returns a boundary value rather than failing —
  issues #39 and #166.)
* Anything reading as criticism of TG 201's completeness.

**Settled by the ecotoxicologist:** a censored likelihood is acceptable in a
regulatory submission if it is statistically correct and yields a more robust
risk assessment.

**Still open** (`oecd_tg201_verification.md` §8): whether TG 210/211/202/231 say
anything explicit about negative increments — a better citation than the algal
test if one does; and whether growth-arrest vs. active-biomass-loss must be
distinguished, which decides how hard to push censoring.

---

## 9. Practical notes before editing

* **Prose-only vignette edits do not need a re-render.** `knitr` copies markdown
  verbatim, so apply the change to *both* `example6.Rmd.orig` and `example6.Rmd`
  in step. A full re-render is a ~4 hour, ~13 GB job (48 model fits) and has
  failed on memory more than once. Only edit `.orig` alone if a **code chunk**
  changed, in which case rebuild via `vignettes/precompile.R`.
* Changing *Recommended handling* is prose-only unless it introduces worked code.
* New references go in `vignettes/bayesnec.bib`.
* `cens()` does not pass through `bayesnecformula` yet (issue #181), so the
  censoring recommendations are guidance, not runnable today.
* Repo convention: PRs target `dev`, not `master`.
