# Verification: the OECD TG 201 claims in `vignette("example6")`

Checked against the guideline itself: OECD (2026), *Test No. 201: Freshwater
Alga and Cyanobacteria, Growth Inhibition Test*, OECD Guidelines for the Testing
of Chemicals, Section 2, OECD Publishing, Paris,
<https://doi.org/10.1787/9789264069923-en>. Approved and declassified 15 June
2026 (originally adopted 1984; revised 2006, corrected 2011). Local copy at
`ignore/9789264069923-en.pdf`.

Paragraph numbers below are TG 201's own numbering.

---

## 0. Decisions taken (2026-08-04) — read this first

The analysis below is complete, but **not all of it was adopted**. What went into
the vignette, and what was deliberately left out:

**Adopted.**

* The single, narrow, fully supported OECD claim: TG 201 Annex 5 advises against
  assigning a zero or small positive value **because it distorts the error
  distribution**, with the honest caveat that the sentence is written about
  negative *inhibition*. This is the real issue and censoring resolves it (§3).
* Both endpoints named — µ (¶49) and yield (¶53) — as endpoints that admit
  negative values (§2).
* The likelihood mechanism from §5b (density vs CDF; flooring's persistent upward
  force on the fit), as a new vignette subsection. This is now the load-bearing
  argument, replacing the appeal to regulatory authority.
* Censoring at zero promoted to a **primary design choice**, with the
  algistatic/algicidal cost stated (§5b). Also written up as a comment on issue
  \#181.
* The two-separate-indicator-columns discipline (§5b).

**Deliberately not adopted.**

* **The whole of §5a — the zero-anchored ECx argument — is out of the vignette.**
  Rationale: the prior understanding of OECD's position on lower asymptotes was a
  misreading, and after one misreading the project is not making further claims
  about what OECD implies regarding ECx conventions. §5a is retained here because
  the reading of eq. [2] is correct and may matter later, but it is not to be
  cited in the vignette without a fresh check.
* The `%I_r > 100` argument (§1 items c, e, f). Whether inhibition above 100% is
  reportable in practice varies, and censoring resolves the problem either way.
* `ecx(ecx_val = 0, type = "direct")` as a named reportable endpoint (§8 Q7) —
  out of scope for this vignette.
* Anything reading as a criticism of TG 201's completeness (§4, §5a closing).
  The vignette gives its own self-contained recommendations instead of arguing
  with the guideline.
* A literature search for other OECD guidelines addressing substitution (§8 Q1) —
  out of scope. TG 201 Annex 5 is the only source located.

**Settled by the ecotoxicologist.** A censored likelihood would be acceptable in
a regulatory submission if it is statistically correct and yields a more robust
risk assessment (§8 Q4, Q9).

---

## 1. Claim-by-claim verdict

The paragraph under review, verbatim from `vignettes/example6.Rmd.orig`:

> The regulatory standard does not floor. OECD TG 201 defines the growth endpoint
> as average specific growth rate, `µ = (ln X_j − ln X_i)/(t_j − t_i)`, which
> lives on the real line, and percent inhibition
> `%I_r = (µ_C − µ_T)/µ_C × 100`, which **exceeds 100% when `µ_T < 0`**. Net
> decline is expressed as more than complete inhibition rather than truncated
> away.

| # | Claim | Verdict |
|---|---|---|
| a | µ is defined as `(ln X_j − ln X_i)/(t_j − t_i)` | **Correct, verbatim.** Equation [1], ¶49. |
| b | It is *the* growth endpoint | **Incomplete.** TG 201 defines **two** response variables and requires both (¶47). |
| c | µ "lives on the real line" | **True of the formula; not asserted by TG 201.** The guideline never discusses µ < 0. |
| d | `%I_r = (µ_C − µ_T)/µ_C × 100` | **Correct, verbatim.** Equation [2], ¶51. |
| e | %I_r exceeds 100% when µ_T < 0 | **Arithmetically true; not stated in TG 201.** The guideline never mentions >100% inhibition. |
| f | "Net decline is expressed as more than complete inhibition rather than truncated away" | **Inference.** Follows from (d); TG 201 is silent on the case. |
| g | **"The regulatory standard does not floor"** | **Correct, and better supported than the vignette realises** — but by a passage the vignette does not cite, and one whose scope is the *opposite tail*. See §3. |

Nothing in the paragraph is wrong. The problem is that it reads as though TG 201
*says* these things, when (c), (e) and (f) are the vignette's own arithmetic, and
the one place TG 201 genuinely does address substitution is not cited at all.

---

## 2. What TG 201 actually provides

**Two endpoints, both required, both real-line.**

¶47: "This Guideline describes two response variables... In order for the test
results to be acceptable in all member countries, the effects should be evaluated
using **both** response variables."

* **(a) Average specific growth rate**, ¶49 eq. [1] — as quoted in the vignette.
  Note ¶50: the *nominally inoculated* biomass is normally used as `X_i`, so
  µ < 0 exactly when final biomass falls below the inoculum.
* **(b) Yield**, ¶53: "the biomass at the end of the test minus the starting
  biomass". This is a **raw increment**, and it is negative under the same
  condition.

This is useful to the vignette: recommendation 1 already offers "the TG 201
specific growth rate, or the raw increment" as the two scales to model on. Both
of those are TG 201 endpoints, and TG 201 mandates reporting both. That is a
stronger position than the vignette currently takes.

¶48 also states plainly that ErCx and EyCx "are not comparable" and will differ
"due to the mathematical basis of the respective approaches" — which is the same
point recommendation 3 makes about ECx on a ratio versus ECx on an increment.

**¶55 on borrowed link functions.** "standard methods of analysis using probit,
logit, or Weibull transforms are intended for use on quantal (e.g. mortality or
survival) data and should be modified to accommodate growth or biomass data."
Direct support for the vignette's broader point about not forcing growth data
into families built for bounded/quantal responses.

---

## 3. The passage the vignette should be citing — and its catch

**Annex 5, "Data analysis by nonlinear regression", under *Procedure*:**

> Negative inhibitions may be a problem with for instance the log-normal
> distribution function likewise demanding an alternative regression function.
> **It is not recommended to assign a zero or small positive value to such
> negative values because this distorts the error distribution.**

And in the same Annex, under *General considerations*:

> Note, that all responses have an error associated and that apparent negative
> inhibitions can be calculated as a result of random error only.

This is an explicit anti-substitution instruction *in the regulatory standard*,
with the vignette's own justification attached — it distorts the error
distribution. It also prescribes the vignette's remedy: **change the regression
function, not the data.**

**The catch.** "Negative inhibitions" means `%I < 0`, i.e. µ_T > µ_C, i.e. growth
*stimulation* at low concentrations — ¶62 defines the term that way ("Growth
stimulation (negative inhibition)"). It is the **low-concentration tail**, not
negative growth rates at the high-concentration tail.

So:

* The stated *reason* ("distorts the error distribution") is general and applies
  identically to flooring negative µ.
* The stated *remedy* (pick a function that admits the observed values) is
  general in the same way.
* But the sentence was written about a different tail, and **TG 201 nowhere
  addresses negative µ or negative yield at all.**

The honest version of the claim is: *TG 201 forbids substituting at a boundary,
gives the error-distribution reason, and prescribes changing the model rather
than the data — but it says this about the stimulation tail, and never discusses
net decline.* That is still a strong claim, and it is defensible. The current
wording, which implies TG 201 handled the decline case, is not.

---

## 4. Transferability to invertebrate growth — currently glossed

Two things in TG 201 argue *against* lifting it wholesale onto the `nassarius`
snail data.

**(i) TG 201's error model is explicitly the non-individual one.** Annex 5,
General considerations:

> The system is distributed or homogenous and the biomass can be viewed as a
> continuum without consideration of individual cells. The variance distribution
> of the type of response for a such system relate solely to experimental
> factors... This is by contrast to typical bioassay responses with quantal data
> for which the tolerance... of individual organisms are often assumed to be the
> dominant variance component.

Individually tracked organisms are the contrast case, by TG 201's own framing.

**(ii) TG 201 has no structural zeros.** ¶46: a zero algal concentration in one
of two or three replicates "may indicate the vessel was not inoculated correctly,
or was improperly cleaned" — it is treated as a procedural outlier and removed.
There is no death in an algal test, so the hurdle question this vignette is
entirely about **does not arise in TG 201**.

What transfers is the substitution principle, not a ready-made model. The
vignette should say so rather than leave the reader to assume TG 201 covers the
invertebrate case.

---

## 5. A practical question worth raising with the colleague

TG 201's validity criteria (¶12) require control biomass to increase **at least
16-fold in 72 h** (µ_C ≥ 0.92 day⁻¹). Given that and the ¶46 outlier rule, it is
worth asking how often µ_T < 0 is actually *observed and reported* in algal
tests. If in practice it essentially never survives to the analysis stage, then
"the regulatory standard does not floor" is true but is about a case the standard
rarely faces — and the real weight of the argument has to come from @helsel2006
and the general principle, not from TG 201's authority.

---

## 5a. The regulatory effect level — what actually resolves this

Added after discussion. The motivation for flooring is wanting a regulatorily
meaningful effect level; the objections to *not* flooring are (i) the fitted
lower asymptote is nearly always extrapolated and can be absurdly far away,
giving nonsense EC50s, and (ii) in risk-assessment terms the range of interest
runs from the control mean down to **zero**, because a concentration that flips
population growth from positive to negative is equivalent to complete mortality.

**TG 201 agrees with (ii), explicitly, and this is the strongest finding in this
document.** It does *not* recommend a free lower asymptote.

* Eq. [2], ¶51: `%I_r = (µ_C − µ_T)/µ_C × 100`. The denominator is µ_C alone.
  100% inhibition ⟺ µ_T = 0. The scale is anchored at zero **by construction** —
  the fitted lower asymptote appears nowhere in it.
* Annex 5, General considerations: "the normalized or relative response, r,
  decreases monotonically from 1 (zero inhibition) to 0 (100 per cent
  inhibition)."
* Annex 5, Models: the named functions — logistic, non-symmetric Weibull,
  log-normal — are "all sigmoid curves asymptotically approaching zero for C → 0
  and one for C → infinity", i.e. on the inhibition scale they bottom out at
  µ = 0.

So ErCx in TG 201 is the zero-anchored quantity. **The range-based ECx — x% of
the span between fitted upper and lower asymptote — is the departure from the
guideline, not the zero-anchored one.** The regulatory position needs no defence
against OECD; it *is* OECD's definition.

### This dissolves objection (i) without flooring

`bayesnec` already distinguishes the two definitions, and already defaults to the
right one:

* `ecx_x_absolute()` (`R/ecx.R:321`) sets `range_y <- c(0, max(y))`, so
  `ecx_y = top × (1 − x/100)`. That is eq. [2] inverted, algebraically identical.
  This is the **default**, `type = "absolute"`.
* `ecx_x_relative()` (`R/ecx.R:309`) uses `range(y)` = [fitted bottom, top].
  This is the one that produces the crazy EC50s.

So the lower asymptote can be extrapolated to wherever it likes: under
`type = "absolute"` it is a **shape parameter**, not part of the effect
definition. It lets the curve descend through the data correctly; it never enters
the ECx calculation. Objection (i) is an artefact of the *relative* convention,
not a consequence of refusing to floor.

The package already enforces the pairing. `R/ecx.R:154` errors with "Absolute ECx
values are not valid for a gaussian response variable unless a model with a bot
parameter is fit", and `R/check_models.R:97` drops the zero-bounded models for
Gaussian data. Fit `nec4param`/`ecx4param` on the real line, report absolute ECx
— which is exactly what recommendation 1 already says to do, without saying why.

### Objection (ii) is directly estimable

The concentration at which growth rate crosses zero is
`ecx(fit, ecx_val = 0, type = "direct")` — the 1–99 range check is skipped for
`type = "direct"` (`R/ecx.R:138`), so `ecx_val = 0` is admissible. No flooring, no
lower asymptote, no >100% inhibition needed. It is the concentration at which the
population stops growing, estimated from a model that was allowed to see the
decline.

For this vignette specifically that is worth more than it looks: in a joint
survival-and-growth fit, anchoring the growth endpoint at zero makes it
**commensurate with the survival endpoint**, which is anchored at zero survival.
The two components then measure effect on the same footing.

*Caveat:* `type = "direct"` is a grid search over `x_range` (`min_abs`), so if the
fitted curve never reaches zero within range it returns a boundary value rather
than failing. Same caveat as issues #39 and #166 on `ecx` resolution — worth
stating wherever this is recommended.

### The gap that remains, and the vignette's actual contribution

TG 201 is internally incomplete on precisely this case. Annex 5 says do not
substitute at a boundary — but every regression function it names bottoms out at
µ = 0 and therefore cannot represent µ < 0 either. The guideline forbids the fix
and does not supply an alternative.

The resolution is to separate two decisions that flooring conflates:

* **the likelihood** — which must admit the values actually measured, so the
  error distribution is not distorted (Annex 5's own reason); and
* **the effect definition** — which should be anchored at zero, because that is
  what is regulatorily meaningful and what eq. [2] specifies.

Flooring is an attempt to buy the second by corrupting the first. They are
independent: fit a model that can cross zero, report ECx on the zero-anchored
scale. That is the sentence the section should be built around.

### Cross-reference already in the vignette

`example6.Rmd.orig:909-912` already notes that `ecx(type = "absolute")` "measures
decline relative to the *fitted* control value and therefore propagates control
uncertainty through the posterior" — but it sits in the **normalisation** section
(@ritz2026, issue #173), disconnected from the flooring argument. TG 201 Annex 5's
"Normalizing responses" makes exactly that complaint about dividing by the control
mean (it "introduces an additional error caused by the error on the control mean",
needing a covariance correction, Draper and Smith 1981). Absolute ECx answers both
arguments. Per the scoping note these stay separate arguments — but a
cross-reference is warranted, since the same tool resolves both.

---

## 5b. Censoring at zero as a deliberate design choice

Proposed in discussion: since the shape of the curve below zero is not of
interest, censor there rather than floor — leaving the likelihood uncorrupted
while confining the fit to the positive-growth region that matters.

**This is right, and the reason is exact.** For an observation at concentration
`x`, with fitted mean `µ(x)`:

| treatment | likelihood contribution | what it asserts |
|---|---|---|
| floor to 0 | `f(0 \| µ(x), σ)` | the true value **is** 0 — false |
| censor at 0 | `F(0 \| µ(x), σ)` | the true value **is ≤ 0** — true |

The difference is not cosmetic. As `µ(x)` falls, `F(0 | µ(x), σ) → 1`, so the
censored contribution **saturates**: a curve descending further below zero is not
penalised, merely un-rewarded. Whereas `f(0 | µ(x), σ) → 0`, so flooring actively
penalises the curve for descending — it exerts a **persistent upward force on the
fit at exactly the concentrations where decline is strongest**. That is the
mechanism behind the vignette's existing point 2, stated in terms of the
likelihood rather than by analogy, and it is worth stating that way.

So censoring is strictly better than flooring: same intent, but the probability
statement it makes is true.

**Name it accurately, though.** Censoring normally describes the *observation
process* — "the true value exists but the instrument could not resolve it". Here
the value **was** resolved: µ = −0.3 day⁻¹ was measured. Censoring at zero is
therefore a deliberate **coarsening**, discarding information genuinely held. That
is defensible, but only when stated as what it is, with its justification:

> The model is intended to describe growth only above the point at which the
> population switches from increase to decline. Observations below that point are
> recorded as being below it, rather than deleted or moved.

Given §5a, that justification is substantive rather than one of convenience —
zero is where the endpoint changes regulatory meaning, not an arbitrary cut.

**What it buys, given §5a.** The "crazy EC50" motivation is already answered by
absolute ECx, so censoring must earn its place on a different argument. It does:
`bot` is a single parameter shared across the whole curve, and when the data
provide no lower plateau it is identified by the prior and by its correlation
with slope and EC50 — which **deforms the fitted curve in the positive region**.
Censoring converts `bot` from a badly extrapolated point estimate into a weakly
identified nuisance with a saturating likelihood. Absolute ECx then makes it
irrelevant to the reported quantity. The two fixes address different failures and
compose:

* **A** (current rec 1): 4-parameter model on the real line, absolute ECx.
* **B** (this proposal): censor at zero.
* **C** = A + B — recommended. Censoring keeps the extrapolated tail from
  deforming the region of interest; absolute ECx keeps `bot` out of the effect
  estimate. Put a weakly informative prior on `bot`, since a saturating
  likelihood plus a flat prior invites sampling pathology.

Note that C is internally consistent in a satisfying way: the model's coarsening
point, the effect scale's anchor, and the regulatory transition are all zero.

**What it costs.** Three things to be honest about.

1. **The decline carries real signal, and censoring degrades it.** Censored
   observations still inform `µ(x)` through `F(0 | µ(x), σ)` — the *proportion*
   below zero at each concentration is informative — but that is close to quantal
   information. Once nearly all replicates at a concentration are below zero,
   µ = −0.1 and µ = −2 are nearly indistinguishable. **Algistatic and algicidal
   chemicals become hard to tell apart.** If that distinction matters for the
   assessment, censoring is the wrong choice.
2. **σ identification** degrades as the censored fraction rises, and ECx depends
   on the curve, so it propagates. Conversely, if variance genuinely shrinks in
   the decline region (common — everything is dying), censoring *protects*
   against that heteroscedasticity rather than exposing the fit to it. Which way
   this cuts is an empirical question per dataset.
3. **It re-opens the ambiguity this vignette exists to close.** A censored zero
   and a structural zero look identical once written to file. The discipline
   already stated for the hurdle applies unchanged and must be stated again here:
   **the censoring indicator and the failure indicator are different columns and
   mean different things.** A dead animal is a structural zero and belongs to the
   hurdle; a living animal that shrank is a censored observation and belongs to
   the response block. Collapsing them is flooring by another route.

**Availability.** Not currently possible in `bayesnec`. `bayesnecformula`
validates only the `trials()` and `weights()` aterms, so `y | cens(...)` does not
pass through — the vignette already says this at `example6.Rmd.orig:961-964`, and
it is issue #181. That issue is framed around censoring at the *recording
resolution*; this proposal is censoring at *zero* on a real-line endpoint. They
are different thresholds with different justifications, but the same plumbing
serves both, which strengthens the case for #181.

**Effect on the list.** This promotes recommendation 2 from a salvage operation
("if the values are already floored and unrecoverable") to a **primary design
choice** — and the two uses should be distinguished, because they are not the
same act. Recovering unusable floored data is a repair. Censoring data you hold
in full is a modelling decision requiring the justification above.

---

## 6. Proposed replacement prose

Drop-in for the paragraph at `vignettes/example6.Rmd.orig:1033-1038` (and the
identical text at `vignettes/example6.Rmd:1358`). Prose-only, so no re-render
needed.

> The regulatory standard says not to floor, and says why. OECD TG 201
> [@oecd2026tg201] defines two growth endpoints and requires both to be reported
> (¶47): average specific growth rate,
> `µ = (ln X_j − ln X_i)/(t_j − t_i)` (eq. 1, ¶49), and yield, the biomass at the
> end of the test minus the biomass at the start (¶53) — a rate and a raw
> increment. Both are negative when the culture ends below its inoculum. Percent
> inhibition is taken relative to the control,
> `%I_r = (µ_C − µ_T)/µ_C × 100` (eq. 2, ¶51), so net decline is expressed as
> more than complete inhibition rather than truncated away. On what to do with
> values that fall outside the expected range, Annex 5 is explicit:
>
> > It is not recommended to assign a zero or small positive value to such
> > negative values because this distorts the error distribution.
>
> That sentence is written about negative *inhibition* — growth stimulation at
> low concentrations, the opposite tail — and TG 201 nowhere discusses negative
> `µ` directly. But the reason given is general, and it is the argument made
> here. So is the remedy: TG 201's instruction is to choose a regression function
> that admits the values observed, not to move the values.
>
> Two limits on carrying this across. TG 201 rests its case on a system where
> "the biomass can be viewed as a continuum without consideration of individual
> cells", explicitly contrasted with responses where the tolerance of individual
> organisms dominates the variance (Annex 5). Growth of individually tracked
> animals is the second case. And an algal test has no structural zeros to begin
> with — a zero cell count is treated as a failed inoculation and removed as an
> outlier (¶46) — so the hurdle question does not arise there at all. What
> transfers is the principle about substitution, not a ready-made model.

Consider closing that paragraph with a forward reference, so the reader is not
left thinking that refusing to floor commits them to a range-based ECx:

> None of this forces the effect scale to follow the fitted curve down. TG 201
> anchors inhibition at zero, not at the lower asymptote (eq. 2, ¶51), and so
> does `bayesnec` by default — see *Recommended handling* below.

Requires a new `bayesnec.bib` entry (there is currently no OECD entry):

```bibtex
@techreport{oecd2026tg201,
  author      = {{OECD}},
  title       = {Test No. 201: Freshwater Alga and Cyanobacteria, Growth
                 Inhibition Test},
  series      = {OECD Guidelines for the Testing of Chemicals, Section 2},
  institution = {OECD Publishing},
  address     = {Paris},
  year        = {2026},
  doi         = {10.1787/9789264069923-en}
}
```

---

## 7. Proposed changes to **Recommended handling**

Current list is sound but **incomplete in the way that matters**: it never says
how ECx is defined, which is the whole reason anyone floors in the first place.
Four changes, all prose-only.

**New item 0 — or fold into item 1: report ECx on the zero-anchored scale.**
This is the missing piece and should probably lead the list, because it is the
answer to the motivation rather than to the symptom. `ecx(type = "absolute")`
(the `bayesnec` default) measures decline from the fitted control to **zero**,
which is eq. [2] of TG 201 exactly. `type = "relative"` measures decline across
the fitted span and is what makes an extrapolated lower asymptote catastrophic.
Under the absolute definition, `bot` is a shape parameter that lets the curve
descend correctly and never enters the effect estimate. Add the zero-crossing
concentration — `ecx(ecx_val = 0, type = "direct")` — as the growth analogue of
complete mortality, with the resolution caveat from §5a. See §5a for the full
argument.

**1. Do not floor.** Say that *both* named scales are TG 201 endpoints — µ (¶49)
and yield/increment (¶53) — rather than presenting the increment as an
alternative to the guideline. Optionally add ¶55: probit/logit/Weibull transforms
"are intended for use on quantal... data and should be modified to accommodate
growth or biomass data", which is the guideline making the vignette's own point
about borrowed link functions. The lower-asymptote model should be justified as
serving the **likelihood** (so the error distribution is not distorted), and
explicitly *not* as supplying the ECx reference range — otherwise the reader
reasonably concludes that not flooring commits them to range-based ECx, which is
the objection that motivates flooring.

**2. Left-censored (Tobit) — split into two, and promote one of them.** See §5b.
As written this is a salvage step for data already floored beyond recovery. It
should also appear as a **primary option**: censor at zero deliberately, when the
shape of the curve below zero is not of interest, combined with a real-line model
and absolute ECx (option C in §5b). The two uses need distinguishing — one is a
repair, the other a modelling decision that requires its own justification and
carries its own costs (loss of algistatic/algicidal discrimination especially).
Flag that it is not yet available: `bayesnecformula` does not pass `cens()`
through, issue #181. No TG 201 support in either direction; the guideline never
contemplates the case, so this must not gain regulatory framing by proximity to
the paragraph above.

**3. Strictly positive scale.** This one can be strengthened. The ratio
`X_j / X_i` is strictly positive by construction, and `log(X_j / X_i) = µ (t_j −
t_i)`. So the ratio scale *is* TG 201's growth rate, exponentiated — modelling
relative to each unit's own baseline is not a departure from the guideline but
the same quantity on a different scale. That also sharpens the existing warning:
ECx is not invariant under that transform, which is exactly ¶48's point that ErCx
and EyCx "are not comparable... due to the mathematical basis of the respective
approaches."

One thing to keep separate: TG 201 Annex 5 has a "Normalizing responses" section
warning that dividing by the **control mean** introduces the error on the control
mean and needs a covariance correction. That is the @ritz2026 / issue #173
argument, and recommendation 3 normalises to each unit's **own** baseline, which
is a different operation. Per the scoping note these two arguments stay separate.

**Framing for the whole list.** Flooring conflates two independent decisions —
what the likelihood must admit, and where the effect scale is anchored. It
sacrifices the first to buy the second, and it does not need to, because the
second is a reporting choice. Saying that once, up front, makes the rest of the
list follow.

---

## 8. Questions for the ecotoxicology colleague

1. Is TG 201 Annex 5 the only place OECD addresses substituting at a boundary, or
   do the invertebrate/fish growth guidelines (TG 210, 211, 202, 231…) say
   anything explicit about negative increments? If one of them does, it would be
   a far better citation for this vignette than the algal test.
2. In practice, do submissions ever report `%I_r > 100`, or do reporting
   templates and downstream tools cap inhibition at 100%? The claim in §1(e)
   depends on that being expressible in practice, not just arithmetically.
3. Does µ_T < 0 actually survive to analysis in real algal tests, given ¶12 and
   ¶46 — or is it rare enough that TG 201's silence is simply lack of occasion?
   (See §5.)
4. Is a left-censored likelihood (recommendation 2) acceptable in a regulatory
   submission, or does it sit outside what regulators will take?
5. Is "the regulatory standard does not floor" a fair headline given §3 — or
   should the vignette make the narrower, fully supported claim that TG 201
   forbids substitution at a boundary and gives the error-distribution reason,
   without asserting it addressed net decline?
6. **The one to lead with (§5a).** Does the colleague agree that the "crazy
   EC50" problem comes from the *range-based* ECx convention rather than from
   admitting negative growth — and that TG 201's eq. [2] is unambiguously
   zero-anchored, so the regulatory position needs no defence against OECD? If
   so, the case for flooring largely collapses: it was buying a reporting
   property at the cost of the likelihood, and the reporting property was
   available for free.
7. Would `ecx(ecx_val = 0, type = "direct")` — the concentration at which growth
   rate crosses zero — be accepted as a reportable endpoint, given it is the
   growth analogue of complete mortality? Does it need a name?
8. On §5b: does the assessment ever need to distinguish **algistatic from
   algicidal** (or the invertebrate equivalent — growth arrest versus active
   loss of biomass)? Censoring at zero largely gives that distinction up. If it
   matters, option A is the right recommendation; if it does not, option C is.
   This is the question that decides how strongly the vignette should push
   censoring.
9. Would a regulator accept a censored likelihood in a submission at all, or is
   it research-grade only?
