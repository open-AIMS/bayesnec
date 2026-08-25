
## Family choice reopened by RF, 2026-08-25

**RF questioned the Beta, and was right.** Day 4 is 49 zeros + 60 ones + 41
interior = **72.7% of observations on a boundary**, all nudged by 0.001. That is
not a boundary correction, it is most of the data.

**What the paper actually did** (`ignore/1-s2.0-S026974912300965X-mmc1.pdf`,
Statistics section, and the Fig. 1 caption): the proportional decline in tissue
survivorship was modelled with **Bernoulli or beta-binomial**, on **square-root
concentration**, using the **`decline` model set**, identity link, default
bayesnec priors — in **bayesnec 2.0.1**. For 1-MN specifically the main-text
caption says Bernoulli at T1 only and beta-binomial elsewhere, so **day 4 is
beta-binomial**.

A hurdle or zero-inflated Beta cannot rescue the Beta: the mass is at **both**
boundaries, which is the trap already documented for #175, and bayesnec has no
`zero_one_inflated_beta`. `beta_binomial` sidesteps it because 0 and 1 are legal
values there — nothing is nudged.

**RF's decisions:** switch to `beta_binomial`; adopt sqrt concentration and the
`decline` set to match the paper; and verify the 2.0.1 claim below before making
it.

**The finding that reopens the vignette's motivation.** The supplementary says
they tried *both* grouping structures on this dataset and abandoned both:

> 'Colony' was initially included as a random effect in the survivorship models;
> however, there were no suitable candidate model fits with the inclusion of
> colony, and it was therefore excluded. 'Chamber' was also used as a
> categorical random factor to accommodate for the non-independence of the 5
> pseudoreplicate coral fragments in each chamber. The random chamber effect was
> incorporated into the model; **however, the model failed to converge.**

Bounded family, identity link, group-level term, bayesnec 2.0.1 — that is the
#245 failure mode exactly. Worktree `/mnt/c/Rworking/wt-201` is checked out at
the `v2.0.1` tag to test whether it reproduces. **Do not state this as the cause
until that test has run**; if it reproduces it belongs in #245 and in the
vignette, and if it does not the claim is dropped.

**A trap caught in the smoke test.** bayesnec's convention for binomial families
is that `y` is the **count of successes** and `trials` the denominator —
`get_priors()` divides `y` by `trials` to recover the proportion. Passing a
proportion *and* `trials = 20` therefore produced a group-level `sd` prior of
`student_t(3, 0, 0.004969)`, twenty times too small, silently. The response must
be `as.integer(round(surv * 20))`, matching the package's own examples
(`nec_data$count`) and the lab convention recorded in the #175 notes.

## 2026-08-25: family switch, and a collision with a parallel session

**PR 250 has been reviewed and advanced by another session** while this one was
working. `issue-245-group-priors` is now at `6120b7b3`, several commits past my
`5bea5494`, including `0d4c3b20 address review of #250: binomial group inits,
prior_type, docs`. **Do not write to `/mnt/c/Rworking/wt-245`** — that session
was committing to it minutes ago. `/mnt/c/Rworking/wt-245check` is a detached
read-only checkout at `6120b7b3` for testing.

**A regression I introduced, found independently by both of us.** The
`gaussian()` query in `group_inits()` — the change that fixed `pgl` — breaks
every binomial family, because `trials()` is not a valid aterm for gaussian, so
`make_standata()` errors and no group inits are generated at all. Their fix
tries the real family first and falls back to `gaussian()`, and its comment
covers a case I had not considered: the `amend()` path, where `check_data()`
never runs. The warning added on 2026-08-24 is what surfaced this; a silent
empty return would have hidden it again.

**The isolation, against the reviewed head:**

| arm | max R-hat | divergences | NEC |
|---|---|---|---|
| `beta_binomial`, raw conc, ungrouped | 1.002 | **0** | 901 [661, 1015] |
| `beta_binomial`, **sqrt** conc, ungrouped | 1.006 | **0** | 30.8 (= 946 ug/L) |
| `beta_binomial`, raw conc, **+ ogl(chamber)** | **2.231** | **2000/2000** | 828 |
| `Beta`, sqrt conc, ungrouped | — | — | fails: "response greater than 0" |

**I was wrong about `sqrt`.** The default `nec` prior mean does sit outside the
transformed predictor's range (2.5 x median = 84.8 against a max of 61, because
`gamma(5, 2/median)` always has mean 2.5 x median and `sqrt` compresses the
upper tail), but arm B shows it fits cleanly regardless. The arithmetic is worth
recording as an observation; it is **not** the cause of anything seen here and
must not be reported as one.

**The real obstacle is `beta_binomial` + a group-level term.** Not an
initialisation failure any more — it starts and then every transition diverges.
The likely cause is structural rather than a defect: `beta_binomial` carries an
overdispersion parameter, and a chamber-level random intercept explains the same
within-chamber variance, so the two compete. `binomial` + a group-level term is
the standard overdispersed-binomial construction, where the random effect *is*
the dispersion; that is under test now. The paper's own GLMMs used "either a
binomial or beta-binomial".

**A separate defect, incidental but real:** arm D shows `Beta` with a
transformed predictor fails with "response greater than 0". `check_data()`
computes the boundary nudge, but `fit_bayesnec()` only writes it back when
`find_transformations()` is empty — so a formula like `crf(sqrt(conc), ...)`
sends the un-nudged response to `brm()`. Worth raising separately.
