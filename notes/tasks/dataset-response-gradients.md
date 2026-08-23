# Which packaged datasets can carry a fitted curve

Written 2026-08-23, after two vignettes failed to fit because their example data
had no usable response gradient. Both were chosen on design tidiness — balanced
replication, clean factor structure — without checking whether the *response*
traversed its range. It does not, in either case.

## The measure

A concentration-response curve needs observations away from the extremes. The
"mid-band" below counts values in the middle 60% of the observed response range.
A response sitting at its floor or ceiling carries no information about curve
shape, whatever the design looks like.

| dataset | n | range | mid-band | distinct x |
|---|---|---|---|---|
| `nec_data` (simulated) | 100 | 0.001–0.98 | 23 (23%) | 100 |
| `herbicide` all seven | 580 | 0–0.73 | 203 (35%) | 9 |
|   `herbicide` atrazine | 84 | 0.04–0.67 | 31 (**37%**) | 7 |
|   `herbicide` simazine | 72 | **0.16–0.71** | 26 (**36%**) | 6 |
|   `herbicide` diuron | 72 | 0.02–0.72 | 24 (33%) | 6 |
|   `herbicide` hexazinone | 76 | 0–0.72 | 24 (32%) | 7 |
|   `herbicide` irgarol | 72 | 0–0.70 | 18 (25%) | 6 |
|   `herbicide` ametryn | 96 | 0–0.70 | 24 (25%) | 8 |
|   `herbicide` tebuthiuron | 108 | 0.04–0.73 | 20 (19%) | 9 |
| `alga` r_salina / B | 70 | −1.15–1.03 | 28 (**40%**) | 13 |
| `alga` r_salina / A | 85 | −1.99–1.49 | 19 (22%) | 14 |
| `alga` c_proliferum / B | 70 | −0.31–0.18 | 12 (17%) | 13 |
| `alga` c_proliferum / A | 85 | −0.58–0.14 | 7 (8%) | 14 |
| `nassarius` survival B | 43 | 0–1 | 3 (7%) | 14 |
| `nassarius` survival A | 34 | 0–1 | 2 (6%) | 11 |
| `nassarius` survival D | 62 | 0–1 | 3 (5%) | 15 |
| `nassarius` survival C | 58 | 0–1 | **0 (0%)** | 14 |

## What follows

**`nassarius` survival is unusable for a fitted curve.** Every contaminant is
effectively a step function; contaminant C has *no* tank between 0 and 1. A
`nec3param` fit on it succeeds at 2 chains and fails at 3, because a badly
initialised chain has nothing to pull it back and `are_chains_correct()` then
rejects the whole fit. This is not a timeout — `bnec()`'s `timeout` defaults to
`Inf`.

This matters because survival looked like the *fix* for a different problem:
modelling `nassarius` growth requires filtering to survivors, which conditions on
the outcome. Survival avoids that and lands somewhere worse. The dataset's
growth response, fitted through `bnec_hurdle()` or `hurdle_gamma`, is the
analysis it exists for.

**`herbicide` is the best-graded set in the package**, but the levels differ
more than the summary suggests. `simazine` is the safest single choice: the
widest mid-band *and* it never reaches 0, so a `Beta` fit has no floor to
contend with. `irgarol` — the one picked by default because it is listed first —
is among the weakest, and its response collapses to ~0 across the top three
concentrations, with 8 of 12 replicates at exactly zero at the highest.

**`alga` r_salina/B is the single best-graded response**, but `example7` (#193)
owns that dataset.

## The rule

Check the response before the design. A balanced, replicated, nested design over
a saturated response still cannot identify a curve — and the failure appears as
"Failed to fit model", which points at the model rather than at the data.
