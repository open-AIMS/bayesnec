# Vignette fit comparison for PR #537

The narrower regularising `d` and `f` priors leave the fitted results in
Examples 1, 3, 5, and 6 unchanged. They can change which equations fit and
pass screening in the model sets of Examples 2, 4, 7, 8, and 9. The largest
confirmed shift in the screened results is a 28% increase in the pooled-plate
N(S)EC in Example 8. This assessment covers the first three Example 8 calls;
the later calls were excluded when the remaining HPC jobs were cancelled.

## Comparison

Each fit uses the same data, seed, model specification, and sampler settings in
three arms: the default uninformative prior, the original regularising prior,
and the pull request's new regularising prior. The pull request changes the
regularising prior for the `d` and `f` parameters from `normal(0, 5)` to
`normal(0, 1)`. It does not change the uninformative prior or the other
regularising parameter priors. Extracted estimates and diagnostic tables are
in the per-example directories beside this report. The fitted RDS objects
remain in the ignored local analysis directory and on HPC scratch storage.

The table describes the change from the **original** to the **new**
regularising prior. Baseline differences between the default and original
regularising arms are given below where they clarify the three-way comparison.
The `NE` column of the machine-readable files means NEC for NEC models and
NSEC for ECx models. Reported intervals are 95% posterior credible intervals.

| Vignette | Fitted examples compared | Original to new regularising result |
| --- | ---: | --- |
| Example 1 | 10 | Identical posterior draws and estimates. None of these fits uses the changed `d` or `f` prior. |
| Example 2 | 1, with 15 decline equations | A new `ecxll5` candidate fits but fails the vignette's R-hat screen. Screened NEC/NSEC: 3.303 to 3.319; EC10: 3.617 to 3.613; EC50: 5.094 to 5.094. |
| Example 2b | 0 | No fitted example in the current source. |
| Example 3 | 9 | Identical posterior draws and estimates. The amended `necsigm` fit supplies its own `normal(0, 2)` prior for `d`; the other fitted models do not use the changed prior. |
| Example 4 | 4 calls, including seven-level herbicide fits | Separate herbicide fits change screened N(S)EC medians by at most 2.3% and screened EC50 medians by less than 0.2%. In the joint refit, ametryn changes 0.1848 to 0.1972 (+6.7%) and irgarol 0.1419 to 0.1342 (−5.4%). The two coral group fits are identical across the regularising arms. |
| Example 5 | 1 fit shown with `eval = FALSE` in the source | Identical posterior draws and NEC estimate; this `nec4param` model has no `d` or `f`. |
| Example 6 | 4 hurdle/joint calls | Identical posterior draws and estimates; the changed prior does not enter these models. |
| Example 7 | 24 case studies and 42 paired simulation exemplars | Across cases, maximum absolute median changes are 3.5% for ErC10, 0.2% for ErC50, and 3.8% for NSEC. Across paired exemplars, they are 8.8%, 0.5%, and 26.1%, respectively. See limits below. |
| Example 8 | First 3 of 8 calls compared across all arms | The screened pooled-plate N(S)EC changes 0.1616 to 0.2068 (+28%), with the new `ecxll5` equation receiving weight 0.534. The concentration-group example changes 0.2597 to 0.2702 (+4.1%). Later plate and toxicant calls are outside this assessment. |
| Example 9 | 6 | The largest screened EC10 change is 0.0870 to 0.0847 (−2.6%) for the constant-dispersion fit. Its NSEC changes 0.1043 to 0.1028 (−1.4%). The full herbicide fit's screened EC10 and EC50 decrease by 0.3% and 0.1%. |

Selected medians show all three arms directly. All values in this table use
the vignette's diagnostic screen where it applies.

| Example and endpoint | Default | Original regularising | New regularising |
| --- | ---: | ---: | ---: |
| 2, decline-set N(S)EC | 3.131 | 3.303 | 3.319 |
| 4, joint ametryn N(S)EC | 0.1839 | 0.1848 | 0.1972 |
| 4, joint irgarol N(S)EC | 0.1403 | 0.1419 | 0.1342 |
| 7, `d2` measured-response exemplar ErC10 | 2.236 | 2.367 | 2.160 |
| 7, `d2` measured-response exemplar NSEC | 1.653 | 1.794 | 1.326 |
| 8, pooled-plate N(S)EC | 0.1624 | 0.1616 | 0.2068 |
| 9, constant-dispersion EC10 | 0.0893 | 0.0870 | 0.0847 |

## Changes in candidate models and model weights

The narrower `d` and `f` priors sometimes allow an `ecxll5` equation that failed
under the original regularising prior to fit and pass the vignette's diagnostic
screen. Its new model weight then changes a model-averaged estimate. For
Example 8's pooled plate fit, `ecxll5` has zero divergent transitions and an
R-hat of approximately 1.002 under the new prior, and receives 0.534 of the
screened model weight. Under the original prior it did not initialise. The
resulting N(S)EC intervals are 0.0368–0.5666 for the original prior and
0.0533–0.5056 for the new prior; they overlap.

In Example 4, the new prior retains `ecxll5` for ametryn and hexazinone in the
separate herbicide fits. The joint refit selects `ecxll5` rather than `ecxll4`
for ametryn and irgarol. The posterior probability that diuron has a larger
N(S)EC than irgarol changes from 0.548 to 0.746 between the regularising arms.
The herbicide EC50 ranking is unchanged. All seven separate and joint
herbicide N(S)EC intervals overlap between the two regularising arms.

In Example 7, `ecxll5` is present in 6 of 24 case-study fits under the original
regularising prior and 15 under the new prior. In the 42 paired simulation
exemplars, it is present in 9 versus 34. The largest exemplar change is for
the `d2` measured-response convention: ErC10 falls from 2.367 to 2.160 and
NSEC from 1.794 to 1.326. The true NSEC for this simulation cell is 1.3.
The old and new intervals overlap for every reported case-study and exemplar
endpoint.

Example 2's new `ecxll5` fit has a prescreen weight of 0.134, but its maximum
R-hat is 1.01045, just above the vignette's 1.01 cutoff. It is therefore
excluded from the screened estimates in the table. Example 9's constant-
dispersion fit gains a screened `ecxll5` candidate with weight 0.192. For its
full herbicide fit, the new `ecxll5` candidate has 172 divergences and is
excluded by the screen.

## Sampler diagnostics

I applied `check_sampling()` to every saved fit in Examples 1–6, 8, and 9.
Its default limits are maximum R-hat 1.01, minimum bulk or tail ESS 400, and
at most 10 post-warmup divergent transitions. The complete per-candidate
results are in [diagnostics.csv](diagnostics.csv), with counts by vignette and
arm in [diagnostics_summary.csv](diagnostics_summary.csv). Counts include
candidate entries repeated in amended or refitted objects; they are not counts
of independent Stan runs. Example 8 counts cover its first three compared
calls.

| Example | Default candidates failing | Original regularising candidates failing | New regularising candidates failing |
| --- | ---: | ---: | ---: |
| 1 | 1 of 10 | 1 of 10 | 1 of 10 |
| 2 | 3 of 14 | 3 of 14 | 4 of 15 |
| 3 | 6 of 13 | 6 of 13 | 6 of 13 |
| 4 | 5 of 108 | 4 of 107 | 4 of 109 |
| 5 | 0 of 1 | 0 of 1 | 0 of 1 |
| 6 | 3 of 17 | 5 of 17 | 5 of 17 |
| 8, first three calls | 11 of 45 | 7 of 42 | 8 of 45 |
| 9 | 17 of 57 | 13 of 58 | 14 of 60 |

The diagnostic changes behind the largest result changes are more specific
than these totals:

| Candidate | Original regularising | New regularising |
| --- | --- | --- |
| Example 4 ametryn `ecxll5` | R-hat 1.002, ESS 1,247, 18 divergences; excluded | R-hat 1.003, ESS 2,008, 0 divergences; retained |
| Example 8 pooled-plate `ecxll5` | Failed to initialise | R-hat 1.002, ESS 1,497, 0 divergences; retained |
| Example 9 constant-dispersion `ecxll5` | R-hat 1.003, ESS 814, 840 divergences; excluded | R-hat 1.002, ESS 2,193, 5 divergences; retained |

Several diagnostics are poor under both regularising versions and are
therefore independent of PR #537. Example 3's amended `necsigm` candidate has
R-hat 2.593, minimum ESS 5, and 104 divergences in both arms. Example 6's
second hurdle fit has the same five failing survival candidates in both arms;
one has R-hat 1.148, minimum ESS 21, and 2,794 divergences. These fits warrant
caution even though their original-to-new posterior draws are identical.

The Example 7 compendium saved maximum R-hat and minimum tail ESS for every
candidate, available in [Example 7 diagnostics](example7/diagnostics.csv),
but did not save divergence counts for every fit. The four full new-prior fits
retained for targeted checking have `ecxll5` R-hat at most 1.0034, minimum
bulk or tail ESS at least 1,500, and zero divergences. Those include the
`d2` measured, `p3` censored, and `p4` censored exemplars with the largest
reported changes. Example 7's case-study model averages include some
candidates with tail ESS below 400 under both regularising versions; the
current source does not apply `screen_models()` to those compendium fits.

### Divergence comparison

For the **260 candidate entries fitted in all three arms**, mean post-warmup
divergences per entry decrease from **86.8** under default to **79.3** under
the original regularising prior and **73.1** under the new one. The totals are
22,569, 20,609, and 19,006. The median is zero in every arm. These are paired
descriptive counts, not an estimate of a common divergence rate across the
different datasets and sampler settings. Amended and refitted objects can
repeat the same Stan fit in this count. The exact per-vignette totals and
means are in [paired_divergence_summary.csv](paired_divergence_summary.csv).

| Example | Paired candidate entries | Default total | Original regularising total | New regularising total |
| --- | ---: | ---: | ---: | ---: |
| 1 | 10 | 15 | 14 | 14 |
| 2 | 14 | 0 | 0 | 0 |
| 3 | 13 | 7,465 | 7,465 | 7,465 |
| 4 | 107 | 95 | 31 | 13 |
| 5 | 1 | 0 | 0 | 0 |
| 6 | 17 | 3,229 | 3,150 | 3,150 |
| 8, first three calls | 42 | 409 | 478 | 478 |
| 9 | 56 | 11,356 | 9,471 | 7,886 |

The default-to-original comparison has 23 entries with fewer divergences,
14 with more, and 223 unchanged. Across the **262 entries present in both
regularising arms**, four have fewer divergences under the new prior, none
have more, and 258 are unchanged. The four changes are ametryn `ecxll5`
(18 to 0), Nassarius `ecxsigm` (2 to 0), Nassarius `ecxll5` (5,076 to 3,493),
and the lum31 constant-dispersion `ecxll5` (840 to 5). The Nassarius
`ecxll5` fit still fails the screen. Eight further candidates fit only under
the new regularising prior; five fail the sampler screen and three pass. This
explains why a larger raw count of failing candidates can coexist with fewer
divergences in the matched candidates.

The figures show the paired totals, the maximum count for each model within
each vignette, and the four candidates that changed between the regularising
arms:

- [Paired divergence totals](plots/paired-divergence-totals.pdf)
- [Model counts, Examples 1–4](plots/divergences-by-model-1to4.pdf) and [Examples 5–9](plots/divergences-by-model-5to9.pdf)
- [Changed candidates](plots/changed-candidate-divergences.pdf)

The [per-model data](divergences_by_model.csv) and [per-candidate data](diagnostics.csv)
give the counts behind those figures. Example 7 is absent from the divergence
figures because its compendium did not retain divergence counts for every
case-study and simulation fit. The later Example 8 calls are also excluded.

## Default versus regularising baseline

The default arm is unaffected by PR #537 but can differ from either
regularising arm. Among the examples with no original-to-new change, the
largest absolute default-to-original relative shift in an extracted NE median
is 7.5% in Example 1, 0.3% in Example 3, and 8.7% in Example 6. Example 4's
2018 coral-colour NEC is 8.116 under default versus 5.877 under either
regularising arm. Example 9's power-dispersion raw NSEC median is 0.0550 under
default versus 0.0455 under the original regularising prior. These are effects
of choosing the regularising prior at all, rather than effects of this pull
request. Exact three-arm endpoint rows are in the per-example `estimates.csv`
files; Example 7 has [case comparisons](example7/case_compare.csv) and
[exemplar comparisons](example7/exemplar_compare.csv).

## Limits and reproducibility

The 42 Example 7 simulation fits are one paired replicate from each of seven
simulation cells and six response conventions. They identify possible fit and
estimate changes but do **not** re-estimate the source vignette's 100-replicate
bias or coverage summaries. All 24 case-study fits were rerun. Example 5's
`eval = FALSE` fit was run explicitly to include it in the comparison.

The latest Example 3 source still states in its prior table that the
regularising `d` and `f` priors are `normal(0, 5)` (line 95 of
`example3.Rmd.orig`). That text needs updating if PR #537 is merged; its
existing default-prior fit results do not change.

The later Example 8 calls were not compared across all three priors. Their
monolithic and parallel HPC jobs were cancelled on 3 November 2026 after
the user chose to finalise the available results. The completed objects remain
under `/export/scratch/rfisher/bayesnec-pr537-impact-20261103`, with job IDs
in [jobs.json](jobs.json) and
[jobs-parallel-example8.json](jobs-parallel-example8.json). The exact
first-three-fit results are in
[Example 8's estimates](example8/summary/estimates.csv). The other completed
vignette renders had no recorded knitting errors.

The analysis used the latest local `example*.Rmd.orig` source for each
vignette, including uncommitted Example 3 and Example 6 worktree edits.
Source hashes and worktree commits are in
[provenance.json](provenance.json). The pull request head was
`7c6cc939886185655139737c491e240e7a56bc28` when checked. The package
base was `2ac46e1ae35cde7d64cf20b7dceb7cc0848d9d8d`. The only overlaid
source file was `R/define_prior.R` from the pull request head. No package
source or vignette worktree was modified for this assessment.
