# Reasoning removed from the comments on grouped, hurdle and joint fits

The #484 pass over `R/bnec_group.R`, `R/bayesnecgroupfit-class.R`,
`R/bnec_hurdle.R`, `R/bayesnechurdlefit-class.R`,
`R/bayesnechurdlefit-methods.R` and `R/bayesnecjointfit-methods.R` removed one
measurement that no pull request, issue or other `notes/` file records. It is
kept here. Every other piece of reasoning removed from these files is recorded
in the pull request or issue that introduced it.

## `R/bnec_group.R`

### The sequential level loop in `bnec_group()`

`future_lapply()` creates every future before it collects any, and a sequential
future relays its conditions only when it is collected. Dispatching all the
levels in one call therefore printed every per-level message at the end of the
run. The comment written in commit `6b1fe4e8` (#347) recorded the measurement
as: "measured at 1.5 s a level, all three messages arrived at 3.0 s. On a
seven-level call at eighteen minutes a fit that is the only progress signal
there is." The loop dispatches one level at a time for that reason.
