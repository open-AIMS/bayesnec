# Random number handling in the parallel dispatcher

The measurements behind `bnec_parallel_lapply()`, the function that serves both
of the loops that fit more than one thing: the model set inside `bnec()` and
`amend()`, and the level loop inside `bnec_group()`. They were removed from its
`@noRd` block, which now states the four load-bearing properties and the
mechanism of each without the numbers.

No script reproduces these. Each was run once, by hand, on R 4.6.1 with
`future` 1.70.0, and they are recorded here so that a later change can be
compared against something. Re-running any of them means writing the harness
again.

## The reach of a supplied seed

Whether the *fits* match a sequential run is a question of the seed. Where one is
supplied they match exactly, because `make_good_inits()` seeds itself with it
wherever it runs. Where none is supplied they do not, because the search draws
from the stream it is handed and a worker's is not the parent's. What the RNG
handling added is that each of the two runs now repeats itself, where before
neither did.

Measured under `plan(multicore, workers = 3)` on three equations, the body being
`add_brm_defaults()` and so the initial-value search itself, with
`set.seed(777)` before each run, against the released code and against the
change: sequential agreed with parallel under a seed and not without one, on
both, while two runs of either kind agreed only after the change.

A run under a plan therefore reproduces under the caller's seed. Measured on the
same setting with no `seed` supplied: two runs at one `set.seed()` gave identical
initial values and a third at another seed gave different ones. One backend and
one R version, so this is a measurement rather than a guarantee, and `seed`
remains the way to fix a run that has to repeat across either.

The model-averaging draw is the exception. `w_draw_seed` and `w_draw_index` are
drawn by `expand_manec()` in the parent, from a stream a sequential run advances
and a parallel one leaves alone, so a parallel run does not reproduce a
sequential run's model-averaged quantities even under a seed. Closing that gap
means deriving the draw from `brm_args$seed` rather than from the ambient stream,
which is a change to `expand_manec()`.

## Restoring the generator kind does not put the workers in step

The reason recorded in the comment before this was measured was wrong. It said
`RNGkind()` re-initialises `.Random.seed` from the clock and the process id. It
does so only where no seed exists yet, which is what `?Random` documents; where a
seed is already present, which inside a `future.seed = TRUE` worker it always is,
the new state is derived from the current one.

Measured over repeated calls in one process and again in a second process, on
both arms:

| sequence | first draw |
|---|---|
| `set.seed(42)`, then `RNGkind()` at the kind already in force | 0.8311705, every time |
| `set.seed(42, kind = "L'Ecuyer-CMRG")`, then `RNGkind()` back to Mersenne-Twister | 0.2046757, every time |

The second is the change a worker makes. What makes the workers differ is
therefore the per-element L'Ecuyer-CMRG stream `future.seed = TRUE` installs, not
the clock. An earlier measurement under `plan(multicore, workers = 3)` --- three
of three draws distinct, with the restore in place as without it --- holds either
way and so did not distinguish the two explanations.

## The set.seed(NULL) defect

`set.seed(NULL)` re-initialises the stream from the clock and the process id.
Calling it whenever no seed was supplied, which is the default path, discarded any
`set.seed()` the user had run.

Measured on `nec3param` fitted to `nec_data` with `Beta(link = "identity")`,
`iter = 1000`, `chains = 2`, backend `rstan`, two calls in one session each
preceded by `set.seed(333)`: the two `fixef()` tables disagreed by 9.1e-5 in `nec`
and 3.1e-3 in `beta`, and the stream was left in a different state afterwards, so
every later random operation differed too. After the change both tables and both
stream states agree exactly.

`vignette("example3")` is the case that exposed it. It runs `set.seed(333)` before
each of nine fitting chunks, which produce eleven individual fits, and passes no
`seed`, so each fit's initial-value search discarded the stream it was handed and
began from a fresh draw. That accounts for both of the outputs that differed
between renders: two `fixef()` tables read off two of those fits, and three
`check_priors()` figures, which are pure functions of the fits they plot,
`brms::hypothesis()` touching the stream only when given a seed of its own.

## mc.cores inside a worker

`bayesnec` passes `cores = 1` to `brm()` under a parallel plan. The obvious
account of what that prevents is wrong: `future` sets `mc.cores` to 1 inside a
worker itself. Measured with `options(mc.cores = 7)` in `R_PROFILE_USER`,
`getOption("mc.cores")` inside a future is 1 under `multisession` at two and four
workers, under `multicore`, and under a two-node `cluster`. Passing `cores = 1` is
not what stands between the user and `workers x chains` processes; it is kept
because it makes what `brms` is asked to do a property of this package rather than
of an implementation detail of another.

## Chunking and export size

`future_lapply()` divides the set into one chunk per worker by default, which is
wrong here twice over: the equations differ in fitting time by an order of
magnitude, so a worker drawing the slow ones sets the wall clock, and a worker
holds every fit in its chunk until the chunk ends. One element per future is used
instead. The trade is that the data, the formula and the priors are sent once per
model rather than once per worker, which for 23 models over four workers is about
six times the transfer. No timing was taken either way.

Measured on the two-equation `manec_example`: the applied function reported 14.1
MiB against 75 bytes for the arguments it reads, and the same closure over a 32 MB
object was refused outright at `future.globals.maxSize = 10 MiB`, naming `FUN`,
which tells the user nothing about the cause. `narrow_environment()` exists for
that.

A formula records the environment it was created in, and `serialize()` writes it
out in full. Measured with a 76 MiB vector bound beside the formula in the
environment that created it: the formula alone serialises to 76.29 MiB, the
narrowed closure that reads it to 76.30 MiB, and the model frame built from it to
76.30 MiB, that last by way of the `.Environment` of its `terms` attribute. A
function the formula names is the case `narrow_formula_environment()` does nothing for: a
transformation the user defined beside the formula is a closure over the same
environment, so the fit is the size it was, measured at 76.294 MiB before and
after.
