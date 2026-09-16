# The initial-value search band, its width and its spread

The measurements that decide `init_limits()`, `group_spread()` and
`boundary_inset()`. They were removed from the `@noRd` blocks on those
functions, which now state the rule and the mechanism alone. Re-run them with
`notes/scripts/init_search_audit.R`, whose `width` measurement produces the
coverage table below.

## The band

`init_limits()` returns the interval an initial curve's asymptotes must lie in.
It is every mean response the design estimates --- the means of the replicated
predictor values, plus the two `regularizing_location()` anchors --- widened by
the variation about those means, and intersected with the support of the mean.

## The width

Four standard deviations. The rule is the smallest width covering the asymptotes
of the curve that generated the data in every cell measured: a band that excludes
them rejects a correct starting value, and a wider one admits a starting point
further into a tail.

Measured over 6,480 simulated responses. Three generating processes --- gaussian,
Beta and poisson, so that the clamp and the boundary inset are exercised and not
only the band --- by three predictor grids, three replication levels, three
equations, a steep and a shallow curve, constant and fivefold-rising dispersion,
twenty seeds.

| width | in-scope cells covered, of 270 |
|---|---|
| 4 | 270 |
| 3 | 268 |
| 2 | 264 |
| 1 | 241 |

By process at a width of four: 90 of 90 gaussian, 90 of 90 Beta, 90 of 90
poisson.

Two kinds of cell are out of scope, both because no width covers them and the
rule would otherwise never be satisfied: a design whose predictor stops short of
the crossing, and a count design whose generating `bot` lies below the floor
`boundary_inset()` sets. Both were confirmed width-invariant before being
excluded.

### The reduction in the chosen width

Under a whole-series spread on an unreplicated design the gaussian process
needed five. The cell that decided it was an unreplicated design with rising
dispersion, missed at the lower end of the response where the noise is largest.
That is a property of the estimator rather than of the band, and the extra width
was paid on every design including the replicated ones. Reading the spread over
each half of the series and taking the larger ---
`successive_difference_spread()` --- covers that cell at four.

The reduction is not felt equally. On a replicated design the pooled spread is
unchanged and the band narrows by the full fifth. On an unreplicated one the
half-series estimator is 7 to 21 per cent wider than the whole-series estimator
it replaces, so the band narrows by 5 to 11 per cent: most of the width the rule
gives back is spent on the better spread.

## The spread estimator

Coverage does not choose it. All three candidates reach 270 of 270 at a width of
four and separate only at one, so the case for the pooled and half-series spread
rests on robustness to one aberrant observation and on the width of the band
relative to the response range.

| estimator | band widening from one aberrant observation | band as a multiple of the response range, replicated designs |
|---|---|---|
| largest single group's `sd` | 6.7 | --- |
| pooled within-group `sd` (used) | 4.1 | 1.1 to 1.4 |
| `range(y)` | 3.0 | --- |
| `sd` of the whole response | 1.6 | 2.6 to 3.8 |

The largest single group's standard deviation is the least robust, because a
maximum over groups follows whichever group an aberrant observation lands in.
The standard deviation of the whole response is the most robust and is not a
spread at all: it grows with the size of the effect, so it widens the band for a
reason that has nothing to do with the noise.

Where the replicates say too little the spread is read from the response's
successive differences instead. On the eight-concentration unreplicated design
measured, that gives 0.068 against 0.395 for `sd(y)`, and a band 1.7 times the
response range against 4.4. Below three observations there are too few
differences and the spread of the whole response stands in.

## The floor in `boundary_inset()`

What replication changes is the spread of the floor and not its level. Over 200
seeds of a `zero_inflated_poisson` design of six concentrations, the median floor
is 0.025 at eight replicates, at eighty and at four hundred. The interquartile
range falls from [0.0125, 0.0375] to [0.0223, 0.0255], and the share of seeds
whose top group is entirely zero --- which puts a level mean on the boundary and
disables the inset altogether --- falls from 15 per cent to none. The floor
concentrates on a tenth of the true smallest level mean rather than falling.

On that design the floor is 0.0375 against a generating `bot` of 0.3, where an
observation-based floor was 0.1 and a floor read from the smallest observed value
was 0.375 --- above the asymptote, which is the error the width is chosen to
avoid.

## The limit the band does not reach

Where the highest concentration has not reached the lower asymptote, every level
mean and both anchors sit above the true `bot`, and widening does not reach it.
Over the simulated designs whose predictor stops short of the crossing, coverage
is 0.160 at a width of four and 0.172 at five, against 0.003 at one. This is the
same limitation `regularizing_location()` records for the `"regularizing"` prior
set, and the criterion this band replaced is affected identically and worse,
because `min(y)` is above the true asymptote on such a design as well.
