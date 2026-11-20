# Divergent transitions in Hamiltonian Monte Carlo

A review of guidance and practice on interpreting and handling a small number of divergent transitions in Stan's No-U-Turn sampler (NUTS), prepared on 2026-11-20 for #597, which proposes that `check_sampling()` screen divergences as a proportion of retained draws.

## Scope and method

The question arose from the screen applied to each equation before model averaging in a concentration-response reanalysis, under which an equation fails if its largest Rhat exceeds 1.01, its smallest bulk or tail ESS is below 400, or it has more than 10 divergent transitions. The `nec4param` equation of `bayesnec` (a flat response to a threshold `nec`, then exponential decline to a lower asymptote `bot`) was refitted at `adapt_delta = 0.9999` with 4 chains of 8,000 warm-up and 8,000 sampling iterations thinned by 4 (8,000 retained draws). The refit gave Rhat 1.00 and ESS of at least 1,950, but 17 to 19 divergent transitions among the retained draws. The threshold is estimated between two adjacent tested concentrations. No ruling on that equation is made here.

Sources were found by web search and by following the references of the Stan documentation, and each was then read in full text where it could be accessed. Stan documentation pages, case studies and forum threads were downloaded and converted to text, and arXiv versions of journal papers were read where the journal version was not openly available (the arXiv version read is named in the reference list). Quotations were checked against the downloaded text. Stan forum threads were read through the forum's JSON interface, which gives each post's author and date. The Stan and `bayesnec` source code was read where a question turned on what the software does (e.g. how thinning interacts with the divergence count). The 43 PDFs in the authors' reference library were also converted to text with `pdftools::pdf_text()` and searched for "divergen", "adapt_delta", "Rhat", "R-hat", "effective sample", "treedepth" and "thin"; the three ecotoxicology papers that could not be downloaded (Fox, 2010; Fisher and Fox, 2023; Fisher et al., 2024b) were read in full from that library. Page numbers are the journal's printed page numbers where the PDF gives them. Sources that could not be accessed are listed in the section on gaps, and nothing is attributed to them here.

Sources were classed either as authoritative guidance (the Stan Development Team's documentation, case studies and papers by Stan developers, and statements by developers on the Stan forum) or as applied practice (what published analyses and practitioners did).

## Guidance from the Stan developers

### Definition and mechanism

The Stan Reference Manual (version 2.40) defines a divergence as follows: "A divergence arises when the simulated Hamiltonian trajectory departs from the true trajectory as measured by departure of the Hamiltonian value from its initial value." When that departure is too large, "the simulation has gone off the rails and cannot be trusted", and the manual describes the consequence as "potentially reducing Hamiltonian Monte Carlo to a simple random walk and biasing estimates by not being able to thoroughly explore the posterior distribution" (Stan Development Team, 2025a, section "Divergent transitions"). A footnote gives the threshold as an energy error of 10^3, against errors of about 10^-7 when the leapfrog integrator is working properly. The manual names "highly varying posterior curvature" as the primary cause, other than bugs in the code.

The source code agrees with that footnote and adds a detail that the manual does not spell out. In `base_nuts.hpp` a transition is flagged divergent when the energy error of any leapfrog step exceeds `max_deltaH_` (1000), and a non-finite energy is set to infinity before the comparison. In `base_hamiltonian.hpp`, a `std::domain_error` raised while evaluating the log density sets the potential energy to infinity (Stan source, develop branch, read 2026-11-20). A trajectory that steps outside the region where the density is defined is therefore counted as divergent, as Betancourt (2020b) notes: "Hamiltonian transitions that venture into constraint violations will also manifest as divergent transitions". The `bayesnec` `example3` vignette (development version 2.1.3.39) describes this mechanism for proposals that take the mean of a bounded family outside its support.

The Stan warnings page adds a second cause relevant to threshold equations: "divergences are likely if the log posterior density doesn’t have continuous derivative everywhere" (Stan Development Team, n.d.).

### Statements on an acceptable number

No Stan document gives an acceptable number or proportion of divergences. The developers' positions have, however, differed between authors and changed over time.

In April 2016 Bob Carpenter asked Michael Betancourt on the stan-users list whether he was "still running a zero-tolerance policy on divergent transitions". Betancourt replied "Yes with caveats for people who really know what they are doing", and gave a test: "if the divergences can be ignored then you should be able to get rid of them by increasing adapt_delta by a small amount. If that doesn’t work then the divergences are meaningful." In a later message he wrote that "divergences that persist even at adapt_delta = 0.999 would make me anxious. I usually accept the occasional divergence only when I’m near the nominal adapt_delta = 0.8." Carpenter replied in that thread that, with a non-centred parameterisation at `adapt_delta = 0.999`, he "often still get[s] 1 or 2 divergent transitions with 1K iterations in each of 4 chains" for hierarchical logistic regressions (stan-users, 2016a).

Betancourt (2017a, section 6.2) gives the strict position in print: "any divergent transitions encountered in a Hamiltonian Markov chain should prompt suspicion of the validity of any Markov chain Monte Carlo estimators." He continues that mild pathologies can be removed by decreasing the step size, but "Often, however, the pathologies are too severe and the divergences will persist regardless of the how much the step size is decreased, indicating significant bias in the resulting estimators." The two diagnostics he describes are "only necessary and not sufficient conditions" for valid estimators (section 6.3).

Betancourt's case study on divergences (2017b) qualifies the strict position in two ways. First, he notes that Stan's heuristic "can be a bit aggressive, however, and sometimes label transitions as divergent even when we have not lost geometric ergodicity". Second, he separates the existence of a bias from its size: "identifying the breakdown of geometric ergodicity ... indicates only that there is a bias, not how significant that bias will be", and an analysis of its significance "can quantify the significance of the bias and potentially deem it acceptable", although such analyses are "extremely subtle, challenging, and time-consuming". His conclusion is that "If the divergences can be removed by increasing adapt_delta then Hamiltonian Monte Carlo applied to the given implementation will yield accurate inferences. If the divergences persist, however, then the model will need to be reimplemented to avoid biases." His workflow case study (Betancourt, 2017c) gives the strictest form of the test: "In order to argue that divergences are only false positives, the divergences have to be completely eliminated for some adapt_delta sufficiently close to 1."

Gabry et al. (2019, section 4), with Betancourt, Vehtari and Gelman as co-authors, describe the heuristic in similar terms ("Occasionally this heuristic falsely flags stable trajectories as divergent") and give a visual test in place of the `adapt_delta` test: false positives are distributed like non-divergent draws, whereas "a concentration of divergences in small neighborhoods of parameter space, however, indicates a region of high curvature in the posterior that obstructs exploration."

The Stan warnings page was revised in March 2022 (commit `c84b698` in `stan-dev/stan-dev.github.io`, 10 March 2022). The earlier text read: "Since the validity of the estimates is not guaranteed if there are post-warmup divergences, the slower sampling is a minor cost." The current text, under the heading "Divergent transitions after warmup", reads:

> Even a small number of divergences after warmup cannot be safely ignored if completely reliable inference is desired. But if you get only few divergences and you get good Rhat and ESS values, the resulting posterior is often good enough to move forward. There are also cases when a small number of divergences without any pattern in their locations can be verified to be unimportant, but this cannot be safely assumed without a careful investigation of the model. (Stan Development Team, n.d.)

The same page frames the use of warnings by stage of analysis: "in early stages of a modelling workflow, we often don’t need completely reliable inference", and "when you see warnings you shouldn’t trust your estimates without first understanding what the warnings mean." The page does not define "few".

Other current Stan documentation is closer to the strict position. In the CmdStan guide, the Stan Development Team write that "If there are any divergences after warmup, then the sample based estimates may be biased" (Stan Development Team, 2025b). In the RStan vignette they write that "Ideally, there should be no divergent transitions after the warmup phase" (Stan Development Team, 2025c). Bürkner (2017, p. 10) describes divergent transitions as ones "that cause a bias in the obtained posterior samples", and the current `brm()` documentation keeps that wording (Bürkner, 2025). Vehtari et al. (2021, p. 688), working the centred eight-schools example, describe divergent transitions as ones "which bias the obtained estimates if they occur" and write that observing "a lot of divergent transitions ... in itself is already a sufficient indicator of convergence problems"; they add that Rhat and ESS "have the advantage over the divergent transitions diagnostic that they can be used with all MCMC algorithms not only with HMC". Gelman et al. (2020, section 4) list "the absence of divergent transitions" first among the diagnostics for validating computation with HMC. Modrák's forum primer (2020), which he later marked as outdated in favour of an expanded version, opens with "Divergences almost always signal a problem and even a small number of divergences cannot be safely ignored", and his earlier blog post (Modrák, 2018a) advises being "suspicious about your results even when only a single divergence had been reported".

Two forum statements from 2023 mark the current range of developer opinion. Andrew Gelman replied to an ecologist with 1 to 2 divergences in some `brms` fits that "Having a few divergent transitions is not a big deal at all", that setting "adapt_delta = 0.9999, max_treedepth = 15, and iter = 20000 is probably a bad idea", and that "If any concerns remain, I recommend doing a simulation experiment", citing simulation-based calibration (Stan Forums, 2023a). Betancourt wrote that "if we see divergences then we know that at least some compatible model configurations are not being included. This exclusion might be small ... or it might be large", and that "in practice we cannot distinguish between these possibilities until we identify what kind of behavior is causing those divergences" (Stan Forums, 2023b).

Across these sources, divergences that are scattered among the non-divergent draws and that disappear when the step size is reduced are treated as false positives of the heuristic. Divergences that cluster in one region, or that persist as the step size is reduced, are treated as evidence that part of the posterior was not explored. The sources differ on what follows when the count is small and Rhat and ESS are good: Betancourt requires the divergences to be removed or explained, the Stan developers suggest on the 2022 warnings page that the posterior is "often good enough", and Gelman (Stan Forums, 2023a) recommends a simulation check where concerns remain.

## Thresholds and tolerances in practice

### Published thresholds

Only a few explicit tolerances were found, and none is derived from an analysis of bias (Table 1).

Table 1. Explicit tolerances for post-warm-up divergences found in this review.

| Source | Field | Tolerance | Basis given |
|---|---|---|---|
| Stan documentation (reference manual, CmdStan guide, warnings page) | Software | None stated | Not applicable |
| Betancourt (stan-users, 2016a; 2017b; 2017c) | Stan development | Zero unless removable by a smaller step size | Divergences that vanish at higher `adapt_delta` are false positives |
| Magnusson et al. (2024), `posteriordb` | Benchmarking | Zero for a reference posterior | Listed with Rhat < 1.01, E-FMI and draw-count criteria |
| Fuglstad et al. (2020) | Statistical methods (simulation) | At most 0.1% of post-warm-up iterations | Used "as a measure of stability"; datasets above it removed from the simulation study |
| Cao and Tufto (2021, preprint) | Ecology (state-space models, simulation) | Below 0.1% | Follows Fuglstad et al.; 44 of 50 replicates kept in one setting |
| `bayesnec` 2.1.3.39 (`check_sampling()`, `screen_models()`) | Ecotoxicology | At most 10 divergent transitions (count) | Documented as having "no literature behind it" |

In two simulation studies, Fuglstad et al. (2020) report "the proportion of datasets that resulted in at most 0.1% divergent transitions for each prior and scenario", and remove dataset and prior combinations above that level from their simulation study. Cao and Tufto (2021) write that they "find it difficult to completely avoid divergent transitions across all data sets even though adapt delta is increased to 0.95", and therefore, "Similar to Fuglstad, Hem, Knight, Rue, and Riebler (2019)", removed simulations with 0.1% or more divergent transitions. Both are simulation studies in which a replicate can be discarded; in both, the rule was applied to simulated replicates and not to a single analysis of real data.

The `bayesnec` authors write in the documentation of `check_sampling()` that "Divergences 10 has no literature behind it. Stan's own guidance is that any divergence means the sampler failed to explore the posterior and estimates may be biased. Ten is pragmatic, from practice with these non-linear models, which routinely produce a handful near a boundary. It is a working default and should be described that way rather than cited." This review found no literature that would replace it.

### Practice by field

In ecology, Monnahan and Kristensen (2018) found 4 divergent transitions when fitting a state-space survival model to mark-recapture data with NUTS in ADMB (through `adnuts`), reran it at `adapt_delta = 0.9` because "This is the recommended approach when models exhibit divergences", and found that "the divergences are eliminated". On the Stan forum in 2023, an ecologist fitting seven `brms` models to 45 species reported 1 to 2 divergences in 1 to 2 of the 7 models for about 15 species, persisting at `adapt_delta = 0.9999` with 20,000 iterations thinned by 15; Gelman's reply is given above (Stan Forums, 2023a).

In ecotoxicology, papers applying `bayesnec` describe how divergences were used in screening but give no numerical criterion. Flores et al. (2021, p. 4) write that "In all cases model fit was assessed visually through chain mixing, R hat values and an assessment of the number of divergent transitions. Where models showed poor model fit, they were excluded from model averaged estimates of endpoints", and Brinkman et al. (2023, p. 4) use almost the same wording ("Models were excluded if they showed poor model fit"). Nordborg et al. (2022, p. 3) write only that "The model assumptions for, and fit of, each candidate model was assessed using the inbuilt functions in baysenec" [sic], with details in a supplementary section that was not available. Luter et al. (2025, p. 4) fitted all models "using default priors and four chains, with 20 000 iterations (19 000 as warmup), and an adapt_delta of 0.99"; the term "divergen" does not appear in their text. The earlier ecotoxicology papers are described under Precedent in ecotoxicology.

In environmental health, Trangucci et al. (2024, section 6.3) report that "There were divergent transitions during sampling for small minority of models. The total divergent transitions were small compared to the total post-warmup samples (≈0.2%)", and report results from those fits. The fits used 2,000 post-warm-up iterations per chain at `adapt_delta = 0.95`.

In pharmacometrics, Margossian et al. (2022) list divergence detection among Stan's diagnostics and defer its use to a planned second part, which could not be found. Training material from Metrum Research Group on population pharmacokinetic models fitted with Torsten describes "a couple divergent transitions" with the comment that "The small number of divergent transitions, while not serious, suggests we should consider some intervention", namely more iterations or reparameterisation (Metrum Research Group, n.d.).

In epidemiology, Grinsztajn et al. (2021) describe divergent transitions as the most serious of Stan's warnings "as it really challenges the validity of the inference, and requires revisiting the model or tweaking the inference settings". Their workflow table lists "a few divergences" in two successive versions of an SEIR model. They attributed the first to a degeneracy and addressed it by adding serological data; the second was accompanied by saturated tree depth and was followed by a final version with an increased maximum tree depth, for which no problems are listed.

In drug-discovery dose-response modelling with Stan's NUTS, Labelle et al. (2019, p. i466) use the term for a rejected proposal ("if [the proposal] is not accepted we say that the iteration as resulted in a divergence" [sic]), which is not the Stan definition (see Definition and mechanism); they ran 4 chains of 2,000 iterations with 1,000 as warm-up (pp. i466–i467).

In cognitive science, Schad et al. (2021, with Betancourt as co-author) write that "Divergent transitions indicate problems in the model fitting, and should be diagnosed and removed", and report that raising `adapt_delta` removed them in their example. The Bayesian Analysis Reporting Guidelines (Kruschke, 2021), written for the social and behavioural sciences, require reporting of convergence (PSRF) and ESS for each parameter; the full text was searched and does not mention divergent transitions. Depaoli et al. (2020, p. 10), in a tutorial on prior sensitivity in psychology whose interactive application was built with `rstan`, use R-hat < 1.01 and ESS > 1,000, and advise that where convergence fails "the practitioner can double (or more) the number of iterations to see if the longer chain fixes the issue. If non-convergence still remains, then it may be that the prior is not well suited for the model or likelihood"; their text does not mention divergent transitions either.

Practitioners on the Stan forum also differ. Asked in 2020 what rate could be ignored (2 divergences in 10,000 draws, or 22), Richard Torkar replied "I take every divergences seriously. Even if I have one (1)", while noting that he had colleagues who accepted a few when ESS was good (Stan Forums, 2020). No developer answered the question of a rate in that thread.

### Count or proportion

Several sources treat the divergence count as proportional to the number of transitions. Betancourt (2017b) found 12 divergences in a single chain of 700 iterations ("almost 2%") for the centred eight-schools model, then 299 in 10,000 iterations, and writes that "The rate of divergences remains near 2% of all iterations". This indicates that a fixed pathology produces divergences at a roughly constant rate, so that the count grows with chain length. The CmdStan `diagnose` utility and Betancourt's `check_div()` function both express the result as a proportion (e.g. "11 of 4000 (0.28%) transitions ended with a divergence"; Stan Development Team, 2025b). Fuglstad et al. (2020) and Cao and Tufto (2021) set their tolerance as a proportion. In 2016, Philip Boonstra asked on stan-users whether "the presence of divergent iterations or the proportion of divergent iterations" matters more; the developers answered with the `adapt_delta` test and did not give a proportion (stan-users, 2016a).

No source was found that argues for a fixed count independent of the number of draws. The `bayesnec` cutoff is the only count-based rule found, and its documentation disclaims a literature basis.

## Number of draws and thinning

### Typical numbers of draws

The CmdStan defaults are 1,000 warm-up and 1,000 sampling iterations per chain with `thin = 1` (Stan Development Team, 2025b), and `brm()` defaults to 4 chains of 2,000 iterations with half as warm-up (Bürkner, 2025), giving 4,000 retained draws. Fisher et al. (2024a) set the `bnec()` default to 4 chains of 10,000 iterations with 8,000 as warm-up, giving 8,000 retained draws, "as we found that a higher number of iterations are generally required for these non-linear models". The retained draws of the `nec4param` refit (8,000) therefore equal the `bayesnec` default and are twice the `brms` default, whereas its post-warm-up transitions (32,000) are eight times the `brms` default.

Applied ecotoxicology fits vary: Brinkman et al. (2023, Supplementary Materials, p. 15) ran "four separate chains ... with 10,000 iterations of each" (the `bayesnec` default), Luter et al. (2025, p. 4) ran 20,000 iterations with 19,000 as warm-up (4,000 retained draws over 4 chains), and the JAGS fits of Fisher et al. (2024b, p. 286) used three chains with 5,000 burn-in and 10,000 update iterations.

The requirements on effective sample size are much lower. Vehtari et al. (2021) recommend a rank-normalised ESS greater than 400 (given on the warnings page as 100 per chain for 4 chains) and Rhat below 1.01, and Gelman et al. (2020, section 5.6) refer to "the usual default effective sample size of a few hundred to a few thousand". Higher requirements appear for particular purposes, for example ESS of at least 10,000 for stable limits of highest-density intervals (Kruschke, 2021) and at least 10,000 approximately independent draws for a reference posterior (Magnusson et al., 2024).

### More iterations

More iterations are not presented as a remedy for divergences. The Stan developers advise on the warnings page that "Increasing the number of iterations is rarely helpful for resolving divergences/max treedepth warnings" (Stan Development Team, n.d.), and Gelman et al. (2020, section 5.1) write that "Our first instinct when faced with a problematic model should not be to throw more computational resources on the model". The constant rate in Betancourt (2017b) suggests that a longer run of one model is expected to produce proportionally more divergences, so a count can only be compared across fits of different length as a proportion.

### Thinning

Thinning changes the reported count. In Stan's `generate_transitions.hpp`, the sampler diagnostics (including `divergent__`) are written only on saved iterations (`if (save && ((m % num_thin) == 0))`), so divergences on discarded iterations are not recorded (Stan source, develop branch, read 2026-11-20). CmdStan issue #1185 (2023) reproduces this: a funnel model run twice with one seed gave 4 divergences unthinned and 0 with `thin = 4`. Jonah Gabry explained that "thinning affects how many iterations are written to csv", and Bob Carpenter recommended "estimating number of divergences as thinning rate * reported divergences, which I think should be correct in expectation". The issue was still open on 2026-11-20. `brms` passes `thin` to the backend, and `bayesnec`'s `check_sampling()` counts divergences with `nuts_params()` on the retained draws, so with `thin = 4` its cutoff of 10 is compared with divergences on a quarter of the transitions.

Thinning was routine in earlier MCMC estimation of the NEC. Fox (2010, p. 125), using WinBUGS, recorded model outputs "at every tenth iteration (a process known as ‘thinning’ which reduces autocorrelation in the recorded data)"; Fox and Landis (2016, p. 2146) show posteriors from "1 in 10 “thinning” of 100 000 Markov chain Monte Carlo iterations" (the sampler is not named in their text); and Krull (2020, p. 6), using a Gibbs sampler, monitored samples "every 10 steps". The samplers used by Fox (2010) and Krull (2020) do not produce divergent transitions, so their thinning does not bear on a divergence count.

Thinning is otherwise discouraged. Link and Eaton (2012) conclude that thinning "is often unnecessary and always inefficient" and recommend against it "unless necessitated by computer memory limitations", and the Stan Development Team (2025b) advise in the CmdStan guide that "thinning is generally not required nor advised" for HMC.

### The nec4param refit expressed as a rate

The following arithmetic applies the sources above to the refit, and is not itself taken from the literature. With 4 chains of 8,000 sampling iterations, the refit made 32,000 post-warm-up transitions and retained 8,000. The 17 to 19 divergences recorded among the retained draws are 0.21 to 0.24% of them, and Carpenter's estimator gives about 68 to 76 divergent transitions in the full 32,000. A count cutoff of 10 applied to 8,000 retained draws corresponds to a rate of 0.125%. At that rate, a fit with the `brms` default of 4,000 draws would be expected to record 8.5 to 9.5 divergences (under the cutoff of 10), whereas the fit with 8,000 retained draws recorded 17 to 19 (over it). For comparison with Table 1, the rate is above the 0.1% of Fuglstad et al. (2020), similar to the ≈0.2% that Trangucci et al. (2024) accepted, and similar to the 0.26% (26 of 10,000) that Betancourt (2017b) judged to be false positives in the non-centred eight-schools model. In Betancourt's case, however, those divergences disappeared at `adapt_delta = 0.90`, whereas those in the refit persisted at 0.9999.

## Remedies and diagnosis

### Raising adapt_delta

Raising the target acceptance rate (`adapt_delta`, default 0.8) reduces the adapted step size. The Stan Development Team (2025a) write in the reference manual that "In some cases, simply lowering the initial step size and increasing the target acceptance rate will keep the step size small enough that sampling can proceed. In other cases, a reparameterization is required". The `brms` documentation advises users seeing the divergence warning to "really think about increasing `adapt_delta`" to a value between 0.8 and 1 (Bürkner, 2025).

More recent guidance limits how far this should be taken. The CmdStan guide recommends "first trying to raise it to 0.9 or at most 0.95. Values about 0.95 are strong indication of bad geometry; the better solution is to change the model geometry through reparameterization" (Stan Development Team, 2025b). The warnings page advises "leaving adjustment of adapt_delta as a last-resort", notes that "Increasing adapt_delta beyond 0.99 and max_treedepth beyond 12 is seldom useful", and adds that "For the purpose of diagnosis, it is sometimes helpful to have more divergences, so reverting to default settings for diagnosis can be useful" (Stan Development Team, n.d.). Betancourt wrote in 2016 that raising `adapt_delta` "can resolve small issues but in many cases diverges are caused by regions of high curvature in the posterior that would require a vanishing step size to fully explore" (stan-users, 2016b). In the centred eight-schools model, Betancourt (2017b) found that raising `adapt_delta` from 0.8 to 0.99 reduced the bias but that "it never completely vanishes, even for the extreme setting of adapt_delta=0.99".

### Reparameterisation and priors

For hierarchical models the standard remedy is the non-centred parameterisation, which the Stan User's Guide describes as preferable when there are few data, the centred form being more efficient when there are many (Stan Development Team, 2025d, "Efficiency tuning"). Betancourt (2017a, section 6.2) writes more generally that where divergences persist "the target distribution itself needs to be regularized, for example with stronger priors or an alternative representation". Gelman et al. (2020, section 5.9) give a nonlinear example (a sum of two declining exponentials) in which computation with "terrible convergence" was made more stable by normal(0, 1) priors on parameters set up on unit scale, and describe the improvement as "an instance of the folk theorem" ("When you have computational problems, often there’s a problem with your model"). Betancourt (2020a, section 3.1) adds a constraint on this remedy: "Forcing less principled information into the analysis simply because it resolves an inconvenient degeneracy is a highway to the poor generalizability zone." Jacob Socolar wrote on the forum that a known parameter range should be encoded in the prior, and "If you’re still seeing divergences, that means that the nasty geometry overlaps regions where your prior and data collectively think that the true value might reside" (Stan Forums, 2023b).

### Locating divergences

The reference manual recommends plotting the draws with divergent transitions highlighted before reparameterising (Stan Development Team, 2025a), and the warnings page names `bayesplot::mcmc_pairs()` and `mcmc_parcoord()` for this. Betancourt (2017c) gives the criterion for reading such a plot: "If the divergences are not false positives then they will tend to concentrate in the pathological neighborhoods of the posterior. Falsely positive divergent iterations, however, will follow the same distribution as non-divergent iterations." Betancourt (2020a, section 4.3) adds that degenerate posteriors often "pinch" at their boundaries, and that "Because divergent iterations are sampled from divergent trajectories they will concentrate towards the problematic neighborhoods, literally pointing the way to the pathology."

### Divergences that persist at very high adapt_delta

Where divergences persist as the step size is reduced, the developers' sources interpret this as a property of the posterior and not of the tuning: Betancourt (stan-users, 2016a; 2017a; 2017b) treats persistence as evidence of a real pathology and recommends changing the model. Betancourt (2020a, section 3) describes three responses to a degeneracy: more information (data or prior), reparameterisation, and "lowered expectations". Under the last, if the computation can quantify a degenerate posterior accurately, "we might have to just report the weak inferences that result as the honest outcome of the analysis", whereas "when the degeneracies are so bad that they obstruct the complete quantification ... we won’t be able to report faithful inferences at all." The warnings page and Gelman (Stan Forums, 2023a) both recommend fitting the model to simulated data with known parameter values and checking that they are recovered, and the warnings page adds that if the problems disappear on simulated data, "your model may be a bad fit for the actual observed data". Socolar (Stan Forums, 2023b) cautions, however, that good recovery in simulations despite divergences "should not increase your confidence in the reliability of the computation in a real-world setting whatsoever", because the true values of real data are unknown.

### Weakly identified regions and threshold parameters

No source was found that treats divergences in a weakly identified region (such as a threshold parameter estimated between two design points) more leniently than divergences elsewhere. The sources that address related situations point towards closer examination.

The `nec4param` mean is written as `bot + (top - bot) * exp(-exp(beta) * (x - nec) * step(x - nec))` (`bayesnec` 2.1.3.39, `pred_equations.R` and `example2b` vignette). The Stan Functions Reference warns that step-like functions applied to parameters "can seriously hinder sampling and optimization efficiency for gradient-based methods (e.g., NUTS, HMC, BFGS)" because "they break gradients due to discontinuities coupled with zero gradients elsewhere" (Stan Development Team, 2025e). Our reading of the formula, which is not a statement from the literature, is that the mean is continuous in `nec` but its derivative with respect to `nec` changes abruptly where `nec` equals an observed concentration, so the log density may lack a continuous derivative at the design points that bound the gap. Modrák (Stan Forums, 2019) distinguished a kink from a jump in a change-point model: a term of the form slope × (x − ω) × indicator "produces a likelihood continuous wrt. ω", whereas a jump at ω does not, "And Stan does not like non-continous likelihoods". He suggested replacing the indicator with `inv_logit` "or similar to get a continous analog of the step function".

Three sources describe divergences arising where data are uninformative. Modrák (2018b) fitted a sigmoid to data drawn mostly from its flat tail and found 281 divergent transitions, explaining that "Transitions crossing the boundary often diverge due to large step size and are rejected, leading to overexploration of the flat area and bias." Gelman et al. (2020, Figure 13) show that the geometry of a Lotka-Volterra posterior is funnel-shaped with 6 data points, sampled by Stan with 9, and well behaved with 21. Socolar (Stan Forums, 2023b) explains that "the way divergent transitions can lead to really bad inference is if the true parameter values sit in (or “behind”) the region where the divergences start cropping up", and that divergences indicate "that your data doesn’t rule out the possibility that the true parameter values sit in a part of the posterior that isn’t getting explored." The `bayesnec` `example1` vignette (development version) gives an example in which 5 divergent transitions in 8,000 draws accompanied a parameter that the data did not inform, which the authors describe as "nothing a routine check would stop on".

The ecotoxicology literature read for this review treats uncertainty in the threshold as a property of the estimate, and does not report sampler diagnostics for the threshold parameter. Fisher et al. (2024b, p. 290) found that an NEC equation extended to allow a sigmoidal decline "often resulted in highly unresolved NEC estimates with extremely wide confidence bands", and concluded that "it seems likely that similar unresolved NEC estimations will result from any sigmoidal model with a relatively flat upper asymptote as the transition point (step/threshold) will likely remain highly uncertain". In their simulation study, a "somewhat complex pattern" in EC05 and EC10 estimates under one NEC scenario was attributed, "possibly", to "the position of the treatment concentrations relative to the theoretical position of the NEC", and for one sigmoidal scenario "increasing the number of treatment concentrations reduces bias in estimates more quickly for a given sampling effort" (p. 286). Fox (2010, p. 127) placed "informative priors that reflect an ‘educated’ guess (informed by an inspection of a plot of the data) as to the likely position of the NEC" on the threshold, which he describes as corresponding to "empirical Bayes". Outside ecotoxicology, Thomas (2006, pp. 669–670) found that maximum-likelihood fits of a sigmoid Emax model to simulated sparse dose-response studies failed to converge in 2.5% of five-dose and 9.6% of three-dose designs, whereas Bayesian analyses with a moderately informative prior "yielded estimators without any overt numerical problems even with the most difficult data sets" (p. 672).

## Model averaging and discarded components

### Authoritative sources

The model-averaging literature read for this review does not discuss what to do with a candidate model whose fit has computational problems. Yao et al. (2018) define stacking, pseudo-BMA and pseudo-BMA+ weights from leave-one-out predictive densities; the text was searched and uses "divergence" only for Kullback-Leibler divergence. The `loo` documentation of `loo_model_weights()` recommends "stacking for averaging predictive distributions, while pseudo-BMA+ can serve as a computationally easier alternative" and is also silent on computational failures, as is Vehtari's cross-validation FAQ.

In their Gaussian mixture example, Yao et al. (2018) do show a property of pseudo-BMA weights relevant to dropping several similar equations at once. When they added duplicates of one candidate distribution to the model list, "BMA (or Pseudo-BMA weighting) puts more weights on N(4, 1) and behaves worse, while stacking changes nowhere except for numerical fluctuation." This suggests that, under pseudo-BMA, the weight given to a shape depends on how many similar equations in the set share it, so removing a group of similar four- and five-parameter equations may change the averaged prediction more under pseudo-BMA than it would under stacking. Fisher et al. (2024a) note the related concern, citing Fox et al. (2021), that "including multiple models of similar shape may overweight the representation of that shape in model averaged predictions".

Two developer sources discuss discarding at the level of chains, which is the closest analogue found. The Stan Development Team (2025d) warn in the User's Guide that "Just throwing away Markov chains with apparently poor behavior (slow, or stuck) can lead to bias in posterior estimates". Gelman et al. (2020, section 5.5) and Yao et al. (2022) propose stacking chains with cross-validation weights in place of discarding them; this "will have the approximate effect of discarding chains that are stuck in out-of-the-way low-probability modes", although "The result from stacking is not necessarily equivalent, even asymptotically, to fully Bayesian inference". Yao et al. (2022, section 7.1) write that "sometimes we are interested in fitting a model that is legitimately difficult to compute, because we want to allow for different possible explanations of the data, and a too strong prior implies an ad hoc selection", and present stacking as suited to those settings.

The documentation of the SBC package states a related principle for simulation-based calibration: "filtering based on divergences or other fitting issues is also just a function of data and thus permissible. The resulting SBC will however provide guarantees only for data that would not be rejected by the same criteria" (SBC package, n.d.). By analogy only (the vignette concerns calibration, not averaging), an average computed after screening describes the screened set of equations, and its properties are conditional on the screen.

### Practice

Discarding failed components before averaging is the documented practice in the ecotoxicology software and papers read for this review. Fisher et al. (2024a, section 5) describe removing models that fail the Rhat criterion (< 1.05) with `amend()` and note that models with "high rhat values for some parameters, divergent transitions or issues identified with chain mixing" may be improved by refitting. They also caution that "when models are added or removed from the model-set, this can sometimes have a substantial influence on model predictions (potentially changing estimated ECx values, for example)", and that "More research is required to understand how model-set selection influences model inference." In the development version, `screen_models()` drops each equation that fails the Rhat, ESS or divergence screen and recomputes the weights over those that remain (`bayesnec` 2.1.3.39, `example2` vignette). In species sensitivity distribution modelling, `ssdtools` (version 2.7.0, `ssd_fit_dists()`, default `all_dists = FALSE`) drops a distribution that fails to fit or converge, with a warning, and averages over the rest; this is maximum likelihood with information-criterion weights, so the failure is of an optimiser and not of a sampler.

The same practice appears in published analyses. Fisher et al. (2024b, pp. 286 and 288) fitted the candidate equations with JAGS and write that "Chain mixing was always assessed visually and, where models exhibited poor mixing, they were excluded from model averaged estimates of toxicity estimates". Flores et al. (2021, p. 4) and Brinkman et al. (2023, p. 4) excluded poorly fitting `bayesnec` equations, judged partly on "the number of divergent transitions", before pseudo-BMA weighting with the Bayesian bootstrap. Thomas (2006, p. 669) took a different course in a frequentist simulation study of a single nonlinear model: "When a failure occurred, prespecified modifications to the estimation were implemented so a dose-response function was always estimated", for example a linear fit in place of a failed Emax fit. None of these sources assesses the effect of the exclusion or substitution on the final estimates.

### Bias from discarding

No source was found that quantifies the bias introduced by discarding a component with computational problems from a model average. The sources above identify the mechanisms without measuring them: the averaged prediction depends on the composition of the model set (Fisher et al., 2024a), pseudo-BMA weights depend on the redundancy of the set (Yao et al., 2018), and discarding poorly behaved chains can bias a single posterior (Stan Development Team, 2025d). Against these, the developers' statements on divergences (Betancourt, 2017a; Stan Forums, 2023b) indicate that retaining a component whose posterior was incompletely explored may also bias the average, by an amount that cannot be known without identifying the cause of the divergences. The literature therefore gives no general basis for preferring to discard such a component, or to retain it.

## Precedent in ecotoxicology

Apart from the `bayesnec` papers, the ecotoxicology papers read here fitted their models with WinBUGS or JAGS, which do not use HMC, so divergent transitions do not arise in them. Fox (2010) fitted the NEC equation (a constant response to a threshold, then exponential decline, written with an indicator function) in WinBUGS, which "uses a particular implementation of MCMC estimation known as Gibbs sampling" (p. 125), to data for three mercury compounds including methyl mercuric chloride. He ran 100,000 iterations after a burn-in of 10,000, thinned by 10, and wrote that "Additional diagnostics tools available in the WinBUGS software were used to check for convergence" (p. 125), without naming the diagnostics or giving their values. Only one equation was fitted to each dataset, so the question of dropping a candidate did not arise. Krull (2020, pp. 5–6) also fitted a Bayesian NEC model with a Gibbs sampler (`rjags`), and the text gives no convergence diagnostic.

Fisher and Fox (2023) demonstrate the NSEC with a sigmoidal model fitted "using the default initial values generated by JAGS: three chains, 2000 iterations, and a burn‐in of 1000" (p. 2025). The paper says nothing about convergence diagnostics, Rhat, ESS or divergent transitions; the extracted text was searched for each term.

Fisher et al. (2024b) compare NEC and NSEC estimates in a simulation study and two case studies, fitting the candidate equations with `jagsNEC` and weighting them by DIC. Their only sampler criterion is visual: "Chain mixing was always assessed visually and, where models exhibited poor mixing, they were excluded from model averaged estimates of toxicity estimates" (pp. 286 and 288). They note that "there are potentially much better weighting strategies available for Bayesian model fits", and that "This was one motivation for the development of the bayesnec package" (p. 290).

Fisher et al. (2024a), the `bayesnec` paper in the Journal of Statistical Software, describe the summary method as returning warnings "where parts of the model have not converged (rhat, R̂ > 1.05 ...)" and indicating "the number of any divergent transitions (if any)", with guidance on more iterations, priors and `adapt_delta`. The default priors "were designed to be somewhat informative (relative to each parameter’s region) but that would, in data-sufficient cases, return fits without HMC divergent transitions in Stan." The paper gives no tolerance for divergences.

The development version of `bayesnec` (2.1.3.39) adds the divergence cutoff of 10 discussed above, and its vignettes record measured divergence rates in specific cases. For example, the authors report in `example3` that an additive group-level term on `bot` in `nec4param` with a Beta family gave "51 divergent transitions of 2000 at `adapt_delta = 0.95`", and none under a multiplicative form; and `example6` raises `adapt_delta` to 0.99 because "the four-parameter models are weakly identified" on the survival component. These are the package authors' own measurements on example data.

The applied `bayesnec` papers by AIMS authors (Flores et al., 2021; Nordborg et al., 2022; Brinkman et al., 2023; Luter et al., 2025) are summarised under Practice by field. Flores et al. (2021) and Brinkman et al. (2023) assessed "the number of divergent transitions" and excluded poorly fitting equations from the average, and none of the four papers states a threshold for the number of divergences.

`ssdtools` (Thorley and Schwarz, 2018; Thorley et al., 2025) fits species sensitivity distributions by maximum likelihood and does not use HMC, so divergent transitions do not arise; its handling of failed fits is described above.

Fox et al. (2021) is not in the authors' library and could not be downloaded, so it was not read. No ecotoxicology paper was found that gives a numerical criterion for divergent transitions.

## Recent work, 2020 to 2026

Vehtari et al. (2021) proposed a rank-normalised, folded Rhat in place of the traditional statistic and recommended Rhat < 1.01 and a rank-normalised ESS above 400; the paper sets no criterion for the number of divergences, although its centred eight-schools example treats "a lot of divergent transitions" as sufficient evidence of convergence problems (p. 688). The 2022 revision of the Stan warnings page, Gelman et al. (2020) and the 2023 forum statements of Gelman and Betancourt are described above. Magnusson et al. (2024) require no divergent transitions for a reference posterior in `posteriordb`. The SBC paper (Modrák et al., 2025) was searched and does not discuss divergent transitions; the SBC package documentation does, as described above.

Two developments in the sampler address posteriors with varying curvature, the main cause of divergences. Modi et al. (2024) propose delayed-rejection HMC, which retries a rejected trajectory with geometrically smaller step sizes, and report "up to five-fold performance gains over optimally-tuned HMC" on multiscale distributions including Neal's funnel; the term "divergence" does not appear in the arXiv text. The Stan Reference Manual (version 2.40) cites Bou-Rabee et al. (2025) for a NUTS variant with local step-size adaptivity ("Gibbs self-tuning"); that paper was not read for this review.

No paper was found that quantifies the bias in posterior summaries as a function of the number or rate of divergent transitions. The closest evidence is illustrative: Betancourt (2017b) shows a bias in log τ with divergences at about 2% of iterations in the centred eight-schools model, and false positives at 0.26% in the non-centred model, and Modrák (2018b) shows overestimated and uncertain parameters with many divergences in a sigmoid model. Searches for a simulation study of bias against divergence counts, under several phrasings, returned only the applied reports summarised in Table 1.

## Gaps in the literature and sources not accessed

The literature read for this review is silent on the following points:

- an acceptable number or proportion of divergences derived from an analysis of bias;
- whether divergences confined to a weakly identified parameter, such as a threshold between two design points, should be treated differently from divergences elsewhere;
- how a model average should treat a candidate equation with persistent divergences, and the bias introduced by discarding it;
- the effect of divergences on the specific quantities used downstream here (NEC, EC10 or NSEC estimates and their intervals);
- the number of divergent transitions in the fits used by any published ecotoxicology analysis.

The following sources were identified but not accessed in full text, and nothing is attributed to them: Fox et al. (2021); Link and Eaton (2012) beyond its abstract; the second part of Margossian et al. (2022), which was announced but not found; Bou-Rabee et al. (2025); and the supplementary information of Fisher et al. (2024b) and Nordborg et al. (2022), which the main texts cite for details of the model fits. A web search for a post on divergences on Andrew Gelman's blog found none.

## References

Betancourt, M. (2017a). A conceptual introduction to Hamiltonian Monte Carlo. arXiv:1701.02434 (v2, 16 July 2018). https://arxiv.org/abs/1701.02434

Betancourt, M. (2017b). Diagnosing biased inference with divergences. Stan case study, January 2017. https://mc-stan.org/learn-stan/case-studies/divergences_and_bias.html

Betancourt, M. (2017c). Robust statistical workflow with RStan. Stan case study, July 2017. https://mc-stan.org/learn-stan/case-studies/rstan_workflow.html

Betancourt, M. (2020a). Identity crisis. Case study, June 2020. https://betanalpha.github.io/assets/case_studies/identifiability.html

Betancourt, M. (2020b). An introduction to Stan. Case study, March 2020. https://betanalpha.github.io/assets/case_studies/stan_intro.html

Bou-Rabee, N., Carpenter, B., Kleppe, T. S. and Marsden, M. (2025). Incorporating local step-size adaptivity into the No-U-Turn sampler using Gibbs self-tuning. The Journal of Chemical Physics 163(8). Cited from the Stan Reference Manual; not read.

Brinkman, D. L., Flores, F., Luter, H. M., Nordborg, F. M., Brooks, M., Parkerton, T. F. and Negri, A. P. (2023). Sensitivity of the Indo-Pacific coral Acropora millepora to aromatic hydrocarbons. Environmental Pollution 332, 121963. https://doi.org/10.1016/j.envpol.2023.121963 (with Supplementary Materials; read from the authors' library)

Bürkner, P.-C. (2017). brms: an R package for Bayesian multilevel models using Stan. Journal of Statistical Software 80(1), 1–28. https://doi.org/10.18637/jss.v080.i01

Bürkner, P.-C. (2025). `brm()` documentation, `brms` version 2.23.2 (`R/brm.R`, GitHub master branch, read 2026-11-20). https://github.com/paul-buerkner/brms/blob/master/R/brm.R

Cao, Y. and Tufto, J. (2021). Bayesian inference with tmbstan for a state-space model with VAR(1) state equation. arXiv:2101.05635 (preprint; no journal version found). https://arxiv.org/abs/2101.05635

CmdStan issue #1185 (2023). "thin" argument affects diagnostics. Comments by J. Gabry and B. Carpenter, 18 August 2023; open on 2026-11-20. https://github.com/stan-dev/cmdstan/issues/1185

Depaoli, S., Winter, S. D. and Visser, M. (2020). The importance of prior sensitivity analysis in Bayesian statistics: demonstrations using an interactive Shiny app. Frontiers in Psychology 11, 608045. https://doi.org/10.3389/fpsyg.2020.608045

Fisher, R., Barneche, D. R., Ricardo, G. F. and Fox, D. R. (2024a). bayesnec: an R package for concentration-response modeling and estimation of toxicity metrics. Journal of Statistical Software 110(5), 1–41. https://doi.org/10.18637/jss.v110.i05

Fisher, R. and Fox, D. R. (2023). Introducing the no-significant-effect concentration. Environmental Toxicology and Chemistry 42(9), 2019–2028. https://doi.org/10.1002/etc.5610 (read from the authors' library)

Fisher, R., Fox, D. R., Negri, A. P., van Dam, J., Flores, F. and Koppel, D. (2024b). Methods for estimating no-effect toxicity concentrations in ecotoxicology. Integrated Environmental Assessment and Management 20(1), 279–293. https://doi.org/10.1002/ieam.4809 (published online 2023; read from the authors' library)

Flores, F., Marques, J. A., Uthicke, S., Fisher, R., Patel, F., Kaserzon, S. and Negri, A. P. (2021). Combined effects of climate change and the herbicide diuron on the coral Acropora millepora. Marine Pollution Bulletin 169, 112582. https://doi.org/10.1016/j.marpolbul.2021.112582

Fox, D. R. (2010). A Bayesian approach for determining the no effect concentration and hazardous concentration in ecotoxicology. Ecotoxicology and Environmental Safety 73(2), 123–131. https://doi.org/10.1016/j.ecoenv.2009.09.012 (read from the authors' library)

Fox, D. R., van Dam, R. A., Fisher, R., Batley, G. E., Tillmanns, A. R., Thorley, J., Schwarz, C. J., Spry, D. J. and McTavish, K. (2021). Recent developments in species sensitivity distribution modeling. Environmental Toxicology and Chemistry 40(2), 293–308. https://doi.org/10.1002/etc.4925 (not accessed)

Fox, D. R. and Landis, W. G. (2016). Don't be fooled—a no-observed-effect concentration is no substitute for a poor concentration–response experiment. Environmental Toxicology and Chemistry 35(9), 2141–2148. https://doi.org/10.1002/etc.3459

Fuglstad, G.-A., Hem, I. G., Knight, A., Rue, H. and Riebler, A. (2020). Intuitive joint priors for variance parameters. Bayesian Analysis 15(4), 1109–1137. https://doi.org/10.1214/19-BA1185 (read as arXiv:1902.00242)

Gabry, J., Simpson, D., Vehtari, A., Betancourt, M. and Gelman, A. (2019). Visualization in Bayesian workflow. Journal of the Royal Statistical Society Series A 182(2), 389–402. https://doi.org/10.1111/rssa.12378 (read as arXiv:1709.01449)

Gelman, A., Vehtari, A., Simpson, D., Margossian, C. C., Carpenter, B., Yao, Y., Kennedy, L., Gabry, J., Bürkner, P.-C. and Modrák, M. (2020). Bayesian workflow. arXiv:2011.01808. https://arxiv.org/abs/2011.01808

Grinsztajn, L., Semenova, E., Margossian, C. C. and Riou, J. (2021). Bayesian workflow for disease transmission modeling in Stan. Statistics in Medicine 40(27), 6209–6234. https://doi.org/10.1002/sim.9164 (read as arXiv:2006.02985)

Krull, M. (2020). Comparing statistical analyses to estimate thresholds in ecotoxicology. PLOS ONE 15(4), e0231149. https://doi.org/10.1371/journal.pone.0231149

Kruschke, J. K. (2021). Bayesian analysis reporting guidelines. Nature Human Behaviour 5(10), 1282–1291. https://doi.org/10.1038/s41562-021-01177-7 (read in PubMed Central, PMC8526359)

Labelle, C., Marinier, A. and Lemieux, S. (2019). Enhancing the drug discovery process: Bayesian inference for the analysis and comparison of dose–response experiments. Bioinformatics 35, i464–i473. https://doi.org/10.1093/bioinformatics/btz335

Link, W. A. and Eaton, M. J. (2012). On thinning of chains in MCMC. Methods in Ecology and Evolution 3(1), 112–115. https://doi.org/10.1111/j.2041-210X.2011.00131.x (abstract read via https://pubs.usgs.gov/publication/70009624)

Luter, H. M., Damjanovic, K., Thomas, M. C., Fisher, R., Hoj, L. and Negri, A. P. (2025). A bioluminescent bacterial toxicity assay for tropical marine environments. Environmental Toxicology, 1–13. https://doi.org/10.1002/tox.70003

Magnusson, M., Torgander, J., Bürkner, P.-C., Zhang, L., Carpenter, B. and Vehtari, A. (2024). posteriordb: testing, benchmarking and developing Bayesian inference algorithms. arXiv:2407.04967. https://arxiv.org/abs/2407.04967

Margossian, C. C., Zhang, Y. and Gillespie, W. R. (2022). Flexible and efficient Bayesian pharmacometrics modeling using Stan and Torsten, Part I. CPT: Pharmacometrics & Systems Pharmacology 11(9), 1151–1169. https://doi.org/10.1002/psp4.12812 (read as arXiv:2109.10184)

Metrum Research Group (n.d.). MCMC diagnostics. Torsten population pharmacokinetics training material. https://merge.metrumrg.com/expo/expo4-torsten-poppk/posts/mcmc-diagnostics.html

Modi, C., Barnett, A. and Carpenter, B. (2024). Delayed rejection Hamiltonian Monte Carlo for sampling multiscale distributions. Bayesian Analysis 19(3), 815–842. https://doi.org/10.1214/23-BA1360 (read as arXiv:2110.00610)

Modrák, M. (2018a). Taming divergences in Stan models. Blog post, 19 February 2018. https://www.martinmodrak.cz/2018/02/19/taming-divergences-in-stan-models/

Modrák, M. (2018b). Identifying non-identifiability. Blog post, 14 May 2018. https://www.martinmodrak.cz/2018/05/14/identifying-non-identifiability/

Modrák, M. (2020). Divergent transitions – a primer. Stan Forums, 30 July 2020 (later marked by the author as outdated). https://discourse.mc-stan.org/t/divergent-transitions-a-primer/17099

Modrák, M., Moon, A. H., Kim, S., Bürkner, P., Huurre, N., Faltejsková, K., Gelman, A. and Vehtari, A. (2025). Simulation-based calibration checking for Bayesian computation: the choice of test quantities shapes sensitivity. Bayesian Analysis 20(2), 461–488. https://doi.org/10.1214/23-BA1404 (read as arXiv:2211.02383)

Monnahan, C. C. and Kristensen, K. (2018). No-U-turn sampling for fast Bayesian inference in ADMB and TMB: introducing the adnuts and tmbstan R packages. PLOS ONE 13(5), e0197954. https://doi.org/10.1371/journal.pone.0197954

Nordborg, F. M., Brinkman, D. L. and Negri, A. P. (2022). Coral recruits are highly sensitive to heavy fuel oil exposure both in the presence and absence of UV light. Environmental Pollution 309, 119799. https://doi.org/10.1016/j.envpol.2022.119799

open-AIMS/bayesnec, development branch, version 2.1.3.39 (read 2026-11-20): `R/check_sampling.R`, `R/screen_models.R`, `R/pred_equations.R`, `vignettes/example1.Rmd`, `example2.Rmd`, `example2b.Rmd`, `example3.Rmd`, `example6.Rmd`. https://github.com/open-AIMS/bayesnec

SBC package (n.d.). Rejection sampling in simulations. Vignette, SBC version 0.5.0.9000. https://hyunjimoon.github.io/SBC/articles/rejection_sampling.html

Schad, D. J., Betancourt, M. and Vasishth, S. (2021). Toward a principled Bayesian workflow in cognitive science. Psychological Methods 26(1), 103–126. https://doi.org/10.1037/met0000275 (read as arXiv:1904.12765v3)

Stan Development Team (n.d.). How to diagnose and resolve convergence problems (Stan warnings page). https://mc-stan.org/learn-stan/diagnostics-warnings.html (formerly https://mc-stan.org/misc/warnings.html). Divergence text revised in commit c84b698, 10 March 2022: https://github.com/stan-dev/stan-dev.github.io/commit/c84b698bdc0355f1940ef4ad41255223774a0c94

Stan Development Team (2025a). Stan Reference Manual, version 2.40, chapter "MCMC sampling", section "Divergent transitions". https://mc-stan.org/docs/reference-manual/mcmc.html

Stan Development Team (2025b). CmdStan User's Guide, version 2.40: "MCMC sampling" (https://mc-stan.org/docs/cmdstan-guide/mcmc_config.html) and "diagnose" (https://mc-stan.org/docs/cmdstan-guide/diagnose_utility.html).

Stan Development Team (2025c). RStan: the R interface to Stan. Vignette. https://mc-stan.org/rstan/articles/rstan.html

Stan Development Team (2025d). Stan User's Guide, version 2.40, chapter "Efficiency tuning". https://mc-stan.org/docs/stan-users-guide/efficiency-tuning.html

Stan Development Team (2025e). Stan Functions Reference, version 2.40, "Step-like functions". https://mc-stan.org/docs/functions-reference/real-valued_basic_functions.html

Stan source code, `stan-dev/stan`, develop branch (read 2026-11-20): `src/stan/services/util/generate_transitions.hpp`, `src/stan/mcmc/hmc/nuts/base_nuts.hpp`, `src/stan/mcmc/hmc/hamiltonians/base_hamiltonian.hpp`. https://github.com/stan-dev/stan

Stan Forums (2019). Advice/peer review: piecewise linear model w/ random change point and discontinuity. Reply by M. Modrák, 18–19 November 2019. https://discourse.mc-stan.org/t/advice-peer-review-piecewise-linear-model-w-random-change-point-and-discontinuity/11919

Stan Forums (2020). What rate of divergences is low enough to ignore? 3 September 2020. https://discourse.mc-stan.org/t/what-rate-of-divergences-is-low-enough-to-ignore/17820

Stan Forums (2023a). Divergent transitions and model averaging issues. Reply by A. Gelman, 16 November 2023. https://discourse.mc-stan.org/t/divergent-transitions-and-model-averaging-issues/33348

Stan Forums (2023b). Are outputs with divergent transitions not at all useful? Replies by J. Socolar (26–27 January 2023) and M. Betancourt (7 March 2023). https://discourse.mc-stan.org/t/are-outputs-with-divergent-transitions-not-at-all-useful/30173

stan-users mailing list (2016a). Determining impact of divergent transitions after warmup. Messages by P. Boonstra, B. Carpenter and M. Betancourt, 2–6 April 2016. https://groups.google.com/g/stan-users/c/FRCfwCwqAnw

stan-users mailing list (2016b). Dealing with divergent transitions in example model: from Wishart to LKJ priors for correlation. Message by M. Betancourt, 4 February 2016. https://groups.google.com/g/stan-users/c/ZTiu_ij_bC8

Thomas, N. (2006). Hypothesis testing and Bayesian estimation using a sigmoid Emax model applied to sparse dose-response designs. Journal of Biopharmaceutical Statistics 16(5), 657–677. https://doi.org/10.1080/10543400600860469 (file named Thomas2007 in the authors' library)

Thorley, J. and Schwarz, C. (2018). ssdtools: an R package to fit species sensitivity distributions. Journal of Open Source Software 3(31), 1082. https://doi.org/10.21105/joss.01082

Thorley, J., Fisher, R., Fox, D. and Schwarz, C. (2025). ssdtools v2: an R package to fit species sensitivity distributions. Journal of Open Source Software 10(105), 7492. https://doi.org/10.21105/joss.07492 (source code `R/fit-dists.R`, version 2.7.0, read at https://github.com/bcgov/ssdtools)

Trangucci, R., Contreras, J., Zelner, J., Eisenberg, J. N. S. and Chen, Y. (2024). Bayesian methods for modeling cumulative exposure to extensive environmental health hazards. Bayesian Analysis (advance publication). arXiv:2404.04398. https://arxiv.org/abs/2404.04398

Vehtari, A. (n.d.). Cross-validation FAQ. https://users.aalto.fi/~ave/CV-FAQ.html

Vehtari, A., Gelman, A., Simpson, D., Carpenter, B. and Bürkner, P.-C. (2021). Rank-normalization, folding, and localization: an improved R̂ for assessing convergence of MCMC. Bayesian Analysis 16(2), 667–718. https://doi.org/10.1214/20-BA1221 (read as arXiv:1903.08008 and in the journal version from the authors' library)

Yao, Y., Vehtari, A., Simpson, D. and Gelman, A. (2018). Using stacking to average Bayesian predictive distributions (with discussion). Bayesian Analysis 13(3), 917–1007. https://doi.org/10.1214/17-BA1091 (read as arXiv:1704.02030; the journal version with discussion and rejoinder was searched from the authors' library)

Yao, Y., Vehtari, A. and Gelman, A. (2022). Stacking for non-mixing Bayesian computations: the curse and blessing of multimodal posteriors. Journal of Machine Learning Research 23(79), 1–45. https://jmlr.org/papers/v23/20-1426.html (read as arXiv:2006.12335v3)
