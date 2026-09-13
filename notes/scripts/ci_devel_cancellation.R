# How often a matrix job of R-CMD-check is cancelled, and what a cancellation
# means. Written for #333, which reported that the `ubuntu-latest (devel)` job of
# a `pull_request` run is cancelled about two thirds of the time and inferred
# from that a gap in R-devel coverage before merge.
#
# The script is the instrument behind `notes/ci_devel_cancellation.md` and the
# comment at the `concurrency` block of `.github/workflows/R-CMD-check.yaml`.
# Every figure quoted in either is printed here.
#
# Two properties of the API shape what can be asked of it. The jobs endpoint
# defaults to `filter=latest`, so a re-run reports the re-run's conclusion and
# the original attempt's is not visible; a re-run also keeps the run's original
# `created_at` while its jobs ran later. Three of the 200 runs in the window
# reported below had `run_attempt > 1`, too few to change any figure, but the
# choice is the API's default rather than this script's. And a cancelled job is
# right-censored: its elapsed time says where it had reached, not how long it
# would have taken, so any median over completed jobs alone is biased downwards.
# Section 3 reports the sensitivity of the one comparison that rests on it.
#
# Requires the `gh` CLI, authenticated. Every figure changes as runs accumulate,
# so a re-run samples a later window than the documents do; the window is
# printed first.
#
# Usage:  Rscript notes/scripts/ci_devel_cancellation.R [n_runs] [n_prs]

args <- commandArgs(trailingOnly = TRUE)
n_runs <- if (length(args) >= 1) as.integer(args[1]) else 200L
n_prs <- if (length(args) >= 2) as.integer(args[2]) else 30L
repo <- "open-AIMS/bayesnec"
workflow <- "R-CMD-check.yaml"

# gh --jq emits TSV, so nothing here needs a JSON parser and the script has no
# package dependency beyond base R.
#
# system2() attaches a non-zero exit as an attribute and warns rather than
# stopping, so a rate limit or an expired token would otherwise return an empty
# frame and be tabulated as an empty result. The status is checked instead.
gh_tsv <- function(args, colnames) {
  out <- system2("gh", shQuote(args), stdout = TRUE)
  status <- attr(out, "status")
  if (!is.null(status) && status != 0L) {
    stop("gh exited ", status, " for: gh ", paste(args, collapse = " "))
  }
  if (!length(out)) {
    return(stats::setNames(
      as.data.frame(matrix(character(), 0, length(colnames))), colnames
    ))
  }
  utils::read.delim(
    text = paste(out, collapse = "\n"), header = FALSE,
    sep = "\t", quote = "", colClasses = "character",
    col.names = colnames, na.strings = character()
  )
}

as_time <- function(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

mins_between <- function(from, to) {
  as.numeric(difftime(as_time(to), as_time(from), units = "mins"))
}

message("fetching the last ", n_runs, " pull_request runs of ", workflow)
pages <- lapply(seq_len(ceiling(n_runs / 100)), function(page) {
  gh_tsv(c(
    "api", "-X", "GET",
    sprintf("repos/%s/actions/workflows/%s/runs", repo, workflow),
    "-f", "event=pull_request", "-f", "per_page=100", "-f", paste0("page=", page),
    "--jq", ".workflow_runs[] | [.id, .head_branch, .created_at] | @tsv"
  ), c("run_id", "branch", "created_at"))
})
# A page returning fewer than 100 rows is the end of the listing, so a short
# result there is the repository having fewer runs than were asked for rather
# than the pages disagreeing. The two are distinguished below.
exhausted <- any(vapply(pages, nrow, integer(1)) < 100L)
runs <- do.call(rbind, pages)
# The two pages are separate requests, so a run entering between them can appear
# on both. A duplicated run would give a duplicated (run_id, job) cell and turn
# every reshape below into a list matrix.
# The dedup below would otherwise absorb an inconsistent pair of pages into a
# short sample, which reports a plausible wrong answer at exit 0; a duplicate
# run id used to crash the reshape instead. Observed once: two pages returned
# 113 distinct runs spanning nine months rather than 200 spanning three weeks.
before <- nrow(runs)
runs <- runs[!duplicated(runs$run_id), ]
if (nrow(runs) < n_runs) {
  if (before > nrow(runs)) {
    stop("asked for ", n_runs, " runs; the pages returned ", before, " rows but ",
         nrow(runs), " distinct, so they overlapped -- re-run.")
  } else if (exhausted) {
    message("only ", nrow(runs), " pull_request runs of ", workflow,
            " exist; using all of them")
    n_runs <- nrow(runs)
  } else {
    stop("asked for ", n_runs, " runs; every page was full yet only ",
         nrow(runs), " came back. The pages disagreed -- re-run.")
  }
}
runs <- utils::head(runs, n_runs)
if (as.numeric(diff(range(as_time(runs$created_at))), units = "days") > 365) {
  stop("the sampled runs span more than a year, which a coherent pair of pages ",
       "cannot: ", format(min(as_time(runs$created_at))), " to ",
       format(max(as_time(runs$created_at))))
}

# One `gh` call per run, eight at a time; serially this is several minutes. The
# `{}` inside the jq filter is deliberate: xargs stamps the run id into every
# output row, which is what makes the eight parallel writers to the shared pipe
# reassemblable. Without it the rows arrive interleaved and unattributable.
message("fetching jobs for ", nrow(runs), " runs")
ids_file <- tempfile()
writeLines(runs$run_id, ids_file)
jq <- paste0('[.jobs[] | ["{}", .name, .status, .conclusion, .created_at, ',
             '.started_at, .completed_at] | @tsv] | .[]')
cmd <- sprintf(
  "xargs -a %s -P 8 -I{} gh api 'repos/%s/actions/runs/{}/jobs' --jq '%s'",
  shQuote(ids_file), repo, jq
)
raw <- system2("bash", c("-c", shQuote(cmd)), stdout = TRUE)
if (!is.null(attr(raw, "status")) && attr(raw, "status") != 0L) {
  stop("at least one gh call in the parallel job fetch failed")
}
jobs <- utils::read.delim(
  text = paste(raw, collapse = "\n"), header = FALSE, sep = "\t", quote = "",
  colClasses = "character", na.strings = character(),
  col.names = c("run_id", "job", "status", "conclusion",
                "created_at", "started_at", "completed_at")
)
# A run whose jobs have not been created yet returns no rows, and gh exits 0. It
# cannot be a settled run, so it is dropped rather than treated as an error. A
# systematic fetch failure would take out many at once, so that is still fatal.
no_jobs <- setdiff(runs$run_id, unique(jobs$run_id))
if (length(no_jobs) > 0.05 * nrow(runs)) {
  stop(length(no_jobs), " of ", nrow(runs), " runs returned no job rows, which ",
       "is too many to be runs that have not started -- re-run")
}
if (length(no_jobs)) {
  message(length(no_jobs), " run(s) returned no job rows and are excluded: ",
          paste(no_jobs, collapse = ", "))
}
jobs$minutes <- mins_between(jobs$started_at, jobs$completed_at)

# Keep only runs in which all four cells have finished. Testing that the four
# names are present is not enough: a queued or running job is present with an
# empty conclusion, and would then be tabulated as "not cancelled".
cells <- c(
  "macOS-latest (release)", "windows-latest (release)",
  "ubuntu-latest (release)", "ubuntu-latest (devel)"
)
settled <- names(which(tapply(
  seq_len(nrow(jobs)), jobs$run_id,
  function(i) all(cells %in% jobs$job[i]) &&
    all(jobs$status[i][jobs$job[i] %in% cells] == "completed")
)))
unsettled <- setdiff(runs$run_id, settled)  # includes the no-jobs runs above
jobs <- jobs[jobs$run_id %in% settled & jobs$job %in% cells, ]
window <- range(as_time(runs$created_at[runs$run_id %in% settled]), na.rm = TRUE)

cat("\n=== window ===\n")
cat(format(window[1]), "to", format(window[2]), "UTC --",
    length(settled), "runs in which all four jobs had finished;",
    length(unsettled), "still running and excluded\n")

cat("\n=== 1. which jobs were cancelled together ===\n")
sets <- tapply(seq_len(nrow(jobs)), jobs$run_id, function(i) {
  x <- sort(jobs$job[i][jobs$conclusion[i] == "cancelled"])
  if (!length(x)) "(none cancelled)" else paste(x, collapse = " + ")
})
print(sort(table(sets), decreasing = TRUE))

# A run whose four jobs were all cancelled was superseded before any of them
# finished. How far in that was is the span of the run, from the first job
# starting to the last one stopping, rather than any single job's elapsed time:
# the four do not always start together (section 6).
allfour <- names(which(sets == paste(sort(cells), collapse = " + ")))
if (length(allfour)) {
  span <- vapply(allfour, function(r) {
    d <- jobs[jobs$run_id == r, ]
    max(mins_between(min(as_time(d$started_at)), d$completed_at))
  }, numeric(1))
  cat(sprintf(
    "\nthe all-four cancellations were superseded a median %.1f minutes in (range %.1f to %.1f)\n",
    stats::median(span), min(span), max(span)
  ))
}

cat("\n=== 2. cancellation rate by job ===\n")
print(round(100 * tapply(jobs$conclusion == "cancelled", jobs$job, mean), 1))

# The pooled rate describes no part of the window. Cancellation appears at a
# date and then persists, so it is reported by day as well, and the pooled
# figure should not be quoted on its own.
cat("\ndevel cancellation rate by day:\n")
devel <- jobs[jobs$job == "ubuntu-latest (devel)", ]
# The run's clock, not the job's: a re-run job is created later than its run, and
# `window` above is on the run's clock, so the two must agree.
devel$day <- substr(runs$created_at[match(devel$run_id, runs$run_id)], 1, 10)
by_day <- data.frame(
  runs = as.vector(table(devel$day)),
  cancelled_pct = round(100 * tapply(devel$conclusion == "cancelled",
                                     devel$day, mean), 0),
  row.names = sort(unique(devel$day))
)
print(by_day)

# Two rates the argument rests on. #333 measured its last 36 runs; the onset is
# the earliest day from which every later day has a non-zero rate, and the rate
# since then is the regime rather than the most recent window.
n_recent <- min(36L, length(settled))
recent <- utils::head(runs$run_id[runs$run_id %in% settled], n_recent)
cat(sprintf("\nmost recent %d runs: %.1f%% cancelled, the window #333 measured\n",
            n_recent,
            100 * mean(devel$conclusion[devel$run_id %in% recent] == "cancelled")))
# The onset is the earliest day after the last day on which nothing was
# cancelled. Where every day has a non-zero rate the whole window is one regime,
# and where the last day is zero there is no second regime to report.
zero_days <- which(by_day$cancelled_pct == 0)
onset <- if (!length(zero_days)) {
  rownames(by_day)[1]
} else if (max(zero_days) == nrow(by_day)) {
  NA_character_
} else {
  rownames(by_day)[max(zero_days) + 1L]
}
if (is.na(onset)) {
  cat("no onset: the most recent sampled day cancelled nothing\n")
} else {
  since <- devel[devel$day >= onset, ]
  cat(sprintf("from the onset at %s (n = %d): %.1f%%\n",
              onset, nrow(since), 100 * mean(since$conclusion == "cancelled")))
}

# The claim that cancellation follows push frequency is measured rather than
# asserted: how long after each run the next run on the same branch was created.
gaps <- do.call(rbind, lapply(
  split(runs[runs$run_id %in% settled, ], ~branch), function(d) {
    d <- d[order(as_time(d$created_at)), ]
    if (nrow(d) < 2) return(NULL)
    data.frame(run_id = d$run_id[-nrow(d)],
               gap = mins_between(d$created_at[-nrow(d)], d$created_at[-1]))
  }
))
cat("\nminutes to the next run on the same branch, by that run's devel outcome:\n")
for (o in c("cancelled", "success")) {
  v <- gaps$gap[gaps$run_id %in% devel$run_id[devel$conclusion == o]]
  if (length(v)) {
    cat(sprintf("  %-10s n=%3d  median %6.1f min\n", o, length(v), stats::median(v)))
  }
}
# The last run on each branch has no successor inside the sample and is dropped.
# Cancellations concentrate in the most recent runs, so this drops proportionally
# more of the cancelled group, and understates the contrast rather than making it.
cat(sprintf("  %d runs are the most recent on their branch and have no interval\n",
            length(settled) - nrow(gaps)))

cat("\n=== 3. elapsed minutes, paired within a run ===\n")
wide <- function(field) {
  m <- tapply(jobs[[field]], list(jobs$run_id, jobs$job), identity)
  m[, cells, drop = FALSE]
}
mins <- wide("minutes")
concl <- wide("conclusion")
cat("\ncompleted jobs only -- a cancelled job is right-censored, and censoring",
    "\nremoves the slow ones, so these medians are biased downwards:\n")
for (j in cells) {
  v <- mins[concl[, j] %in% c("success", "failure"), j]
  cat(sprintf("  %-26s n=%3d  median %5.1f  max %5.1f\n", j, length(v),
              stats::median(v), if (length(v)) max(v) else NA_real_))
}
longest <- which.max(ifelse(concl[, "ubuntu-latest (devel)"] %in%
                              c("success", "failure"),
                            mins[, "ubuntu-latest (devel)"], NA_real_))
cat(sprintf(
  "\nthe longest devel job that ran to a conclusion took %.1f minutes and ended '%s'\n",
  mins[longest, "ubuntu-latest (devel)"], concl[longest, "ubuntu-latest (devel)"]
))
concluded_devel <- mins[concl[, "ubuntu-latest (devel)"] %in%
                          c("success", "failure"), "ubuntu-latest (devel)"]
cat(sprintf("devel jobs that ran to a conclusion past 60 minutes: %d; past 70: %d\n",
            sum(concluded_devel > 60), sum(concluded_devel > 70)))
# The maximum is one excursion; a bound is set against the distribution. The
# centiles say where the routine work sits and the dates say whether the tail is
# a standing property of the job or one bad afternoon.
cat("centiles of concluded devel duration (minutes):\n")
print(round(stats::quantile(concluded_devel, c(0.5, 0.9, 0.95, 0.99, 1)), 1))
top <- utils::head(sort(concluded_devel, decreasing = TRUE), 8)
tops <- data.frame(
  minutes = round(top, 1),
  started = substr(jobs$started_at[match(
    paste(names(top), "ubuntu-latest (devel)"), paste(jobs$run_id, jobs$job)
  )], 1, 10),
  ended = concl[names(top), "ubuntu-latest (devel)"]
)
cat("the eight longest, with the day each started:\n")
print(tops, row.names = FALSE)

# Paired within a run, which is the only way to compare two cells without the
# between-run variation swamping the difference. The confidence interval, not
# the sign test, is what the argument needs: the question is the size of the
# difference, not whether it is distinguishable from zero.
paired <- function(a, b, keep) {
  ok <- concl[, a] %in% keep & concl[, b] %in% keep
  d <- mins[ok, a] - mins[ok, b]
  if (length(d) < 2) {
    cat("  too few pairs\n")
    return(invisible(NULL))
  }
  w <- suppressWarnings(stats::wilcox.test(d, conf.int = TRUE))
  st <- stats::binom.test(sum(d > 0), sum(d != 0))
  cat(sprintf(
    "  %s minus %s\n    n=%d, median %+.1f min, Hodges-Lehmann %+.2f (95%% CI %.2f to %.2f)\n    longer in %d of %d untied pairs, sign test p=%.2g\n",
    a, b, length(d), stats::median(d), w$estimate, w$conf.int[1], w$conf.int[2],
    sum(d > 0), sum(d != 0), st$p.value
  ))
}
cat("\npaired differences, both jobs successful:\n")
paired("ubuntu-latest (devel)", "ubuntu-latest (release)", "success")
paired("windows-latest (release)", "ubuntu-latest (devel)", "success")
# Censoring is not symmetric -- devel is the longer job, so it is cancelled more
# often than its release counterpart -- so the same comparison is repeated over
# the wider set that also admits failures. If the conclusion depended on the
# censoring, the two would disagree.
cat("\nthe same, admitting failures as well:\n")
paired("ubuntu-latest (devel)", "ubuntu-latest (release)", c("success", "failure"))
cat("  this adds pairs in which a job failed. It recovers no censored pair:\n")
cat("  a cancelled job has no duration, so a pair containing one is absent from\n")
cat("  both sets. What it shows is the direction a wider criterion takes the\n")
cat("  estimate.\n")

# The censored pairs are bounded rather than recovered. Where devel was cancelled
# at c and the release job concluded at r, devel's true duration exceeds c, so
# c - r is a lower bound on that pair's true difference.
d_canc <- concl[, "ubuntu-latest (devel)"] == "cancelled" &
  concl[, "ubuntu-latest (release)"] %in% c("success", "failure")
r_canc <- concl[, "ubuntu-latest (release)"] == "cancelled" &
  concl[, "ubuntu-latest (devel)"] %in% c("success", "failure")
lower <- mins[d_canc, "ubuntu-latest (devel)"] - mins[d_canc, "ubuntu-latest (release)"]
cat(sprintf(
  "\npairs censored by a devel cancellation: %d; by a release cancellation: %d\n",
  sum(d_canc), sum(r_canc)
))
cat(sprintf(
  "in the %d devel-censored pairs devel had already run a median %+.1f minutes\nlonger than the release job took, %d of them positive. Each is a value that\npair's true difference lies above. Enough of them are positive at their bounds\nthat the censored pairs cannot be what makes the estimate positive; where the\npooled estimate would go if they were admitted is not determined by a bound.\n",
  sum(d_canc), stats::median(lower), sum(lower > 0)
))

# A job cancelled at c is known only to have exceeded c, so the time it had left
# is estimated from the concluded jobs that also exceeded c, rather than by
# subtracting c from an unconditional median and clamping the negative results.
# It needs no floor. It is not monotone in c: conditioning on T > c can raise the
# conditional median faster than c rises, and above about 60 minutes the only
# concluded jobs longer than c are the 2026-09-06 cluster, so the estimate for a
# job cancelled that late rests on one afternoon. The total is a rough figure on
# an option rejected on other grounds.
cancelled_devel <- mins[concl[, "ubuntu-latest (devel)"] == "cancelled",
                        "ubuntu-latest (devel)"]
remaining <- vapply(cancelled_devel, function(c_at) {
  longer <- concluded_devel[concluded_devel > c_at]
  if (!length(longer)) return(max(0, max(concluded_devel) - c_at))
  stats::median(longer) - c_at
}, numeric(1))
cat(sprintf(
  "\n%d devel jobs were cancelled. Estimating each one's remaining time from the\nconcluded jobs that ran longer than it had, exempting devel would add about\n%.0f runner-minutes over the window. Those minutes are unbilled on a public\nrepository, so the resource is queue latency rather than spend.\n",
  length(cancelled_devel), sum(remaining)
))

cat("\n=== 4. did the merged head commit have a devel result? ===\n")
# `headRefOid` is the tip of the source branch, not the merge commit that lands
# on the base. That is the right commit to ask about: a `pull_request` run checks
# out `refs/pull/N/merge`, so the devel job tested the head merged into the base,
# and GitHub attaches the resulting check runs to the head.
prs <- gh_tsv(c(
  "pr", "list", "--repo", repo, "--state", "merged", "--limit", as.character(n_prs),
  "--json", "number,mergedAt,headRefOid",
  "--jq", ".[] | [.number, .mergedAt, .headRefOid] | @tsv"
), c("number", "merged_at", "sha"))
covered <- vapply(seq_len(nrow(prs)), function(i) {
  cr <- gh_tsv(c(
    # A commit accumulates check runs across re-runs, and the endpoint pages at
    # 30 by default, so devel could fall off the end of an unpaginated request.
    "api", "-X", "GET",
    sprintf("repos/%s/commits/%s/check-runs", repo, prs$sha[i]),
    "-f", "per_page=100",
    "--jq", paste0('.check_runs[] | select(.name=="ubuntu-latest (devel)") | ',
                   '[.conclusion, .completed_at] | @tsv')
  ), c("conclusion", "completed_at"))
  if (!nrow(cr)) return(FALSE)
  any(cr$conclusion == "success" &
        as_time(cr$completed_at) < as_time(prs$merged_at[i]), na.rm = TRUE)
}, logical(1))
cat(sprintf(
  "%d of the last %d merged pull requests had a successful devel check on the\nhead commit, completed before the merge.\n",
  sum(covered), nrow(prs)
))
if (any(!covered)) {
  cat("not covered: #", paste(prs$number[!covered], collapse = ", #"), "\n", sep = "")
}

cat("\n=== 5. the same job on pushes to dev ===\n")
dev_runs <- gh_tsv(c(
  "api", "-X", "GET",
  sprintf("repos/%s/actions/workflows/%s/runs", repo, workflow),
  "-f", "event=push", "-f", "branch=dev", "-f", "per_page=40",
  "--jq", ".workflow_runs[] | [.id, .created_at] | @tsv"
), c("run_id", "created_at"))
dev_devel <- do.call(rbind, lapply(seq_len(nrow(dev_runs)), function(i) {
  d <- gh_tsv(c(
    "api", sprintf("repos/%s/actions/runs/%s/jobs", repo, dev_runs$run_id[i]),
    "--jq", paste0('.jobs[] | select(.name=="ubuntu-latest (devel)") | ',
                   '[.status, .conclusion, .completed_at] | @tsv')
  ), c("status", "conclusion", "completed_at"))
  if (nrow(d) != 1) return(NULL)
  cbind(dev_runs[i, ], d)
}))
# The denominator is the number of runs a devel conclusion was actually read
# from, so it cannot disagree with the table below it.
cat(sprintf("of the last %d pushes to dev with a settled devel job, it was:\n",
            sum(dev_devel$status == "completed")))
print(table(dev_devel$conclusion[dev_devel$status == "completed"]))
# "Cancelled" is what the API reports; a supersede is inferred only where a
# later push to dev was created while this run's devel job was still going.
canc <- dev_devel[dev_devel$conclusion == "cancelled", ]
if (nrow(canc)) {
  superseded <- vapply(seq_len(nrow(canc)), function(i) {
    any(as_time(dev_runs$created_at) > as_time(canc$created_at[i]) &
          as_time(dev_runs$created_at) < as_time(canc$completed_at[i]))
  }, logical(1))
  cat(sprintf(
    "of the %d cancelled, %d were superseded: a later push to dev was created\nwhile the devel job was still running. The other %d were stopped by some other\nmeans, which the API does not distinguish.\n",
    nrow(canc), sum(superseded), sum(!superseded)
  ))
}

cat("\n=== 6. how the four jobs are scheduled ===\n")
# Two claims elsewhere depend on this: that the four cells run in parallel rather
# than in a queue, and that a run's span is not any single job's elapsed time.
queue <- mins_between(jobs$created_at, jobs$started_at) * 60
cat(sprintf(
  "over %d job rows, seconds from a job being created to starting:\n  all rows: median %.0f, 90th centile %.0f, max %.0f\n",
  length(queue), stats::median(queue), stats::quantile(queue, 0.9), max(queue)
))
if (any(queue < 0)) {
  # A re-run job reports a `started_at` earlier than its `created_at`. The
  # figures above include those rows, so the same statistics are repeated with
  # them dropped; quote whichever is stated, and state which.
  cat(sprintf(
    "  %d of these are negative, an artefact of a re-run. Dropping them: median %.0f,\n  90th centile %.0f, max %.0f\n",
    sum(queue < 0), stats::median(queue[queue >= 0]),
    stats::quantile(queue[queue >= 0], 0.9), max(queue[queue >= 0])
  ))
}
# Whether the repository runs jobs one at a time is a claim the `push` filter
# rests on, so it is counted rather than assumed: the running total of started
# minus completed over the whole window.
edges <- rbind(
  data.frame(t = as_time(jobs$started_at), d = 1L),
  data.frame(t = as_time(jobs$completed_at), d = -1L)
)
edges <- edges[order(edges$t), ]
running <- cumsum(edges$d)
cat(sprintf(
  "R-CMD-check jobs running at once: maximum %d, median %d while any are running\n",
  max(running), stats::median(running[running > 0])
))

stagger <- tapply(seq_len(nrow(jobs)), jobs$run_id, function(i) {
  as.numeric(diff(range(as_time(jobs$started_at[i]))), units = "mins")
})
cat(sprintf(
  "minutes between the first and last of a run's four jobs starting: median %.1f, max %.1f;\n%d runs over one minute\n",
  stats::median(stagger), max(stagger), sum(stagger > 1)
))
