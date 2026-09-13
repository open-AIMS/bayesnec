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
runs <- do.call(rbind, lapply(seq_len(ceiling(n_runs / 100)), function(page) {
  gh_tsv(c(
    "api", "-X", "GET",
    sprintf("repos/%s/actions/workflows/%s/runs", repo, workflow),
    "-f", "event=pull_request", "-f", "per_page=100", "-f", paste0("page=", page),
    "--jq", ".workflow_runs[] | [.id, .head_branch, .created_at] | @tsv"
  ), c("run_id", "branch", "created_at"))
}))
# The two pages are separate requests, so a run entering between them can appear
# on both. A duplicated run would give a duplicated (run_id, job) cell and turn
# every reshape below into a list matrix.
runs <- runs[!duplicated(runs$run_id), ]
runs <- utils::head(runs, n_runs)

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
if (!setequal(unique(jobs$run_id), runs$run_id)) {
  stop("jobs were not returned for every run: ",
       length(setdiff(runs$run_id, unique(jobs$run_id))), " missing")
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
unsettled <- setdiff(runs$run_id, settled)
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
devel$day <- substr(devel$created_at, 1, 10)
by_day <- data.frame(
  runs = as.vector(table(devel$day)),
  cancelled_pct = round(100 * tapply(devel$conclusion == "cancelled",
                                     devel$day, mean), 0),
  row.names = sort(unique(devel$day))
)
print(by_day)

# The claim that cancellation follows push frequency is measured rather than
# asserted: how long after each run the next run on the same branch was created.
gap <- unlist(lapply(split(runs[runs$run_id %in% settled, ], ~branch), function(d) {
  d <- d[order(as_time(d$created_at)), ]
  if (nrow(d) < 2) return(stats::setNames(numeric(0), character(0)))
  stats::setNames(mins_between(d$created_at[-nrow(d)], d$created_at[-1]),
                  d$run_id[-nrow(d)])
}))
names(gap) <- sub("^[^.]*\\.", "", names(gap))
cat("\nminutes to the next run on the same branch, by that run's devel outcome:\n")
for (o in c("cancelled", "success")) {
  v <- gap[names(gap) %in% devel$run_id[devel$conclusion == o]]
  if (length(v)) {
    cat(sprintf("  %-10s n=%3d  median %6.1f min\n", o, length(v), stats::median(v)))
  }
}

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
cat(sprintf("devel jobs that ran to a conclusion past 60 minutes: %d; past 70: %d\n",
            sum(mins[, "ubuntu-latest (devel)"] > 60 &
                  concl[, "ubuntu-latest (devel)"] %in% c("success", "failure")),
            sum(mins[, "ubuntu-latest (devel)"] > 70 &
                  concl[, "ubuntu-latest (devel)"] %in% c("success", "failure"))))

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
  ties <- sum(d == 0)
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
cat("\nthe same, admitting failures as well, as a sensitivity check:\n")
paired("ubuntu-latest (devel)", "ubuntu-latest (release)", c("success", "failure"))
cat(sprintf(
  "  pairs lost to a devel cancellation: %d; to a release cancellation: %d\n",
  sum(concl[, "ubuntu-latest (devel)"] == "cancelled" &
        concl[, "ubuntu-latest (release)"] != "cancelled"),
  sum(concl[, "ubuntu-latest (release)"] == "cancelled" &
        concl[, "ubuntu-latest (devel)"] != "cancelled")
))

# Exempting devel from cancellation would run every cancelled devel job on to
# completion. The additional runner time is at least the gap between where each
# was cancelled and the median a completed devel job takes; at least, because a
# job cancelled beyond that median would have run further still, and those
# contribute nothing to the sum.
cancelled_devel <- mins[concl[, "ubuntu-latest (devel)"] == "cancelled",
                        "ubuntu-latest (devel)"]
typical <- stats::median(mins[concl[, "ubuntu-latest (devel)"] %in%
                                c("success", "failure"), "ubuntu-latest (devel)"])
cat(sprintf(
  "\n%d devel jobs were cancelled; running each on to the median %.1f minutes adds\nat least %.0f runner-minutes (%d were already past that median and add more).\n",
  length(cancelled_devel), typical, sum(pmax(0, typical - cancelled_devel)),
  sum(cancelled_devel > typical)
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
  "seconds from a job being created to starting: median %.0f, 90th centile %.0f, max %.0f\n",
  stats::median(queue), stats::quantile(queue, 0.9), max(queue)
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
