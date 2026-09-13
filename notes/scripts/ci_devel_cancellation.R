# How often a matrix job of R-CMD-check is cancelled, and what a cancellation
# means. Written for #333, which reported that the `ubuntu-latest (devel)` job of
# a `pull_request` run is cancelled about two thirds of the time and inferred
# from that a gap in R-devel coverage before merge.
#
# The script is the instrument behind the comment at the `concurrency` block of
# `.github/workflows/R-CMD-check.yaml`. It reports four things:
#
#   1. which jobs of a run were cancelled together, which separates a run
#      superseded before anything finished from one that lost a single job;
#   2. the per-job cancellation rate;
#   3. elapsed time paired within a run, which is the only way to compare two
#      jobs without the between-run variation swamping the difference;
#   4. whether the head commit of each recently merged pull request had a
#      completed devel check before it merged, which is the coverage question.
#
# Requires the `gh` CLI, authenticated. Every figure moves as runs accumulate, so
# a re-run reports a later window than the comment does; the window is printed.
#
# Usage:  Rscript notes/scripts/ci_devel_cancellation.R [n_runs] [n_prs]

args <- commandArgs(trailingOnly = TRUE)
n_runs <- if (length(args) >= 1) as.integer(args[1]) else 200L
n_prs <- if (length(args) >= 2) as.integer(args[2]) else 30L
repo <- "open-AIMS/bayesnec"
workflow <- "R-CMD-check.yaml"

# gh --jq emits TSV, so nothing here needs a JSON parser and the script has no
# package dependency beyond base R.
gh_tsv <- function(args, colnames) {
  out <- system2("gh", shQuote(args), stdout = TRUE)
  if (!length(out)) {
    return(stats::setNames(
      as.data.frame(matrix(character(), 0, length(colnames))), colnames
    ))
  }
  d <- utils::read.delim(
    text = paste(out, collapse = "\n"), header = FALSE,
    sep = "\t", quote = "", colClasses = "character",
    col.names = colnames, na.strings = character()
  )
  d
}

as_time <- function(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

message("fetching the last ", n_runs, " pull_request runs of ", workflow)
runs <- do.call(rbind, lapply(seq_len(ceiling(n_runs / 100)), function(page) {
  gh_tsv(c(
    "api", "-X", "GET",
    sprintf("repos/%s/actions/workflows/%s/runs", repo, workflow),
    "-f", "event=pull_request", "-f", "per_page=100", "-f", paste0("page=", page),
    "--jq", ".workflow_runs[] | [.id, .head_branch, .created_at] | @tsv"
  ), c("run_id", "branch", "created_at"))
}))
runs <- utils::head(runs, n_runs)

# One `gh` call per run, eight at a time. Serially this is several minutes.
message("fetching jobs for ", nrow(runs), " runs")
ids_file <- tempfile()
writeLines(runs$run_id, ids_file)
jq <- '[.jobs[] | ["{}", .name, .conclusion, .started_at, .completed_at] | @tsv] | .[]'
cmd <- sprintf(
  "xargs -a %s -P 8 -I{} gh api 'repos/%s/actions/runs/{}/jobs' --jq '%s'",
  shQuote(ids_file), repo, jq
)
jobs <- utils::read.delim(
  text = paste(system2("bash", c("-c", shQuote(cmd)), stdout = TRUE), collapse = "\n"),
  header = FALSE, sep = "\t", quote = "", colClasses = "character",
  col.names = c("run_id", "job", "conclusion", "started_at", "completed_at")
)
jobs$minutes <- as.numeric(
  difftime(as_time(jobs$completed_at), as_time(jobs$started_at), units = "mins")
)

# Keep only runs that reported all four cells, so every tabulation below has the
# same denominator.
cells <- c(
  "macOS-latest (release)", "windows-latest (release)",
  "ubuntu-latest (release)", "ubuntu-latest (devel)"
)
complete <- names(which(tapply(jobs$job, jobs$run_id, function(x) all(cells %in% x))))
jobs <- jobs[jobs$run_id %in% complete & jobs$job %in% cells, ]
window <- range(as_time(runs$created_at[runs$run_id %in% complete]))

cat("\n=== window ===\n")
cat(format(window[1]), "to", format(window[2]), "--",
    length(complete), "runs\n")

cat("\n=== 1. which jobs were cancelled together ===\n")
sets <- tapply(seq_len(nrow(jobs)), jobs$run_id, function(i) {
  x <- sort(jobs$job[i][jobs$conclusion[i] == "cancelled"])
  if (!length(x)) "(none cancelled)" else paste(x, collapse = " + ")
})
print(sort(table(sets), decreasing = TRUE))

# A run whose four jobs were all cancelled was superseded before any of them
# finished; the elapsed time of any one of them says how far in that was.
allfour <- names(which(sets == paste(sort(cells), collapse = " + ")))
if (length(allfour)) {
  m <- jobs$minutes[jobs$run_id %in% allfour & jobs$job == "ubuntu-latest (devel)"]
  cat(sprintf(
    "\nall-four cancellations were a median %.1f minutes in (range %.1f to %.1f)\n",
    stats::median(m), min(m), max(m)
  ))
}

cat("\n=== 2. cancellation rate by job ===\n")
print(round(100 * tapply(jobs$conclusion == "cancelled", jobs$job, mean), 1))

# The pooled rate hides a burst. Cancellation clusters where pushes are
# frequent, so the rate is split by recency to show how far it varies within one
# window rather than reported as a single figure.
cat("\ndevel cancellation rate, most recent 80 runs against the rest:\n")
recent <- utils::head(runs$run_id[runs$run_id %in% complete], 80L)
for (lab in c("most recent 80", "the rest")) {
  i <- if (lab == "most recent 80") jobs$run_id %in% recent else !jobs$run_id %in% recent
  d <- jobs[i & jobs$job == "ubuntu-latest (devel)", ]
  cat(sprintf("  %-15s n=%3d  cancelled %4.1f%%\n", lab, nrow(d),
              100 * mean(d$conclusion == "cancelled")))
}

cat("\n=== 3. elapsed minutes, paired within a run ===\n")
wide <- function(field) {
  m <- tapply(jobs[[field]], list(jobs$run_id, jobs$job), identity)
  m[, cells, drop = FALSE]
}
mins <- wide("minutes")
concl <- wide("conclusion")
cat("\nsuccessful jobs only -- a cancelled job is censored, and censoring",
    "\nremoves the slow ones, so these medians are biased downwards:\n")
for (j in cells) {
  v <- mins[concl[, j] == "success", j]
  cat(sprintf("  %-26s n=%3d  median %5.1f  max %5.1f\n", j, length(v),
              stats::median(v), max(v)))
}
paired <- function(a, b) {
  ok <- concl[, a] == "success" & concl[, b] == "success"
  d <- mins[ok, a] - mins[ok, b]
  cat(sprintf(
    "  %s minus %s: n=%d, median %+.1f min, longer in %d, sign test p=%.2g\n",
    a, b, length(d), stats::median(d), sum(d > 0),
    stats::binom.test(sum(d > 0), length(d))$p.value
  ))
}
cat("\npaired differences:\n")
paired("ubuntu-latest (devel)", "ubuntu-latest (release)")
paired("windows-latest (release)", "ubuntu-latest (devel)")

# Exempting devel from cancellation would run every one of these to completion.
# The runner time that adds is the gap between where each was cancelled and the
# median a completed devel job takes.
cancelled_devel <- mins[concl[, "ubuntu-latest (devel)"] == "cancelled",
                        "ubuntu-latest (devel)"]
typical <- stats::median(mins[concl[, "ubuntu-latest (devel)"] == "success",
                              "ubuntu-latest (devel)"])
cat(sprintf(
  "\n%d devel jobs were cancelled in this window; running each on to the median\n%.1f minutes adds about %.0f runner-minutes.\n",
  length(cancelled_devel), typical, sum(pmax(0, typical - cancelled_devel))
))

cat("\n=== 4. did the merged head commit have a devel result? ===\n")
prs <- gh_tsv(c(
  "pr", "list", "--repo", repo, "--state", "merged", "--limit", as.character(n_prs),
  "--json", "number,mergedAt,headRefOid",
  "--jq", ".[] | [.number, .mergedAt, .headRefOid] | @tsv"
), c("number", "merged_at", "sha"))
covered <- vapply(seq_len(nrow(prs)), function(i) {
  cr <- gh_tsv(c(
    "api", sprintf("repos/%s/commits/%s/check-runs", repo, prs$sha[i]),
    "--jq", paste0('.check_runs[] | select(.name=="ubuntu-latest (devel)") | ',
                   '[.conclusion, .completed_at] | @tsv')
  ), c("conclusion", "completed_at"))
  if (!nrow(cr)) return(FALSE)
  any(cr$conclusion == "success" &
        as_time(cr$completed_at) < as_time(prs$merged_at[i]), na.rm = TRUE)
}, logical(1))
cat(sprintf(
  "%d of the last %d merged pull requests had a successful devel check on the\n",
  sum(covered), nrow(prs)
))
cat("merged head commit, completed before the merge.\n")
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
dev_devel <- unlist(lapply(dev_runs$run_id, function(id) {
  gh_tsv(c(
    "api", sprintf("repos/%s/actions/runs/%s/jobs", repo, id),
    "--jq", '.jobs[] | select(.name=="ubuntu-latest (devel)") | .conclusion'
  ), "conclusion")$conclusion
}))
cat(sprintf("of the last %d pushes to dev, the devel job was: \n", nrow(dev_runs)))
print(table(dev_devel, useNA = "ifany"))
