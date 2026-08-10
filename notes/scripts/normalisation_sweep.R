# Real-data sweep behind notes/normalisation_detection.md.
#
# Applies the two normalisation signatures to every numeric column of every
# real dataset available in the repository, to measure the false positive rate
# before the check was adopted (issue #173 item 3).
#
# Run from the package root. `ignore/` is gitignored, so this reproduces only
# on a working copy that has it; the exported data frames under `data/` are
# always available.

devtools::load_all(".")

# Signature A, without the rational-grid guard, for comparison.
a_naive <- function(y) {
  y <- y[is.finite(y)]
  length(y) >= 5 && max(y) == 1 && sum(y == 1) == 1
}

# Signature A as shipped.
a_strict <- function(y) {
  y <- y[is.finite(y)]
  length(y) >= 5 && max(y) == 1 && sum(y == 1) == 1 && !on_rational_grid(y)
}

# Signature B as shipped.
b_check <- function(y, x) {
  ok <- is.finite(y) & is.finite(x)
  y <- y[ok]
  x <- x[ok]
  if (length(y) < 5) {
    return(FALSE)
  }
  ctl <- y[x == min(x)]
  length(ctl) >= 3 && !all(ctl == ctl[1]) && abs(mean(ctl) - 1) < 1e-8
}

collect_frames <- function() {
  frames <- list()
  for (f in list.files("ignore", pattern = "\\.csv$", full.names = TRUE)) {
    d <- try(utils::read.csv(f, stringsAsFactors = FALSE), silent = TRUE)
    if (!inherits(d, "try-error")) {
      frames[[basename(f)]] <- d
    }
  }
  for (f in list.files("data", full.names = TRUE)) {
    e <- new.env()
    load(f, e)
    for (nm in ls(e)) {
      o <- get(nm, e)
      if (is.data.frame(o)) {
        frames[[paste0("data/", nm)]] <- o
      }
    }
  }
  frames
}

frames <- collect_frames()
n_a <- 0L
n_b <- 0L
hits_a <- list()
hits_b <- list()

for (fn in names(frames)) {
  d <- frames[[fn]]
  num <- names(d)[vapply(d, is.numeric, logical(1))]
  num <- num[vapply(d[num], function(z) sum(is.finite(z)) >= 5, logical(1))]
  for (yv in num) {
    n_a <- n_a + 1L
    if (a_naive(d[[yv]])) {
      hits_a[[length(hits_a) + 1]] <- data.frame(
        file = fn, y_var = yv, naive = TRUE, strict = a_strict(d[[yv]]),
        stringsAsFactors = FALSE)
    }
  }
  # signature B needs an (x, y) pair; the predictor is unknown a priori, so try
  # every ordered pair
  for (xv in num) {
    for (yv in setdiff(num, xv)) {
      n_b <- n_b + 1L
      if (b_check(d[[yv]], d[[xv]])) {
        hits_b[[length(hits_b) + 1]] <- data.frame(
          file = fn, y_var = yv, x_var = xv, stringsAsFactors = FALSE)
      }
    }
  }
}

cat("data frames:", length(frames), "\n")
cat("signature A screened:", n_a, "columns |", length(hits_a),
    "naive hits\n")
if (length(hits_a)) print(do.call(rbind, hits_a), row.names = FALSE)
cat("\nsignature B screened:", n_b, "pairs |", length(hits_b), "hits\n")
if (length(hits_b)) print(do.call(rbind, hits_b), row.names = FALSE)
