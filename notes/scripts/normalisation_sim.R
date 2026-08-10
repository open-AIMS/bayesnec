# False positive / power simulation behind notes/normalisation_detection.md.
#
# Measures how often each normalisation signature fires on genuine data and on
# deliberately normalised data (issue #173 item 3). Run from the package root.

devtools::load_all(".")

set.seed(173)
n_rep <- 2000

a_naive <- function(y) {
  y <- y[is.finite(y)]
  length(y) >= 5 && max(y) == 1 && sum(y == 1) == 1
}
a_strict <- function(y) {
  a_naive(y) && !on_rational_grid(y)
}
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

# five concentrations x k replicates, declining response
design <- function(k = 6) {
  x <- rep(c(0, 1, 2, 4, 8), each = k)
  list(x = x, p = 0.9 / (1 + (x / 3)^2))
}

res <- list()
record <- function(label, truth, check, gen) {
  hits <- vapply(seq_len(n_rep), function(i) {
    d <- gen()
    c(isTRUE(check$naive(d)), isTRUE(check$strict(d)))
  }, logical(2))
  res[[length(res) + 1]] <<- data.frame(
    scenario = label, truth = truth, check = check$name,
    naive = mean(hits[1, ]), guarded = mean(hits[2, ]),
    stringsAsFactors = FALSE)
}

chk_a <- list(name = "A_divided_by_max",
              naive = function(d) a_naive(d$y),
              strict = function(d) a_strict(d$y))
chk_b <- list(name = "B_divided_by_control_mean",
              naive = function(d) b_check(d$y, d$x),
              strict = function(d) b_check(d$y, d$x))

# ---- signature A -----------------------------------------------------------

for (nt in c(5, 10, 20, 30, 50)) {
  local({
    n_trials <- nt
    record(sprintf("genuine count proportion k/n, n=%d", n_trials), "genuine",
           chk_a, function() {
             d <- design()
             list(x = d$x, y = rbinom(length(d$p), n_trials, d$p) / n_trials)
           })
  })
}
record("genuine continuous proportion (Beta)", "genuine", chk_a, function() {
  d <- design()
  list(x = d$x, y = rbeta(length(d$p), d$p * 20, (1 - d$p) * 20))
})
record("genuine positive continuous (Gamma)", "genuine", chk_a, function() {
  d <- design()
  list(x = d$x, y = rgamma(length(d$p), 10, 10 / (d$p * 30)))
})
record("genuine proportion rounded to 2 dp", "genuine", chk_a, function() {
  d <- design()
  list(x = d$x, y = round(rbeta(length(d$p), d$p * 20, (1 - d$p) * 20), 2))
})
record("divided by observed max (Gamma)", "normalised", chk_a, function() {
  d <- design()
  y <- rgamma(length(d$p), 10, 10 / (d$p * 30))
  list(x = d$x, y = y / max(y))
})
record("divided by observed max (Beta)", "normalised", chk_a, function() {
  d <- design()
  y <- rbeta(length(d$p), d$p * 20, (1 - d$p) * 20)
  list(x = d$x, y = y / max(y))
})
record("divided by max * 1.01 (Gamma)", "normalised", chk_a, function() {
  d <- design()
  y <- rgamma(length(d$p), 10, 10 / (d$p * 30))
  list(x = d$x, y = y / (max(y) * 1.01))
})
record("count proportion divided by its own max, n=20", "normalised", chk_a,
       function() {
         d <- design()
         y <- rbinom(length(d$p), 20, d$p) / 20
         list(x = d$x, y = y / max(y))
       })

# ---- signature B -----------------------------------------------------------

for (kk in c(3, 4, 6, 10)) {
  local({
    k <- kk
    record(sprintf("genuine, %d control replicates", k), "genuine", chk_b,
           function() {
             d <- design(k)
             list(x = d$x, y = rgamma(length(d$p), 10, 10 / (d$p * 30)))
           })
    record(sprintf("divided by control mean, %d control reps", k), "normalised",
           chk_b, function() {
             d <- design(k)
             y <- rgamma(length(d$p), 10, 10 / (d$p * 30))
             list(x = d$x, y = y / mean(y[d$x == 0]))
           })
  })
}
record("divided by control mean then rounded to 3 dp", "normalised", chk_b,
       function() {
         d <- design()
         y <- rgamma(length(d$p), 10, 10 / (d$p * 30))
         list(x = d$x, y = round(y / mean(y[d$x == 0]), 3))
       })
record("divided by observed max (wrong signature)", "normalised", chk_b,
       function() {
         d <- design()
         y <- rgamma(length(d$p), 10, 10 / (d$p * 30))
         list(x = d$x, y = y / max(y))
       })

out <- do.call(rbind, res)
out$naive <- sprintf("%.4f", out$naive)
out$guarded <- sprintf("%.4f", out$guarded)
print(out, row.names = FALSE)
