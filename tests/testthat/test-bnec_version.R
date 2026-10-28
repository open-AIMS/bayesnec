# DESCRIPTION fields as each installation route writes them.
installed_as <- function(route) {
  built <- "R 4.6.1; ; 2026-10-28 00:00:00 UTC; unix"
  sha <- "0123456789abcdef0123456789abcdef01234567"
  switch(
    route,
    remotes_github = list(
      Version = "2.2.0", Built = built, RemoteType = "github",
      RemoteHost = "api.github.com", RemoteRepo = "bayesnec",
      RemoteUsername = "open-AIMS", RemoteRef = "dev", RemoteSha = sha,
      GithubRepo = "bayesnec", GithubUsername = "open-AIMS",
      GithubRef = "dev", GithubSHA1 = sha
    ),
    pak_github = list(
      Version = "2.2.0", Built = built, RemoteType = "github",
      RemoteHost = "api.github.com", RemotePkgRef = "open-AIMS/bayesnec@dev",
      RemoteRepo = "bayesnec", RemoteUsername = "open-AIMS",
      RemoteRef = "dev", RemoteSha = sha
    ),
    cran = list(Version = "2.2.0", Built = built, Repository = "CRAN"),
    pak_cran = list(
      Version = "2.2.0", Built = built, Repository = "CRAN",
      RemoteType = "standard", RemotePkgRef = "bayesnec",
      RemoteRef = "bayesnec", RemoteRepos = "https://cloud.r-project.org",
      RemoteSha = "2.2.0"
    ),
    pak_cran_ref = list(
      Version = "2.2.0", Built = built, Repository = "CRAN",
      RemoteType = "cran", RemotePkgRef = "cran::bayesnec",
      RemotePkgPlatform = "source", RemoteSha = "2.2.0"
    ),
    r_universe = list(
      Version = "2.2.0", Built = built,
      Repository = "https://open-aims.r-universe.dev",
      RemoteUrl = "https://github.com/open-AIMS/bayesnec",
      RemoteRef = "HEAD", RemoteSha = sha
    ),
    pak_git = list(
      Version = "2.2.0", Built = built, RemoteType = "git",
      RemoteUrl = "https://github.com/open-AIMS/bayesnec.git",
      RemotePkgRef = "git::https://github.com/open-AIMS/bayesnec.git",
      RemoteRef = "HEAD", RemoteSha = sha
    ),
    r_cmd_install = list(Version = "2.2.0", Built = built),
    install_local = list(Version = "2.2.0", Built = built,
                         RemoteType = "local", RemoteUrl = "~/bayesnec",
                         RemoteSha = sha),
    load_all = list(Version = "2.2.0")
  )
}

sha_a <- "0123456789abcdef0123456789abcdef01234567"
sha_b <- "fedcba9876543210fedcba9876543210fedcba98"

github_at <- function(sha, version = "2.2.0") {
  desc <- installed_as("remotes_github")
  desc$RemoteSha <- sha
  desc$Version <- version
  desc
}

with_record <- function(fit, desc) {
  attr(fit, "bnec_version") <- bayesnec:::version_table(
    bayesnec:::fit_equations(fit), bayesnec:::version_record(desc)
  )
  fit
}

stored_fits <- bayesnec:::recover_prebayesnecfit(manec_example)
fit_from_store <- function(..., model) stored_fits[[model]]
logit_data <- data.frame(x = nec_data$x, y = stats::qlogis(nec_data$y))

test_that("each installation route gives its source and commit", {
  rec <- function(route) bayesnec:::version_record(installed_as(route))
  for (route in c("remotes_github", "pak_github")) {
    r <- rec(route)
    expect_identical(r$source, "github", info = route)
    expect_identical(r$commit, sha_a, info = route)
    expect_identical(r$ref, "dev", info = route)
    expect_identical(r$repo, "open-AIMS/bayesnec", info = route)
  }
  # pak writes the version into RemoteSha for a package from a repository.
  for (route in c("cran", "pak_cran", "pak_cran_ref")) {
    r <- rec(route)
    expect_identical(r$source, "CRAN", info = route)
    expect_true(is.na(r$commit), info = route)
    expect_true(is.na(r$ref), info = route)
  }
  r <- rec("pak_git")
  expect_identical(r$source, "git")
  expect_identical(r$commit, sha_a)
  expect_identical(r$repo, "https://github.com/open-AIMS/bayesnec.git")
  r <- rec("r_universe")
  expect_identical(r$source, "https://open-aims.r-universe.dev")
  expect_identical(r$commit, sha_a)
  expect_identical(r$repo, "https://github.com/open-AIMS/bayesnec")
  # A local installation records no commit, even where a field holds one.
  for (route in c("r_cmd_install", "install_local", "load_all")) {
    r <- rec(route)
    expect_identical(r$source, "local", info = route)
    expect_true(is.na(r$commit), info = route)
    expect_true(is.na(r$repo), info = route)
  }
  expect_identical(rec("cran")$bayesnec, "2.2.0")
  expect_identical(rec("cran")$R, as.character(getRversion()))
})

test_that("an unreadable DESCRIPTION gives the unknown record", {
  r <- bayesnec:::version_record(NA)
  expect_identical(r$source, "unknown")
  expect_true(is.na(r$bayesnec))
})

test_that("the running installation is read from its own DESCRIPTION", {
  r <- bayesnec:::version_record()
  expect_identical(r$bayesnec, as.character(utils::packageVersion("bayesnec")))
  expect_type(r$source, "character")
  expect_false(is.na(r$source))
})

test_that("a fit made before the record existed reports unknown", {
  v <- bnec_version(manec_example)
  expect_named(v, c("model", "bayesnec", "source", "commit", "ref", "repo",
                    "R", "brms", "backend"))
  expect_equal(v$model, c("nec4param", "ecx4param"))
  expect_equal(v$source, c("unknown", "unknown"))
  expect_true(all(is.na(v$bayesnec)))
  # brms stores its own version in every brmsfit, so it is still reported.
  expect_equal(v$brms, rep(as.character(manec_example$mod_fits[[1]]$fit$version$brms), 2))
  expect_match(v$backend, "^rstan ")
  # Removing the attribute from a recorded fit gives the same answer.
  recorded <- with_record(nec4param, installed_as("cran"))
  attr(recorded, "bnec_version") <- NULL
  expect_identical(bnec_version(recorded)$source, "unknown")
  out <- capture.output(print(summary(nec4param))) |>
    suppressWarnings()
  expect_true(any(grepl(
    "Fitted by a version of bayesnec that did not record its installation",
    out, fixed = TRUE
  )))
})

test_that("bnec_version refuses an object it cannot describe", {
  expect_error(bnec_version(nec_data), "applies to an object returned by")
})

test_that("bnec() records the installation on a set and on a single fit", {
  local_mocked_bindings(
    fit_bayesnec = fit_from_store,
    bayesnec_description = function() github_at(sha_a),
    .package = "bayesnec"
  )
  set <- suppressWarnings(suppressMessages(
    bnec(y ~ crf(x, model = c("nec4param", "ecx4param")), data = logit_data,
         family = gaussian())
  ))
  v <- bnec_version(set)
  expect_equal(v$model, c("nec4param", "ecx4param"))
  expect_equal(v$commit, c(sha_a, sha_a))
  expect_equal(v$source, c("github", "github"))
  single <- suppressWarnings(suppressMessages(
    bnec(y ~ crf(x, model = "nec4param"), data = logit_data,
         family = gaussian())
  ))
  expect_equal(bnec_version(single)$commit, sha_a)
  expect_equal(bnec_version(single)$model, "nec4param")
})

test_that("c() and + keep each equation's record and state the mixture", {
  a <- with_record(nec4param, github_at(sha_a))
  b <- with_record(ecx4param, installed_as("cran"))
  msgs <- capture_messages(ab <- suppressWarnings(c(a, b)))
  expect_match(msgs, "fitted by different installations", all = FALSE)
  v <- bnec_version(ab)
  expect_equal(v$model, c("nec4param", "ecx4param"))
  expect_equal(v$source, c("github", "CRAN"))
  expect_equal(v$commit, c(sha_a, NA))
  msgs <- capture_messages(plus <- suppressWarnings(a + b))
  expect_match(msgs, "fitted by different installations", all = FALSE)
  expect_equal(bnec_version(plus)$source, c("github", "CRAN"))
  # A fit with no record keeps the unknown record beside a recorded one.
  ab_old <- suppressWarnings(suppressMessages(c(a, ecx4param)))
  expect_equal(bnec_version(ab_old)$source, c("github", "unknown"))
  # Two fits from one installation are not a mixture.
  same <- with_record(ecx4param, github_at(sha_a))
  msgs <- capture_messages(suppressWarnings(c(a, same)))
  expect_false(any(grepl("different installations", msgs)))
  out <- capture.output(print(summary(ab, check_fit = FALSE))) |>
    suppressWarnings() |>
    suppressMessages()
  expect_true(any(grepl("Fitted by different installations", out)))
  expect_true(any(grepl(
    "nec4param: bayesnec 2.2.0, installed from github open-AIMS/bayesnec@dev at commit 0123456",
    out, fixed = TRUE
  )))
  expect_true(any(grepl("ecx4param: bayesnec 2.2.0, installed from CRAN",
                        out, fixed = TRUE)))
})

test_that("a set from one installation prints one line", {
  a <- with_record(nec4param, github_at(sha_a))
  out <- capture.output(print(summary(a))) |>
    suppressWarnings()
  expect_true(any(grepl(paste0(
    "Fitted by bayesnec 2.2.0, installed from github ",
    "open-AIMS/bayesnec@dev at commit 0123456, under R"
  ), out, fixed = TRUE)))
  expect_s3_class(suppressWarnings(summary(a))$bnec_version, "data.frame")
})

test_that("pull_out() keeps the record of the equation it returns", {
  a <- with_record(nec4param, github_at(sha_a))
  b <- with_record(ecx4param, installed_as("cran"))
  ab <- suppressWarnings(suppressMessages(c(a, b)))
  expect_equal(
    bnec_version(suppressMessages(pull_out(ab, "nec4param")))$commit, sha_a
  )
  expect_equal(
    bnec_version(suppressMessages(pull_out(ab, "ecx4param")))$source, "CRAN"
  )
})

test_that("amend() keeps retained records and records what it adds", {
  a <- with_record(nec4param, github_at(sha_a))
  b <- with_record(ecx4param, github_at(sha_a))
  ab <- suppressWarnings(suppressMessages(c(a, b)))
  dropped <- suppressWarnings(suppressMessages(amend(ab, drop = "ecx4param")))
  expect_equal(bnec_version(dropped)$commit, sha_a)
  local_mocked_bindings(
    fit_bayesnec = fit_from_store,
    bayesnec_description = function() github_at(sha_b, "2.2.1"),
    .package = "bayesnec"
  )
  msgs <- capture_messages(
    added <- suppressWarnings(amend(a, add = "ecx4param"))
  )
  expect_match(msgs, "fitted by different installations", all = FALSE)
  v <- bnec_version(added)
  expect_equal(v$model, c("nec4param", "ecx4param"))
  expect_equal(v$commit, c(sha_a, sha_b))
  expect_equal(v$bayesnec, c("2.2.0", "2.2.1"))
})

test_that("update() gives every refitted equation the current record", {
  a <- with_record(nec4param, github_at(sha_a))
  b <- with_record(ecx4param, installed_as("cran"))
  ab <- suppressWarnings(suppressMessages(c(a, b)))
  # Return the stored brmsfit rather than sampling it again.
  local_mocked_bindings(
    update = function(object, ...) object,
    bayesnec_description = function() github_at(sha_b),
    .package = "bayesnec"
  )
  upd <- suppressWarnings(suppressMessages(stats::update(ab)))
  v <- bnec_version(upd)
  expect_equal(v$model, c("nec4param", "ecx4param"))
  expect_equal(v$commit, c(sha_b, sha_b))
  upd1 <- suppressWarnings(suppressMessages(stats::update(a)))
  expect_equal(bnec_version(upd1)$commit, sha_b)
})

test_that("a hurdle fit reports the record of each component", {
  a <- with_record(nec4param, github_at(sha_a))
  ab <- with_record(suppressWarnings(suppressMessages(c(nec4param, ecx4param))),
                    installed_as("cran"))
  local_mocked_bindings(
    bnec = function(formula, data, ...) {
      if (".alive" %in% names(data)) ab else a
    },
    .package = "bayesnec"
  )
  dat <- data.frame(x = rep(1:4, each = 5),
                    y = c(seq(1, 3, length.out = 15), rep(0, 5)))
  h <- suppressMessages(bnec_hurdle(y ~ crf(x, "nec3param"), data = dat))
  v <- bnec_version(h)
  expect_equal(v$component, c("growth", "survival", "survival"))
  expect_equal(v$model, c("nec4param", "nec4param", "ecx4param"))
  expect_equal(v$source, c("github", "CRAN", "CRAN"))
  expect_identical(names(v)[1:2], c("component", "model"))
})

test_that("a grouped fit reports the record of each level", {
  a <- with_record(nec4param, github_at(sha_a))
  local_mocked_bindings(
    bnec = function(...) a,
    .package = "bayesnec"
  )
  d <- nec_data[, c("x", "y")]
  d$site <- rep(c("a", "b"), length.out = nrow(d))
  g <- suppressWarnings(suppressMessages(
    bnec_group(y ~ crf(x, "nec4param"), data = d, group_var = "site",
               family = "beta")
  ))
  v <- bnec_version(g)
  expect_equal(v$level, c("a", "b"))
  expect_equal(v$commit, c(sha_a, sha_a))
})

test_that("a joint refit of a grouped fit records the installation", {
  mock_level <- function() {
    structure(
      list(mod_fits = list(nec4param = NULL, ecx4param = NULL),
           success_models = c("nec4param", "ecx4param"),
           mod_stats = data.frame(model = c("nec4param", "ecx4param"),
                                  wi = c(0.7, 0.3),
                                  row.names = c("nec4param", "ecx4param")),
           ne_type = "N(S)EC"),
      class = c("bayesmanecfit", "bnecfit")
    )
  }
  grouped <- structure(
    list(fits = list(a = mock_level(), b = mock_level()), group_var = "site",
         levels = c("a", "b"),
         formula = bnf(y ~ crf(x, c("nec4param", "ecx4param"))),
         data = data.frame(x = c(1, 2, 3, 4), y = c(2, 1, 2, 1),
                           site = c("a", "a", "b", "b")),
         family = gaussian(), n = c(2L, 2L), weights_method = "pseudobma"),
    class = c("bayesnecgroupfit", "bnecfit")
  )
  local_mocked_bindings(
    fit_bayesnec = function(formula, ...) {
      list(fit = stored_fits$nec4param$fit, bayesnecformula = formula,
           init = NULL)
    },
    bayesnec_description = function() github_at(sha_b),
    .package = "bayesnec"
  )
  j <- suppressMessages(bnec_joint(grouped))
  v <- bnec_version(j)
  expect_equal(v$level, c("a", "b"))
  expect_equal(v$model, c("nec4param", "nec4param"))
  expect_equal(v$commit, c(sha_b, sha_b))
  out <- capture.output(print(j))
  expect_true(any(grepl("fitted by .*: bayesnec 2.2.0, installed from github",
                        out)))
  # A joint fit with no record reports unknown at each level.
  attr(j, "bnec_version") <- NULL
  expect_equal(bnec_version(j)$source, c("unknown", "unknown"))
})
