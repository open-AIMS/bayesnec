# Every \link{} target in man/ must resolve, or R CMD check warns on all
# platforms. devtools::test() never builds Rd, so CI is otherwise the first
# signal. Run this before pushing anything that touches roxygen.
ex <- readLines("NAMESPACE")
known <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", ex, value = TRUE))
known <- c(known, sub("^S3method\\(([^,]+),.*$", "\\1",
                      grep("^S3method", ex, value = TRUE)))
rd <- list.files("man", pattern = "\\.Rd$", full.names = TRUE)
known <- unique(c(known, unlist(lapply(rd, function(f) {
  l <- readLines(f, warn = FALSE)
  sub("^\\\\alias\\{(.*)\\}$", "\\1", grep("^\\\\alias\\{", l, value = TRUE))
}))))
bad <- list()
for (f in rd) {
  l <- paste(readLines(f, warn = FALSE), collapse = " ")
  m <- regmatches(l, gregexpr("\\\\link\\{[^}]+\\}", l))[[1]]
  tg <- unique(sub("^\\\\link\\{(.*)\\}$", "\\1", m))
  miss <- setdiff(tg, known)
  if (length(miss)) bad[[basename(f)]] <- miss
}
if (length(bad)) {
  cat("BROKEN \\link targets:\n")
  for (n in names(bad)) cat(" ", n, "->", paste(bad[[n]], collapse = ", "), "\n")
  quit(status = 1)
}
cat("all \\link targets resolve\n")
