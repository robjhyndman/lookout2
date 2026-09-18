# Timing (Section 2): seconds taken by the minimum spanning tree and by the
# whole algorithm on standard normal samples. "exact" uses the full
# leave-one-out kernel density estimate (fast = FALSE), computed with a
# fixed-radius neighbour search whose cost is proportional to the number of
# pairs of observations inside the kernel support; it is run for n <= 10^4 in
# every dimension and up to n = 10^5 in two dimensions, where the support
# holds few observations. "truncated" sums each kernel over at most 500
# nearest neighbours (fast = TRUE).

run_timing <- function(gamma) {
  grid <- bind_rows(
    expand.grid(m = c(2, 5, 10), n = c(1e3, 1e4, 1e5)),
    data.frame(m = 2, n = 1e6)
  )
  purrr::pmap_dfr(grid, function(m, n) {
    X <- matrix(rnorm(n * m), n, m)
    mst <- system.time(lookout::find_tda_bw(X, gamma = gamma))["elapsed"]
    truncated <- system.time(
      lookout::lookout(X, gamma = gamma, fast = TRUE)
    )["elapsed"]
    exact <- if (n <= 1e4 || (m == 2 && n <= 1e5)) {
      system.time(lookout::lookout(X, gamma = gamma, fast = FALSE))["elapsed"]
    } else {
      NA_real_
    }
    tibble(
      m = m,
      n = n,
      mst = unname(mst),
      exact = unname(exact),
      truncated = unname(truncated)
    )
  })
}

create_table_timing <- function(tab) {
  dir.create("Data_Output", showWarnings = FALSE)
  file <- here::here("Data_Output/timing.tex")
  fmt <- function(x) ifelse(is.na(x), "", formatC(x, digits = 2, format = "fg", big.mark = ","))
  rows <- sprintf(
    "%d & $10^{%d}$ & %s & %s & %s \\\\",
    tab$m,
    round(log10(tab$n)),
    fmt(tab$mst),
    fmt(tab$exact),
    fmt(tab$truncated)
  )
  writeLines(
    c(
      "\\begin{tabular}{rrrrr}",
      "\\toprule",
      "$m$ & $n$ & Tree & Total, exact KDE & Total, truncated KDE \\\\",
      "\\midrule",
      rows,
      "\\bottomrule",
      "\\end{tabular}"
    ),
    file
  )
  file
}
