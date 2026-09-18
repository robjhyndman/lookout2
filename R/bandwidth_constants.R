# Scaled minimum spanning tree bandwidth for standard normal samples (Section 2.6)
#
# For each (m, n), draws `reps` standard normal samples, computes the package's
# bandwidth h_n (the gamma quantile of the MST edge lengths), and reports
#   scaled: mean of n^(1/m) h_n,
#   ratio:  mean h_n divided by h_opt, the bandwidth minimising the AMISE for the
#           Epanechnikov kernel and a standard normal density, both measured as
#           the per-coordinate standard deviation of the kernel,
#   count:  expected number of observations inside the kernel support at the
#           mode, n P(||Y|| <= sqrt(m+4) h_n), with ||Y||^2 chi-squared on m df.

bandwidth_constants <- function(
  gamma,
  reps = 10,
  m_values = c(2, 3, 5),
  n_values = c(1000, 4000, 16000)
) {
  grid <- expand.grid(m = m_values, n = n_values)
  purrr::pmap_dfr(grid, function(m, n) {
    b_m <- pi^(m / 2) / base::gamma(m / 2 + 1)
    h <- replicate(reps, {
      X <- matrix(rnorm(n * m), n, m)
      lookout::find_tda_bw(X, gamma = gamma)
    })
    # AMISE = n^-1 h^-m R(K) + h^4 psi / 4 with mu_2(K) = 1, where for the
    # Epanechnikov kernel in sd units R(K) = 2(m+2) / (b_m (m+4)^(1+m/2)) and
    # for the standard normal psi = m(m+2) / (4 (4 pi)^(m/2)).
    h_opt <- (8 * (4 * pi)^(m / 2) / (b_m * (m + 4)^(1 + m / 2)))^(1 / (m + 4)) *
      n^(-1 / (m + 4))
    count <- n * pchisq((m + 4) * h^2, df = m)
    tibble(
      m = m,
      n = n,
      scaled = mean(n^(1 / m) * h),
      ratio = mean(h) / h_opt,
      count = mean(count)
    )
  }) |>
    arrange(m, n)
}

create_table_bandwidth <- function(tab) {
  dir.create("Data_Output", showWarnings = FALSE)
  file <- here::here("Data_Output/bandwidth_constants.tex")
  rows <- sprintf(
    "%d & %d & %.1f & %.2f & %s \\\\",
    tab$m,
    tab$n,
    tab$scaled,
    tab$ratio,
    format(signif(tab$count, 2), big.mark = ",", scientific = FALSE, trim = TRUE)
  )
  writeLines(
    c(
      "\\begin{tabular}{rrrrr}",
      "\\toprule",
      "$m$ & $n$ & $n^{1/m}h_n$ & $h_n/h_{\\text{opt}}$ & Observations in window at mode \\\\",
      "\\midrule",
      rows,
      "\\bottomrule",
      "\\end{tabular}"
    ),
    file
  )
  file
}
