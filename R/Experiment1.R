# Experiment 1: Gamma distribution

generate_exp1 <- function(n1, n2, rate) {
  # Generate random data
  out <- rbind(
    matrix(rgamma(n = 2 * n1, shape = 2, rate = 2), ncol = 2),
    matrix(rgamma(n = 2 * n2, shape = 2, rate = rate), ncol = 2)
  ) |>
    as.data.frame()
  colnames(out) <- c("X1", "X2")
  out$Points <- c(rep("Non-anomaly", n1), rep("Anomaly", n2))
  as_tibble(out)
}

run_synthetic_exp1 <- function(scale = scale) {
  n1 <- 500
  n2 <- 10
  rate2 <- seq(10) / 10
  bw <- 0.98

  results_old <- results_new <- tibble(
    outrate = numeric(10 * length(rate2)),
    outliers = 0
  ) |>
    bind_cols(set_up_diff_metrics(10 * length(rate2)))

  kk <- 1
  for (jj in 1:10) {
    for (i in 1:length(rate2)) {
      X <- generate_exp1(n1, n2, rate2[i])
      lookobj_new <- lookout::lookout(
        X[, 1:2],
        alpha = 0.01,
        scale = scale,
        gamma = 0.98,
        old_version = FALSE
      )
      lookobj_old <- lookout::lookout(
        X[, 1:2],
        alpha = 0.01,
        scale = scale,
        gamma = 1,
        old_version = TRUE
      )
      act <- X$Points == "Anomaly"
      results_new[kk, ] <- c(
        rate2[i],
        n2,
        diff_metrics(act, which_outliers(lookobj_new))
      )
      results_old[kk, ] <- c(
        rate2[i],
        n2,
        diff_metrics(act, which_outliers(lookobj_old))
      )
      kk <- kk + 1
    }
  }
  # Process the results data directly
  results_combined <- bind_rows(
    results_new |>
      mutate(method = "New lookout"),
    results_old |>
      mutate(method = "Old lookout")
  ) |>
    select(
      method,
      outrate,
      true_positive_rate,
      true_negative_rate,
      false_positive_rate,
      false_negative_rate
    ) |>
    rename(
      "True Positive Rate" = true_positive_rate,
      "False Positive Rate" = false_positive_rate,
      "False Negative Rate" = false_negative_rate,
      "True Negative Rate" = true_negative_rate
    ) |>
    pivot_longer(cols = 3:6, names_to = "metric", values_to = "value")

  results_combined
}

create_figure_exp1 <- function(results) {
  set_ggplot_options()
  g1 <- ggplot(results, aes(x = outrate, y = value, color = method)) +
    geom_jitter(width = 0.05, height = 0, alpha = 0.4) +
    geom_smooth() +
    facet_grid(metric ~ method) +
    labs(x = "Anomaly Rate", y = "Count") +
    theme(legend.position = "none") +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2))

  # Generate random data for plotting
  n1 <- 500
  n2 <- 10
  rate2 <- c(0.2, 0.9)

  df <- bind_rows(
    generate_exp1(n1, n2, rate2[1]),
    generate_exp1(n1, n2, rate2[2])
  ) |>
    mutate(
      rate = paste("rate =", rep(rate2, each = n1 + n2)),
    )

  g2 <- ggplot(df, aes(X1, X2, color = Points)) +
    geom_point() +
    facet_wrap(~rate, strip.position = "top", nrow = 2) +
    theme(legend.position = "left") +
    coord_fixed()

  # Create figure
  dir.create("Figures", showWarnings = FALSE)
  fig <- here::here("Figures/Exp1_Gamma_Rates.pdf")
  cairo_pdf(file = fig, width = 8, height = 6)
  print(patchwork::wrap_plots(g2, g1, nrow = 1))
  crop::dev.off.crop(fig)

  fig
}
