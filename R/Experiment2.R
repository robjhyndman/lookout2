# Experiment 2: Normal distribution

generate_exp2 <- function(n1, n2, mm) {
  out <- rbind(
    data.frame(x = rnorm(n1), y = rnorm(n1)),
    data.frame(
      x = rnorm(n2, mean = mm, sd = 0.2),
      y = rnorm(n2, mean = mm, sd = 0.2)
    )
  )
  colnames(out) <- c("X1", "X2")
  out$Points <- c(rep("Non-anomaly", n1), rep("Anomaly", n2))
  as_tibble(out)
}

run_synthetic_exp2 <- function() {
  reps <- 20
  mm_seq <- seq(2.5, 4, by = 0.25)
  n1 <- 1000
  n2 <- 10
  bw <- 0.98

  results_old <- results_new <- tibble(
    mean = numeric(reps * length(mm_seq)),
    outliers = 0
  ) |>
    bind_cols(set_up_diff_metrics(reps * length(mm_seq)))

  kk <- 1
  for (jj in seq_len(reps)) {
    for (i in seq_along(mm_seq)) {
      mm <- mm_seq[i]
      # Generate random data
      X <- generate_exp2(n1, n2, mm)
      lookobj_new <- lookout::lookout(
        X[, 1:2],
        alpha = 0.01,
        scale = TRUE,
        gamma = 0.98,
        old_version = FALSE
      )
      lookobj_old <- lookout::lookout(
        X[, 1:2],
        alpha = 0.01,
        scale = TRUE,
        gamma = 1,
        old_version = TRUE
      )
      act <- X$Points == "Anomaly"
      results_new[kk, ] <- c(
        mm,
        n2,
        diff_metrics(act, which_outliers(lookobj_new))
      )
      results_old[kk, ] <- c(
        mm,
        n2,
        diff_metrics(act, which_outliers(lookobj_old))
      )
      kk <- kk + 1
    }
  }

  bind_rows(
    results_new |>
      mutate(method = "New lookout"),
    results_old |>
      mutate(method = "Old lookout")
  ) |>
    select(
      method,
      mean,
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
}

create_figure_exp2 <- function(results) {
  set_ggplot_options()
  g1 <- ggplot(results, aes(x = mean, y = value, color = method)) +
    geom_jitter(width = 0.05, height = 0, alpha = 0.4) +
    geom_smooth() +
    facet_grid(metric ~ method) +
    labs(x = "Mean", y = "Rate") +
    theme(legend.position = "none") +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2))

  # Generate random data for plotting
  n1 <- 1000
  n2 <- 10
  mm <- c(2.5, 3.75)

  df <- bind_rows(
    generate_exp2(n1, n2, mm[1]),
    generate_exp2(n1, n2, mm[2])
  ) |>
    mutate(Mean = paste("Mean =", sprintf("%.2f", rep(mm, each = n1 + n2))))

  g2 <- df |>
    ggplot() +
    aes(X1, X2, color = Points) +
    geom_point() +
    facet_wrap(~Mean, strip.position = "top", nrow = 2) +
    theme(legend.position = "left") +
    coord_fixed()

  # Create figure
  dir.create("Figures", showWarnings = FALSE)
  fig <- here::here("Figures/Exp2_Normal_Rates.pdf")
  cairo_pdf(file = fig, width = 8, height = 6)
  print(patchwork::wrap_plots(g2, g1, nrow = 1))
  crop::dev.off.crop(fig)

  fig
}
