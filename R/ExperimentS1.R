# Experiment S1: Normal distribution

generate_expS1 <- function(n1, n2, mm) {
  out <- rbind(
    data.frame(x = rnorm(n1), y = rnorm(n1)),
    data.frame(
      x = rnorm(n2, mean = mm, sd = 1),
      y = rnorm(n2, mean = mm, sd = 1)
    )
  )
  colnames(out) <- c("Y1", "Y2")
  out$Points <- c(rep("Non-anomaly", n1), rep("Anomaly", n2))
  as_tibble(out)
}

run_synthetic_expS1 <- function(scale, alpha, beta, gamma) {
  reps <- 20
  mm_seq <- seq(2.5, 4, by = 0.25)
  n1 <- 1000
  n2 <- 10

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
      X <- generate_expS1(n1, n2, mm)
      lookobj_new <- lookout::lookout(
        X[, 1:2],
        scale = scale,
        alpha = alpha,
        beta = beta,
        gamma = gamma,
        fast = FALSE,
        old_version = FALSE
      )
      lookobj_old <- lookout::lookout(
        X[, 1:2],
        alpha = alpha,
        beta = beta,
        scale = scale,
        fast = FALSE,
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
    results_new |> mutate(method = "New Lookout"),
    results_old |> mutate(method = "Old Lookout")
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

create_figure_expS1 <- function(results) {
  smooth <- results |>
    group_by(method, metric) |>
    reframe(fit = truncated_smooth(mean, value)) |>
    tidyr::unnest(fit)

  g1 <- ggplot(results, aes(x = mean)) +
    geom_jitter(
      aes(y = value, color = method),
      width = 0.25 / 4,
      height = 0,
      alpha = 0.4,
      size = 0.75
    ) +
    geom_ribbon(
      data = smooth,
      aes(x = x, ymin = lower, ymax = upper, group = method),
      alpha = 0.2,
      fill = "#555555"
    ) +
    geom_line(
      data = smooth,
      aes(x = x, y = fit, color = method, group = method),
      linewidth = 1,
    ) +
    facet_wrap(metric ~ .) +
    labs(x = "Mean (µ)", y = "Anomaly rate") +
    guides(color = guide_legend(title = "Algorithm")) #+
  #scale_y_continuous(breaks = seq(0, 1, by = 0.2))

  # Generate random data for plotting
  n1 <- 1000
  n2 <- 10
  mm <- c(2.5, 3.75)

  df <- bind_rows(
    generate_expS1(n1, n2, mm[1]),
    generate_expS1(n1, n2, mm[2])
  ) |>
    mutate(
      Mean = paste("µ =", sprintf("%.2f", rep(mm, each = n1 + n2))),
      alpha = 0.4 + 0.6 * (Points == "Anomaly")
    )

  g2 <- df |>
    ggplot() +
    aes(Y1, Y2, color = Points) +
    geom_point(alpha = df$alpha, size = 0.75) +
    facet_wrap(Mean ~ ., strip.position = "top", nrow = 1) +
    coord_fixed() +
    scale_color_manual(
      values = c("Non-anomaly" = "#999999", "Anomaly" = "red"),
      name = "Points"
    )

  experiment_plot(g2, g1, "ExpS1_Normal_Rates.pdf")
}
