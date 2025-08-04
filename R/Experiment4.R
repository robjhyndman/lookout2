# Experiment 4: Increasing N with gamma distribution

generate_exp4 <- function(nn) {
  num_outliers <- ceiling(5 / 1000 * nn)
  X1 <- matrix(rgamma(n = 2 * nn, shape = 2, rate = 2), ncol = 2)
  X2 <- matrix(rgamma(n = 2 * nn, shape = 2.2, rate = 2), ncol = 2)
  x1dist <- apply(X1, 1, function(x) sqrt(x[1]^2 + x[2]^2))
  qq <- quantile(x1dist, probs = 0.99)
  x2dist <- apply(X2, 1, function(x) sqrt(x[1]^2 + x[2]^2))
  inds <- which(x2dist > qq)
  inds2 <- sample(inds, num_outliers)
  out <- as.data.frame(rbind(X1, X2[inds2, ]))
  colnames(out) <- c("X1", "X2")
  out$Points <- c(
    rep("Non-anomaly", NROW(X1)),
    rep("Anomaly", length(inds2))
  )
  as_tibble(out)
}

run_synthetic_exp4 <- function(scale = scale) {
  nnvals <- rep((1:10) * 1000, each = 10)
  df3 <- df1 <- df2 <- set_up_diff_metrics(length(nnvals))

  for (ii in 1:length(nnvals)) {
    X <- generate_exp4(nnvals[[ii]])
    lookobj_new <- lookout::lookout(
      X[, 1:2],
      alpha = 0.01,
      scale = scale,
      gamma = 0.95,
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
    df1[ii, ] <- diff_metrics(act, which_outliers(lookobj_new))
    df3[ii, ] <- diff_metrics(act, which_outliers(lookobj_old))
  }

  bind_rows(
    df1 |> mutate(Algorithm = "New Lookout", N = nnvals),
    df3 |> mutate(Algorithm = "Old Lookout", N = nnvals)
  ) |>
    relocate(Algorithm) |>
    select(
      Algorithm,
      N,
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
    pivot_longer(cols = 3:6)
}

create_figure_exp4 <- function(results) {
  set_ggplot_options()
  dir.create("Figures", showWarnings = FALSE)
  fig1 <- here::here("Figures/Exp4_N_Increases_Gamma.pdf")
  cairo_pdf(file = fig1, width = 6.6, height = 3.5)
  print(
    ggplot(results, aes(x = N, y = value, color = Algorithm)) +
      geom_jitter(size = 0.75, width = 1000 / 4, height = 0.0, alpha = 0.4) +
      facet_grid(~name) +
      xlab("Number of points") +
      ylab("Rate") +
      geom_smooth() +
      theme(legend.position = "bottom")
  )
  crop::dev.off.crop(fig1)

  # Generate Data to plot
  nn <- 10000
  df_data <- generate_exp4(nn) |>
    mutate(alpha = 0.4 + 0.6 * (Points == "Anomaly"))

  g2 <- ggplot(df_data, aes(X1, X2, color = Points)) +
    geom_point(alpha = df_data$alpha, size = 1) +
    theme(legend.position = "bottom") +
    coord_fixed() +
    labs(x = "x", y = "y") +
    scale_color_manual(
      values = c("Non-anomaly" = "#999999", "Anomaly" = "red"),
      name = "Points"
    )

  fig2 <- here::here("Figures/Exp4_Data.pdf")
  cairo_pdf(file = fig2, width = 3.5, height = 3.5)
  print(g2)
  crop::dev.off.crop(fig2)

  c(fig1, fig2)
}
