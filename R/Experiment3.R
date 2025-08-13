# Experiment 3: Increasing N with normal distribution

generate_exp3 <- function(nn) {
  num_outliers <- ceiling(5 / 1000 * nn)
  meanx <- runif(num_outliers, min = -sqrt(2) * 2.2, max = sqrt(2) * 2.2)
  meany <- sqrt(2 * 2.2^2 - meanx^2)
  inds <- sample(seq(num_outliers), ceiling(num_outliers / 2))
  meany[inds] <- -1 * meany[inds]

  out <- bind_rows(
    data.frame(
      x = rnorm(nn),
      y = rnorm(nn)
    ),
    data.frame(
      x = rnorm(num_outliers, mean = meanx, sd = 0.1), # meanx
      y = rnorm(num_outliers, mean = meany, sd = 0.1) # meany
    )
  )
  colnames(out) <- c("X1", "X2")
  out$Points <- c(rep("Non-anomaly", nn), rep("Anomaly", num_outliers))
  as_tibble(out)
}

run_synthetic_exp3 <- function(scale = scale) {
  nnvals <- seq(10) * 1000
  nnvals <- rep(nnvals, each = 10)
  df3 <- df1 <- set_up_diff_metrics(length(nnvals))

  for (ii in seq_along(nnvals)) {
    X <- generate_exp3(nnvals[ii])
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

create_figure_exp3 <- function(results) {
  g1 <- ggplot(results, aes(x = N, y = value, color = Algorithm)) +
    geom_jitter(width = 1000 / 4, height = 0, alpha = 0.4, size = 0.75) +
    facet_wrap(name ~ .) +
    labs(x = "Number of points (n)", y = "Anomaly rate") +
    geom_smooth()

  # Generate Data to plot
  nn <- 10000
  df <- generate_exp3(nn) |>
    mutate(alpha = 0.4 + 0.6 * (Points == "Anomaly"))
  g2 <- ggplot(df, aes(X1, X2, color = Points)) +
    geom_point(alpha = df$alpha, size = 1) +
    coord_fixed() +
    scale_color_manual(
      values = c("Non-anomaly" = "#999999", "Anomaly" = "red"),
      name = "Points"
    )
  experiment_plot(g2, g1, "Exp3.pdf")
}
