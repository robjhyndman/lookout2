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
    df1 |> mutate(Algo = "New_Lookout", N = nnvals),
    df3 |> mutate(Algo = "Old_Lookout", N = nnvals)
  ) |>
    select(
      Algo,
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
  set_ggplot_options()

  g1 <- ggplot(results, aes(x = N, y = value, color = Algo)) +
    geom_point(size = 0.5) +
    facet_grid(~name) +
    xlab("Number of points") +
    ylab("Rate") +
    geom_smooth() +
    theme(legend.position = "bottom")

  # Generate Data to plot
  nn <- 10000
  df <- generate_exp3(nn)
  g2 <- ggplot(df, aes(X1, X2, color = Points)) +
    geom_point() +
    theme(legend.position = "bottom") +
    coord_fixed() +
    labs(x = "x", y = "y")

  # Create figures
  dir.create("Figures", showWarnings = FALSE)
  fig1 <- here::here("Figures/Exp3_N_Increases_Normal.pdf")
  cairo_pdf(file = fig1, width = 6.6, height = 3.5)
  print(g1)
  crop::dev.off.crop(fig1)

  fig2 <- here::here("Figures/Exp3_Data.pdf")
  cairo_pdf(file = fig2, width = 3.5, height = 3.5)
  print(g2)
  crop::dev.off.crop(fig2)

  c(fig1, fig2)
}
