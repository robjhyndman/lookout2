# Experiment 4: Comparison with other methods (different setup)

generate_exp4 <- function(iterate) {
  nn <- 805
  r1 <- runif(nn)
  r2 <- rnorm(nn, mean = 5)
  theta <- 2 * pi * r1
  R2 <- 2
  dist <- r2 + R2
  X <- tibble(
    Y1 = dist * cos(theta),
    Y2 = dist * sin(theta),
    Y3 = runif(nn)
  )
  X[nn - 5 + seq(5), 1] <- rnorm(5, 5 - (iterate - 1) * 0.5, sd = 0.1)
  X[nn - 5 + seq(5), 2] <- rnorm(5, 0, sd = 0.1)
  X$Points <- c(rep("Non-anomaly", nn - 5), rep("Anomaly", 5))
  X
}

run_synthetic_exp4 <- function(reps, pp, scale, alpha, beta, gamma) {
  compare_algorithms(4, generate_exp4, reps, pp, scale, alpha, beta, gamma)
}

create_figure_exp4 <- function(results) {
  # Plot data
  X <- generate_exp4(3) |>
    mutate(alpha = 0.4 + 0.6 * (Points == "Anomaly"))
  g1 <- X |>
    ggplot(aes(Y1, Y2)) +
    geom_point(aes(color = Points), alpha = X$alpha, size = 0.75) +
    scale_color_manual(
      values = c("Non-anomaly" = "#999999", "Anomaly" = "red"),
      name = "Points"
    )
  X <- generate_exp4(9) |>
    mutate(alpha = 0.4 + 0.6 * (Points == "Anomaly"))
  g2 <- X |>
    ggplot(aes(Y1, Y2)) +
    geom_point(aes(color = Points), alpha = X$alpha, size = 0.75) +
    scale_color_manual(
      values = c("Non-anomaly" = "#999999", "Anomaly" = "red"),
      name = "Points"
    )
  g3 <- X |>
    ggplot(aes(Y2, Y3)) +
    geom_point(aes(color = Points), alpha = X$alpha, size = 0.75) +
    scale_color_manual(
      values = c("Non-anomaly" = "#999999", "Anomaly" = "red"),
      name = "Points"
    )

  p <- patchwork::wrap_plots(g1, g2, patchwork::guide_area(), g3) +
    patchwork::plot_layout(guides = "collect")

  create_figure_comparison(4, p, results)
}
