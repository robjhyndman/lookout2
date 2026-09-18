# Experiment 3: Comparison with other methods

generate_exp3 <- function(iterate) {
  Y <- data.frame(
    Y2 = rnorm(405),
    Y3 = rnorm(405),
    Y4 = rnorm(405),
    Y5 = rnorm(405),
    Y6 = rnorm(405)
  )
  Y1_1 <- rnorm(400)
  Y1_2 <- rnorm(5, mean = 2 + (iterate - 1) * 0.5, sd = 0.2)
  Y$Y1 <- c(Y1_1, Y1_2)
  Y$Points <- c(rep("Non-anomaly", 400), rep("Anomaly", 5))
  as_tibble(Y)
}

run_synthetic_exp3 <- function(reps, pp, scale, alpha, beta, gamma) {
  compare_algorithms(3, generate_exp3, reps, pp, scale, alpha, beta, gamma)
}

create_figure_exp3 <- function(results) {
  # Plot data
  X <- generate_exp3(3) |>
    mutate(alpha = 0.4 + 0.6 * (Points == "Anomaly"))
  g1 <- X |>
    ggplot(aes(Y1, Y2)) +
    geom_point(aes(color = Points), alpha = X$alpha, size = 0.75) +
    scale_color_manual(
      values = c("Non-anomaly" = "#999999", "Anomaly" = "red"),
      name = "Points"
    )
  X <- generate_exp3(9) |>
    mutate(alpha = 0.4 + 0.6 * (Points == "Anomaly"))
  g2 <- X |>
    ggplot(aes(Y1, Y2)) +
    geom_point(aes(color = Points), alpha = X$alpha, size = 0.75) +
    scale_color_manual(
      values = c("Non-anomaly" = "#999999", "Anomaly" = "red"),
      name = "Points"
    )
  g3 <- X |>
    ggplot(aes(Y3, Y4)) +
    geom_point(aes(color = Points), alpha = X$alpha, size = 0.75) +
    scale_color_manual(
      values = c("Non-anomaly" = "#999999", "Anomaly" = "red"),
      name = "Points"
    )

  p <- patchwork::wrap_plots(g1, g2, patchwork::guide_area(), g3) +
    patchwork::plot_layout(guides = "collect")

  create_figure_comparison(3, p, results)
}
