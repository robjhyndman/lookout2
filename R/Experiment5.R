# Experiment 5: Comparison with other methods

generate_exp5 <- function(iterate) {
  X <- data.frame(
    X2 = rnorm(405),
    X3 = rnorm(405),
    X4 = rnorm(405),
    X5 = rnorm(405),
    X6 = rnorm(405)
  )
  X1_1 <- rnorm(400)
  X1_2 <- rnorm(5, mean = 2 + (iterate - 1) * 0.5, sd = 0.2)
  X$X1 <- c(X1_1, X1_2)
  X$Points <- c(rep("Non-anomaly", 400), rep("Anomaly", 5))
  as_tibble(X)
}

run_synthetic_exp5 <- function(reps, pp, scale) {
  compare_algorithms(5, generate_exp5, reps, pp, scale)
}

create_figure_exp5 <- function(results) {
  # Plot data
  X <- generate_exp5(3) |>
    mutate(alpha = 0.4 + 0.6 * (Points == "Anomaly"))
  g1 <- X |>
    ggplot(aes(X1, X2)) +
    geom_point(aes(color = Points), alpha = X$alpha, size = 0.75) +
    scale_color_manual(
      values = c("Non-anomaly" = "#999999", "Anomaly" = "red"),
      name = "Points"
    )
  X <- generate_exp5(9) |>
    mutate(alpha = 0.4 + 0.6 * (Points == "Anomaly"))
  g2 <- X |>
    ggplot(aes(X1, X2)) +
    geom_point(aes(color = Points), alpha = X$alpha, size = 0.75) +
    scale_color_manual(
      values = c("Non-anomaly" = "#999999", "Anomaly" = "red"),
      name = "Points"
    )
  g3 <- X |>
    ggplot(aes(X3, X4)) +
    geom_point(aes(color = Points), alpha = X$alpha, size = 0.75) +
    scale_color_manual(
      values = c("Non-anomaly" = "#999999", "Anomaly" = "red"),
      name = "Points"
    )

  p <- patchwork::wrap_plots(g1, g2, patchwork::guide_area(), g3) +
    patchwork::plot_layout(guides = "collect")

  create_figure_exp567(5, p, results)
}
