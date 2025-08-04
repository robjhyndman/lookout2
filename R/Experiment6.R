# Experiment 6: Comparison with other methods (different setup)

generate_exp6 <- function(iterate) {
  nn <- 805
  r1 <- runif(nn)
  r2 <- rnorm(nn, mean = 5)
  theta <- 2 * pi * r1
  R2 <- 2
  dist <- r2 + R2
  X <- tibble(
    x1 = dist * cos(theta),
    x2 = dist * sin(theta),
    x3 = runif(nn)
  )
  X[nn - 5 + seq(5), 1] <- rnorm(5, 5 - (iterate - 1) * 0.5, sd = 0.1)
  X[nn - 5 + seq(5), 2] <- rnorm(5, 0, sd = 0.1)
  X$Points <- c(rep("Non-anomaly", nn - 5), rep("Anomaly", 5))
  X
}

run_synthetic_exp6 <- function(reps, pp, scale) {
  compare_algorithms(6, generate_exp6, reps, pp, scale)
}

create_figure_exp6 <- function(results) {
  # Plot data
  g1 <- generate_exp6(3) |>
    ggplot(aes(x1, x2)) +
    geom_point(aes(color = Points))
  X <- generate_exp6(9)
  g2 <- X |>
    ggplot(aes(x1, x2)) +
    geom_point(aes(color = Points))
  g3 <- X |>
    ggplot(aes(x2, x3)) +
    geom_point(aes(color = Points))

  p <- patchwork::wrap_plots(g1, g2, patchwork::guide_area(), g3) +
    patchwork::plot_layout(guides = "collect")

  create_figure_exp567(6, p, results)
}
