# Experiment 5: High-dimensional comparison

generate_exp5 <- function(iterate) {
  nn <- 500
  dd <- 19
  X <- matrix(runif(nn * (dd + 1)), ncol = dd + 1, nrow = nn)
  colnames(X) <- paste("x", seq(dd + 1), sep = "")
  X[nn, seq(iterate)] <- rep(0.9, iterate)
  X <- as_tibble(X)
  X$Points <- c(rep("Non-anomaly", nn - 1), rep("Anomaly", 1))
  X
}

run_synthetic_exp5 <- function(reps, pp, scale, alpha, beta, gamma) {
  compare_algorithms(5, generate_exp5, reps, pp, scale, alpha, beta, gamma)
}

create_figure_exp5 <- function(results) {
  # plot data
  df <- bind_rows(
    generate_exp5(5) |> as_dobin() |> mutate(iterate = 5),
    generate_exp5(12) |> as_dobin() |> mutate(iterate = 12),
    generate_exp5(20) |> as_dobin() |> mutate(iterate = 20)
  ) |>
    mutate(
      alpha = 0.4 + 0.6 * (labels == "Anomaly"),
      iterate = factor(
        paste("Iteration", iterate),
        levels = paste("Iteration", c(5, 12, 20))
      )
    )

  p <- df |>
    ggplot() +
    aes(D1, D2) +
    geom_point(aes(color = labels), alpha = df$alpha, size = 0.75) +
    facet_wrap(~iterate, strip.position = "right", ncol = 1) +
    theme(legend.position = "bottom") +
    scale_color_manual(
      values = c("Non-anomaly" = "#999999", "Anomaly" = "red"),
      name = "Points"
    )

  create_figure_comparison(5, p, results)
}
