analyze_old_faithful <- function(scale, alpha, beta, gamma) {
  oldfaithful2 <- weird::oldfaithful |>
    filter(duration < 7200, waiting < 7200)

  lookobjNew <- lookout::lookout(
    oldfaithful2[, c("duration", "waiting")],
    scale = scale,
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    fast = FALSE,
    old_version = FALSE
  )

  lookobjOld <- lookout::lookout(
    oldfaithful2[, c("duration", "waiting")],
    alpha = alpha,
    beta = beta,
    scale = scale,
    fast = FALSE,
    old_version = TRUE
  )

  bind_rows(
    as.data.frame(lookobjNew) |>
      mutate(method = "New Lookout"),
    as.data.frame(lookobjOld) |>
      mutate(method = "Old Lookout")
  )
}

create_old_faithful_figure <- function(results) {
  set_ggplot_options()

  df <- results |>
    mutate(alpha = 0.4 + 0.6 * as.numeric(outliers))
  dir.create("Figures", showWarnings = FALSE)
  fig <- here::here("Figures/old_faithful.pdf")
  cairo_pdf(file = fig, width = 8, height = 4)
  print(
    df |>
      ggplot(aes(x = duration, y = waiting, color = outliers)) +
      geom_point(alpha = df$alpha) +
      facet_wrap(~method, nrow = 1) +
      scale_color_manual(
        values = c(`FALSE` = "#999999", `TRUE` = "red")
      ) +
      guides(color = "none")
  )
  crop::dev.off.crop(fig)

  fig
}
