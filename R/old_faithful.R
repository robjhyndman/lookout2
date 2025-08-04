analyze_old_faithful <- function(scale = scale) {
  oldfaithful2 <- weird::oldfaithful |>
    filter(duration < 7200, waiting < 7200)

  lookobjNew <- lookout::lookout(
    oldfaithful2[, 2:3],
    alpha = 0.01,
    scale = scale,
    gamma = 0.98,
    old_version = FALSE
  )

  lookobjOld <- lookout::lookout(
    oldfaithful2[, 2:3],
    alpha = 0.01,
    scale = scale,
    gamma = 1,
    old_version = TRUE
  )

  bind_rows(
    as.data.frame(lookobjNew) |>
      mutate(method = "New lookout"),
    as.data.frame(lookobjOld) |>
      mutate(method = "Old lookout")
  )
}

create_old_faithful_figure <- function(results) {
  set_ggplot_options()

  dir.create("Figures", showWarnings = FALSE)
  fig <- here::here("Figures/old_faithful.pdf")
  cairo_pdf(file = fig, width = 8, height = 4)
  print(
    results |>
      ggplot(aes(x = duration, y = waiting, color = !outliers)) +
      geom_point() +
      facet_wrap(~method, nrow = 1) +
      guides(color = "none")
  )
  crop::dev.off.crop(fig)

  fig
}
