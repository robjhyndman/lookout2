analyze_wine_data <- function(scale = scale) {
  wine_reviews <- weird::fetch_wine_reviews()
  wine_reviews2 <- wine_reviews |>
    filter(variety %in% c("Shiraz", "Syrah")) |>
    select(points, price)

  lookobjNew <- lookout::lookout(
    wine_reviews2,
    alpha = 0.01,
    scale = scale,
    gamma = 0.98,
    old_version = FALSE
  )

  lookobjOld <- lookout::lookout(
    wine_reviews2,
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

create_wine_figure <- function(results) {
  set_ggplot_options()
  dir.create("Figures", showWarnings = FALSE)
  fig <- here::here("Figures/wine_reviews.pdf")
  cairo_pdf(file = fig, width = 8, height = 4)
  print(
    results |>
      ggplot(aes(x = points, y = price, color = !outliers)) +
      geom_point() +
      facet_wrap(~method, nrow = 1) +
      guides(color = "none")
  )
  crop::dev.off.crop(fig)

  fig
}
