analyze_wine_data <- function(wine_reviews, scale, alpha, beta, gamma) {
  wine_reviews2 <- wine_reviews |>
    filter(variety %in% c("Shiraz", "Syrah")) |>
    select(points, price)

  lookobjNew <- lookout::lookout(
    wine_reviews2,
    scale = scale,
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    fast = TRUE,
    old_version = FALSE
  )

  lookobjOld <- lookout::lookout(
    wine_reviews2,
    scale = scale,
    alpha = alpha,
    beta = beta,
    fast = TRUE,
    old_version = TRUE
  )

  bind_rows(
    as.data.frame(lookobjNew) |>
      mutate(method = "New Lookout"),
    as.data.frame(lookobjOld) |>
      mutate(method = "Old Lookout")
  )
}

create_wine_figure <- function(results) {
  set_ggplot_options()
  df <- results |>
    mutate(alpha = 0.4 + 0.6 * as.numeric(outliers))
  dir.create("Figures", showWarnings = FALSE)
  fig <- here::here("Figures/wine_reviews.pdf")
  cairo_pdf(file = fig, width = 8, height = 4)
  print(
    df |>
      ggplot(aes(x = points, y = price, color = outliers)) +
      geom_point(alpha = df$alpha) +
      facet_wrap(~method, nrow = 1) +
      scale_color_manual(
        values = c(`FALSE` = "#999999", `TRUE` = "red"),
      ) +
      guides(color = "none")
  )
  crop::dev.off.crop(fig)

  fig
}
