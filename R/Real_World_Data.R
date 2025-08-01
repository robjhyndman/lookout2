# --------------------------------------------------------------------------
# TASK 1: OLD FAITHFUL DATA
# TASK 2: WINE QUALITY AND PRICE
# --------------------------------------------------------------------------

library(lookout)
library(weird)
library(ggplot2)
library(stray)
library(HDoutliers)
source(here::here("R/functions.R"))

# --------------------------------------------------------------------------
# TASK 1: OLD FAITHFUL DATA
# --------------------------------------------------------------------------
oldfaithful2 <- oldfaithful |>
  filter(duration < 7200, waiting < 7200)

lookobjNew <- lookout::lookout(
  oldfaithful2[, 2:3],
  alpha = 0.01,
  scale = TRUE,
  gamma = 0.98,
  old_version = FALSE
)

lookobjOld <- lookout::lookout(
  oldfaithful2[, 2:3],
  alpha = 0.01,
  scale = TRUE,
  gamma = 1,
  old_version = TRUE
)

df <- bind_rows(
  as.data.frame(lookobjNew) |>
    mutate(method = "New lookout"),
  as.data.frame(lookobjOld) |>
    mutate(method = "Old lookout")
)

fig <- here::here("Figures/old_faithful.pdf")
cairo_pdf(file = fig, width = 8, height = 4)
df |>
  ggplot(aes(x = duration, y = waiting, color = !outliers)) +
  geom_point() +
  facet_wrap(~method, nrow = 1) +
  guides(color = "none")
crop::dev.off.crop(fig)

# --------------------------------------------------------------------------
# TASK 2: WINE QUALITY AND PRICE
# --------------------------------------------------------------------------
wine_reviews <- fetch_wine_reviews()
wine_reviews2 <- wine_reviews |>
  filter(variety %in% c("Shiraz", "Syrah")) |>
  select(points, price)

lookobjNew <- lookout::lookout(
  wine_reviews2,
  alpha = 0.01,
  scale = TRUE,
  gamma = 0.98,
  old_version = FALSE
)

lookobjOld <- lookout::lookout(
  wine_reviews2,
  alpha = 0.01,
  scale = TRUE,
  gamma = 1,
  old_version = TRUE
)


df <- bind_rows(
  as.data.frame(lookobjNew) |>
    mutate(method = "New lookout"),
  as.data.frame(lookobjOld) |>
    mutate(method = "Old lookout")
)

fig <- here::here("Figures/wine_reviews.pdf")
cairo_pdf(file = fig, width = 8, height = 4)
df |>
  ggplot(aes(x = points, y = price, color = !outliers)) +
  geom_point() +
  facet_wrap(~method, nrow = 1) +
  guides(color = "none")
crop::dev.off.crop(fig)
