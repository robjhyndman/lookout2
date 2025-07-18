# --------------------------------------------------------------------------
# TASK 1: OLD FAITHFUL DATA
# TASK 2: WINE QUALITY AND PRICE
# --------------------------------------------------------------------------

library(lookout)
library(weird)
library(ggplot2)
library(gridExtra)
library(stray)
library(HDoutliers)


# --------------------------------------------------------------------------
# TASK 1: OLD FAITHFUL DATA
# --------------------------------------------------------------------------
data("oldfaithful")

oldfaithful2 <- oldfaithful |>
  filter(duration < 6000)

oldfaithful2 <- oldfaithful |>
  filter(duration < 7200, waiting < 7200)

ggplot(oldfaithful2, aes(x = duration, y = waiting)) +
  geom_point(alpha = 0.5) +
  labs(y = "Waiting time to next eruption (seconds)", x = "Duration (seconds)")


lookobjNew <- lookout::lookout(
  oldfaithful2[, 2:3],
  alpha = 0.05,
  unitize = TRUE,
  normalize = FALSE,
  bw_para = 0.98,
  version = 2,
  bw_power = NA
)
lookobjNew
g1 <- autoplot(lookobjNew) +
  ggtitle("New lookout")

lookobjOld <- lookout::lookout(
  oldfaithful2[, 2:3],
  alpha = 0.05,
  unitize = TRUE,
  normalize = FALSE,
  bw_para = 1,
  version = 1,
  bw_power = NA
)


lookobjOld
g2 <- autoplot(lookobjOld) +
  ggtitle("Old lookout")


strayout <- strayout <- stray::find_HDoutliers(oldfaithful2[, 2:3]) #, knnsearchtype = "kd_tree", alpha=0.05)
# Stray gives weird results  - 1022 anomalies
grid.arrange(g1, g2, nrow = 1)


# --------------------------------------------------------------------------
# TASK 2: WINE QUALITY AND PRICE
# --------------------------------------------------------------------------
wine_reviews <- fetch_wine_reviews()
wine_reviews2 <- wine_reviews |>
  filter(variety %in% c("Shiraz", "Syrah")) |>
  select(points, price)
wine_reviews |>
  filter(variety %in% c("Shiraz", "Syrah")) |>
  select(points, price) |>
  ggplot(aes(y = price, x = points)) +
  geom_jitter(height = 0, width = 0.3, alpha = 0.5) +
  scale_y_log10()

lookobjNew <- lookout::lookout(
  wine_reviews2,
  alpha = 0.05,
  unitize = TRUE,
  normalize = FALSE,
  bw_para = 0.98,
  version = 2,
  bw_power = NA
)
lookobjNew


g1 <- autoplot(lookobjNew) +
  ggtitle("New lookout")


lookobjOld <- lookout::lookout(
  wine_reviews2,
  alpha = 0.05,
  unitize = TRUE,
  normalize = FALSE,
  bw_para = 1,
  version = 1,
  bw_power = NA
)

lookobjOld

strayout <- stray::find_HDoutliers(wine_reviews2) #, knnsearchtype = "kd_tree", alpha=0.05)
# Stray gives weird results  - 1887 anomalies

g2 <- autoplot(lookobjOld) +
  ggtitle("Old lookout")

grid.arrange(g1, g2, nrow = 1)
