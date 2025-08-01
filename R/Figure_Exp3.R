library(ggplot2)
library(dplyr)
library(tidyr)
source(here::here("R/functions.R"))

# ------------------------------------------------------------------------------
# TASK 03: EXP3 - AS N INCREASES - NORMAL DISTRIBUTION
# ------------------------------------------------------------------------------

results <- read.csv(
  here::here(
    "Data_Output/Synthetic_Exp3_Increasing_Increasing_N_Normal.csv"
  )
) |>
  select(
    Algo,
    N,
    true_positive_rate,
    true_negative_rate,
    false_positive_rate,
    false_negative_rate
  ) |>
  rename(
    "True Positive Rate" = true_positive_rate,
    "False Positive Rate" = false_positive_rate,
    "False Negative Rate" = false_negative_rate,
    "True Negative Rate" = true_negative_rate
  ) |>
  pivot_longer(cols = 3:6)

g1 <- ggplot(results, aes(x = N, y = value, color = Algo)) +
  geom_point(size = 0.5) +
  facet_grid(~name) +
  xlab("Number of points") +
  ylab("Rate") +
  geom_smooth() +
  theme(legend.position = "bottom")

# -----------------------------------------------------------------------------
# Generate Data to plot
nn <- 10000
df <- generate_exp3(nn)
g2 <- ggplot(df, aes(X1, X2, color = Points)) +
  geom_point() +
  theme(legend.position = "bottom") +
  coord_fixed() +
  labs(x = "x", y = "y")

fig <- here::here("Figures/Exp3_N_Increases_Normal.pdf")
cairo_pdf(file = fig, width = 6.6, height = 3.5)
print(g1)
crop::dev.off.crop(fig)

fig <- here::here("Figures/Exp3_Data.pdf")
cairo_pdf(file = fig, width = 3.5, height = 3.5)
print(g2)
crop::dev.off.crop(fig)
