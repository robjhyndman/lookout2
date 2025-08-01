library(ggplot2)
library(dplyr)
library(tidyr)
source(here::here("R/functions.R"))

# ------------------------------------------------------------------------------
# TASK 04: EXP4 - AS N INCREASES - GAMMA DISTRIBUTION
# ------------------------------------------------------------------------------

df <- read.csv(here::here(
  "Data_Output/Synthetic_Exp4_Increasing_N_Gamma.csv"
)) |>
  relocate(Algo) |>
  select(
    Algo,
    N,
    true_positive_rate,
    true_negative_rate,
    false_positive_rate,
    false_negative_rate
  ) |>
  pivot_longer(cols = 3:6)

fig <- here::here("Figures/Exp4_N_Increases_Gamma.pdf")
cairo_pdf(file = fig, width = 6.6, height = 3.5)
ggplot(df, aes(x = N, y = value, color = Algo)) +
  geom_point(size = 0.1) +
  geom_jitter(size = 0.1, height = 0.02) +
  facet_grid(~name) +
  xlab("Number of points") +
  ylab("Rate") +
  geom_smooth(se = FALSE) +
  theme(legend.position = "bottom")
crop::dev.off.crop(fig)

# -----------------------------------------------------------------------------
# Generate Data to plot
nn <- 10000
df <- generate_exp4(nn)

g2 <- ggplot(df, aes(X1, X2, color = Points)) +
  geom_point() +
  theme(legend.position = "bottom") +
  coord_fixed() +
  labs(x = "x", y = "y")

fig <- here::here("Figures/Exp4_Data.pdf")
cairo_pdf(file = fig, width = 3.5, height = 3.5)
print(g2)
crop::dev.off.crop(fig)
