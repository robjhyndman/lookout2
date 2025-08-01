library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
source(here::here("R/functions.R"))

# ------------------------------------------------------------------------------
# TASK 01: EXP1 - GAMMA DISTRIBUTION TWO SEPARATE DISTRIBUTIONS
# ------------------------------------------------------------------------------

results <- bind_rows(
  read_csv(
    here::here("Data_Output/Synthetic_Exp1_gamma_new_lookout.csv")
  ) |>
    mutate(method = "New lookout"),
  read_csv(
    here::here("Data_Output/Synthetic_Exp1_gamma_old_lookout.csv")
  ) |>
    mutate(method = "Old lookout")
) |>
  select(
    method,
    outrate,
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
  pivot_longer(cols = 3:6, names_to = "metric", values_to = "value")

g1 <- ggplot(results, aes(x = outrate, y = value, color = method)) +
  geom_jitter(width = 0.05, height = 0, alpha = 0.4) +
  geom_smooth() +
  facet_grid(metric ~ method) +
  labs(x = "Anomaly Rate", y = "Count") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2))

# ------------------------------------------------------------------------------
# PLOTTING THE DATA
# Generate random data

n1 <- 500
n2 <- 10
shape1 <- 2
shape2 <- 2
rate1 <- 2
rate2 <- c(0.2, 0.9)

df <- bind_rows(
  generate_exp1(n1, n2, rate2[1]),
  generate_exp1(n1, n2, rate2[2])
) |>
  mutate(
    rate = paste("rate =", rep(rate2, each = n1 + n2)),
  )

g2 <- ggplot(df, aes(X1, X2, color = Points)) +
  geom_point() +
  facet_wrap(~rate, strip.position = "top", nrow = 2) +
  theme(legend.position = "left") +
  coord_fixed()

fig <- here::here("Figures/Exp1_Gamma_Rates.pdf")
cairo_pdf(file = fig, width = 8, height = 6)
print(patchwork::wrap_plots(g2, g1, nrow = 1))
crop::dev.off.crop(fig)
