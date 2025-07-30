# Need to install lookout from scaling branch
# remotes::install_github("sevvandi/lookout@scaling")

library(lookout)
library(tidyverse)
source(here::here("R/functions.R"))
set.seed(2024)

# Okabe-Ito colours
options(
  ggplot2.discrete.colour = c(
    "#D55E00",
    "#0072B2",
    "#009E73",
    "#CC79A7",
    "#E69F00",
    "#56B4E9",
    "#F0E442"
  ),
  ggplot2.discrete.fill = c(
    "#D55E00",
    "#0072B2",
    "#009E73",
    "#CC79A7",
    "#E69F00",
    "#56B4E9",
    "#F0E442"
  )
)
# Fira Sans font for graphics
ggplot2::theme_set(
  ggplot2::theme_get() +
    ggplot2::theme(text = ggplot2::element_text(family = "Fira Sans"))
)

# ------------------------------------------------------------------------------
# TASK 01:  GAMMA DISTRIBUTION OLD LOOKOUT AND NEW LOOKOUT
# ------------------------------------------------------------------------------

out_old <- gamma_outliers(
  n1 = 500,
  n2 = 10,
  shape1 = 2,
  shape2 = 2,
  rate1 = 2,
  rate2 = rep((1:4) / 10, 30),
  bw = 1,
  old_version = TRUE
)

out_new <- gamma_outliers(
  n1 = 500,
  n2 = 10,
  shape1 = 2,
  shape2 = 2,
  rate1 = 2,
  rate2 = rep((1:4) / 10, 30),
  bw = 0.95,
  old_version = FALSE
)

gamma_out <- bind_rows(
  out_old |> mutate(method = "Old lookout"),
  out_new |> mutate(method = "New lookout")
) |>
  relocate(method, ) |>
  select(-c(specificity)) |>
  rename(
    "True Positives" = true_pos,
    "False Positives" = false_pos,
    "False Negatives" = false_neg,
    "True Negatives" = true_neg
  ) |>
  pivot_longer(cols = 5:8)

p <- gamma_out |>
  ggplot(aes(as.factor(outrate), value, color = method)) +
  geom_jitter(width = 0.05, height = 0, alpha = 0.4) +
  facet_grid(name ~ method, scales = "free_y") +
  labs(
    x = "Anomaly rate",
    y = "Count"
  ) +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 500, by = 2))

fig <- here::here(paste0("Figures/Gamma_Experiment.pdf"))
cairo_pdf(file = fig, width = 8, height = 6)
print(p)
crop::dev.off.crop(fig)

write.csv(
  gamma_out,
  here::here("Data_Output/Outliers_Gamma.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------------------------
# TASK 02:  NORMAL DISTRIBUTION OLD LOOKOUT AND NEW LOOKOUT
# ------------------------------------------------------------------------------

reps <- 20
mm <- seq(2.5, 4, by = 0.25)
dfout_new <- dfout_old <- tibble(
  Algo = character(reps),
  mm = numeric(reps),
  N = numeric(reps),
  true_pos = numeric(reps),
  true_neg = numeric(reps),
  false_pos = numeric(reps),
  false_neg = numeric(reps),
  specificity = numeric(reps)
)

kk <- 1
for (ii in seq_along(mm)) {
  for (jj in seq_len(reps)) {
    out <- exp_normal(
      n1 = 1000,
      n2 = 10,
      mm = mm[ii],
      bw = 0.98,
      old_version = FALSE
    )
    dfout_new[kk, ] <- c("New lookout", mm[ii], out)
    kk <- kk + 1
  }
}

kk <- 1
for (ii in seq_along(mm)) {
  for (jj in seq_len(reps)) {
    out <- exp_normal(
      n1 = 1000,
      n2 = 10,
      mm = mm[ii],
      bw = 1,
      old_version = TRUE
    )
    dfout_old[kk, ] <- c("Old lookout", mm[ii], out)
    kk <- kk + 1
  }
}

dfout <- bind_rows(dfout_new, dfout_old)
write.csv(
  dfout,
  here::here("Data_Output/Outliers_Normal.csv"),
  row.names = FALSE
)

normal_out <- dfout |>
  select(-c(specificity)) |>
  rename(
    "True Positives" = true_pos,
    "False Positives" = false_pos,
    "False Negatives" = false_neg,
    "True Negatives" = true_neg
  ) |>
  pivot_longer(cols = 4:7)

p <- normal_out |>
  ggplot(aes(mm, value, color = Algo)) +
  geom_jitter(height = 0, width = 0.01, alpha = 0.4) +
  geom_smooth() +
  facet_grid(name ~ Algo, scales = "free_y") +
  xlab("Mean of anomaly distribution") +
  ylab("Count") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 1000, by = 2))

fig <- here::here(paste0("Figures/Normal_Comparison_Old_New.pdf"))
cairo_pdf(file = fig, width = 8, height = 6)
print(p)
crop::dev.off.crop(fig)
