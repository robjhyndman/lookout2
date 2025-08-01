library(ggplot2)
library(dplyr)
library(patchwork)
source(here::here("R/functions.R"))

# ------------------------------------------------------------------------------
# TASK 06: EXP6 - COMPARISON WITH OTHER METHODS
# ------------------------------------------------------------------------------
dfl <- read.csv(
  here::here(
    "Data_Output/Experiment_6_Comparison_with_Other_Methods_Results.csv"
  )
)

dfl[dfl$Algorithm == "rdos", "Algorithm"] <- "RDOS"
dfl[dfl$Algorithm == "kdeos", "Algorithm"] <- "KDEOS"

g4 <- ggplot(dfl, aes(x = Iteration, y = mean, color = Algorithm)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
  geom_line(aes(color = Algorithm), linewidth = 1) +
  ylab("Performance") +
  facet_wrap(~Metric) +
  scale_x_continuous(breaks = 2 * (1:5)) +
  theme(legend.position = "bottom")

# Plot data
g1 <- generate_exp6(3) |>
  ggplot(aes(x1, x2)) +
  geom_point(aes(color = Points))
X <- generate_exp6(9)
g2 <- X |>
  ggplot(aes(x1, x2)) +
  geom_point(aes(color = Points))
g3 <- X |>
  ggplot(aes(x2, x3)) +
  geom_point(aes(color = Points))

p <- wrap_plots(g1, g2, guide_area(), g3) +
  plot_layout(guides = "collect")

fig <- here::here("Figures/Exp6_Data_and_Results.pdf")
cairo_pdf(file = fig, width = 8, height = 5)
p | g4
crop::dev.off.crop(fig)
