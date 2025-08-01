library(ggplot2)
library(dplyr)
library(patchwork)
source(here::here("R/functions.R"))

# ------------------------------------------------------------------------------
# TASK 07: EXP7 - COMPARISON WITH OTHER METHODS
# ------------------------------------------------------------------------------

dfl <- read.csv(
  here::here(
    "Data_Output/Experiment_7_Comparison_with_Other_Methods_Results.csv"
  )
)
dfl[dfl$Algorithm == "rdos", "Algorithm"] <- "RDOS"
dfl[dfl$Algorithm == "kdeos", "Algorithm"] <- "KDEOS"

g4 <- ggplot(dfl, aes(x = Iteration, y = mean, color = Algorithm)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
  geom_line(aes(color = Algorithm), linewidth = 1) +
  ylab("Performance") +
  facet_wrap(~Metric) +
  scale_x_continuous(breaks = (1:4) * 5) +
  theme(legend.position = "bottom")


# plot data
dobX <- bind_rows(
  generate_exp7(5) |> as_dobin() |> mutate(iterate = 5),
  generate_exp7(12) |> as_dobin() |> mutate(iterate = 12),
  generate_exp7(20) |> as_dobin() |> mutate(iterate = 20)
)

p <- dobX |>
  ggplot() +
  aes(D1, D2) +
  geom_point(aes(color = labels)) +
  facet_wrap(~iterate, strip.position = "right", ncol = 1) +
  theme(legend.position = "bottom")

fig <- here::here("Figures/Exp7_Data_and_Results.pdf")
cairo_pdf(file = fig, width = 8, height = 5)
p | g4
crop::dev.off.crop(fig)
