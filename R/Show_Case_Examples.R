# --------------------------------------------------------------
# TASK 01: TEST LOOKOUT ON FIGURE 7 EXAMPLES IN PAPER
# --------------------------------------------------------------
# ---------------------------------------------------------------------
# TASK 1 - EXAMPLE 1
library(dplyr)
library(ggplot2)
library(lookout)
source(here::here("R/functions.R"))

X <- bind_rows(
  tibble(
    x = rnorm(1000),
    y = rnorm(1000)
  ),
  tibble(
    x = rnorm(5, mean = 10, sd = 0.2),
    y = rnorm(5, mean = 10, sd = 0.2)
  )
)

ex1_lookout_new <- lookout::lookout(
  X,
  alpha = 0.01,
  scale = TRUE,
  gamma = 0.98,
  old_version = FALSE
)

# Old lookout
ex1_lookout_old <- lookout::lookout(
  X,
  alpha = 0.01,
  scale = TRUE,
  gamma = 1,
  old_version = TRUE
)

# ---------------------------------------------------------------------
# TASK 2 - EXAMPLE 2

X <- bind_rows(
  tibble(
    x = rnorm(1000, mean = -10),
    y = rnorm(1000),
  ),
  tibble(
    x = rnorm(1000, mean = 10),
    y = rnorm(1000)
  ),
  tibble(
    x = rnorm(5, sd = 0.2),
    y = rnorm(5, sd = 0.2)
  )
)
ex2_lookout_new <- lookout::lookout(
  X,
  alpha = 0.01,
  scale = TRUE,
  gamma = 0.98,
  old_version = FALSE
)

ex2_lookout_old <- lookout::lookout(
  X,
  alpha = 0.01,
  scale = TRUE,
  gamma = 1,
  old_version = TRUE
)

# ---------------------------------------------------------------------
# TASK 3 - EXAMPLE 3

X <- bind_rows(
  tibble(
    x = rnorm(500),
    y = rnorm(500)
  ),
  tibble(
    x = rnorm(100, sd = 0.7),
    y = rnorm(100, mean = 8, sd = 0.7)
  ),
  tibble(
    x = rnorm(100, mean = 8, sd = 0.7),
    y = rnorm(100, sd = 0.7)
  ),
  tibble(
    x = rnorm(3, mean = 6, sd = 0.2),
    y = rnorm(3, mean = 6, sd = 0.2)
  )
)

ex3_lookout_new <- lookout::lookout(
  X,
  alpha = 0.01,
  scale = TRUE,
  gamma = 0.98,
  old_version = FALSE
)

ex3_lookout_old <- lookout::lookout(
  X,
  alpha = 0.01,
  scale = TRUE,
  gamma = 1,
  old_version = TRUE
)

# ---------------------------------------------------------------------
# TASK 4 - EXAMPLE 4

X <- bind_rows(
  tibble(
    x = rnorm(500),
    y = rnorm(500)
  ),
  tibble(
    x = rnorm(100, sd = 0.7),
    y = rnorm(100, mean = 8, sd = 0.7)
  ),
  tibble(
    x = rnorm(100, mean = 8, sd = 0.7),
    y = rnorm(100, sd = 0.7)
  ),
  tibble(
    x = c(5, 8, 12),
    y = c(5, 7.5, 4)
  )
)

ex4_lookout_new <- lookout::lookout(
  X,
  alpha = 0.01,
  scale = TRUE,
  gamma = 0.98,
  old_version = FALSE
)
ex4_lookout_old <- lookout::lookout(
  X,
  alpha = 0.01,
  scale = TRUE,
  gamma = 1,
  old_version = TRUE
)

# ---------------------------------------------------------------------
# TASK 5 - EXAMPLE 5
X <- bind_rows(
  tibble(x = rnorm(1000, sd = 0.2)) %>%
    mutate(y = x^2 + rnorm(1000, sd = 0.001)),
  tibble(
    x = c(0, -0.4, 0.4),
    y = c(0.15, 0.3, 0.3)
  )
)
ex5_lookout_new <- lookout::lookout(
  X,
  alpha = 0.01,
  scale = TRUE,
  gamma = 0.98,
  old_version = FALSE
)
ex5_lookout_old <- lookout::lookout(
  X,
  alpha = 0.01,
  scale = TRUE,
  gamma = 1,
  old_version = TRUE
)

df <- bind_rows(
  as.data.frame(ex1_lookout_new) |>
    mutate(Example = "Example 1", Version = "New lookout"),
  as.data.frame(ex1_lookout_old) |>
    mutate(Example = "Example 1", Version = "Old lookout"),
  as.data.frame(ex2_lookout_new) |>
    mutate(Example = "Example 2", Version = "New lookout"),
  as.data.frame(ex2_lookout_old) |>
    mutate(Example = "Example 2", Version = "Old lookout"),
  as.data.frame(ex3_lookout_new) |>
    mutate(Example = "Example 3", Version = "New lookout"),
  as.data.frame(ex3_lookout_old) |>
    mutate(Example = "Example 3", Version = "Old lookout"),
  as.data.frame(ex4_lookout_new) |>
    mutate(Example = "Example 4", Version = "New lookout"),
  as.data.frame(ex4_lookout_old) |>
    mutate(Example = "Example 4", Version = "Old lookout"),
  as.data.frame(ex5_lookout_new) |>
    mutate(Example = "Example 5", Version = "New lookout"),
  as.data.frame(ex5_lookout_old) |>
    mutate(Example = "Example 5", Version = "Old lookout")
) |>
  as_tibble() |>
  group_by(Example) |>
  mutate(x = (x - min(x)) / (max(x) - min(x))) |>
  ungroup()


fig <- here::here("Figures/Showcase_Examples.pdf")
cairo_pdf(file = fig, width = 6, height = 10)
ggplot(df, aes(x = x, y = y, color = outliers)) +
  geom_point() +
  facet_grid(Example ~ Version, scales = "free") +
  theme(
    axis.title.x = element_blank(), # Hide x-axis title
    axis.text.x = element_blank(), # Hide x-axis text labels
    axis.ticks.x = element_blank(), # Hide x-axis tick marks
    axis.title.y = element_blank(), # Hide y-axis title
    axis.text.y = element_blank(), # Hide y-axis text labels
    axis.ticks.y = element_blank() # Hide y-axis tick marks
  )
crop::dev.off.crop(fig)
