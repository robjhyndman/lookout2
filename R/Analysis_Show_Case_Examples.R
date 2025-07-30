# --------------------------------------------------------------
# TASK 01: TEST LOOKOUT ON FIGURE 7 EXAMPLES IN PAPER
# --------------------------------------------------------------
# ---------------------------------------------------------------------
# TASK 1 - EXAMPLE 1
library(tidyverse)
library(lookout)
library(gridExtra)
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

lookobj1 <- lookout::lookout(
  X,
  alpha = 0.01,
  scale = TRUE,
  gamma = 0.98,
  old_version = FALSE
)
g1 <- autoplot(lookobj1) +
  ggtitle("New lookout")

# Old lookout
lookobj2 <- lookout::lookout(
  X,
  alpha = 0.01,
  scale = TRUE,
  gamma = 1,
  old_version = TRUE
)
g2 <- autoplot(lookobj2) +
  ggtitle("Old Lookout")

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

lookobj1 <- lookout::lookout(
  X,
  alpha = 0.01,
  scale = TRUE,
  gamma = 0.98,
  old_version = FALSE
)

g3 <- autoplot(lookobj1)

lookobj2 <- lookout::lookout(
  X,
  alpha = 0.01,
  scale = TRUE,
  gamma = 1,
  old_version = TRUE
)
g4 <- autoplot(lookobj2)

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

Xdf <- cbind.data.frame(X, label = c(rep("Normal", 700), rep("Anomaly", 3)))
#ggplot(Xdf, aes(x, y)) +
#  geom_point(aes(color = label))

# Newer version
lookobj1 <- lookout::lookout(
  X,
  alpha = 0.01,
  scale = TRUE,
  gamma = 0.98,
  old_version = FALSE
)

g5 <- autoplot(lookobj1)

# Older version
lookobj2 <- lookout::lookout(
  X,
  alpha = 0.01,
  scale = TRUE,
  gamma = 1,
  old_version = TRUE
)
g6 <- autoplot(lookobj2)

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

Xdf <- cbind.data.frame(X, label = c(rep("Normal", 700), rep("Anomaly", 3)))
#ggplot(Xdf, aes(x, y)) +
#  geom_point(aes(color = label))

# Newer version
lookobj1 <- lookout::lookout(
  X,
  alpha = 0.01,
  scale = TRUE,
  gamma = 0.98,
  old_version = FALSE
)
g7 <- autoplot(lookobj1)

# Older version
lookobj2 <- lookout::lookout(
  X,
  alpha = 0.01,
  scale = TRUE,
  gamma = 1,
  old_version = TRUE
)
g8 <- autoplot(lookobj2)

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
# x = c(0, -0.2, 0.4),
# y = c(0.3, 0.4, 0.5)
Xdf <- cbind.data.frame(X, label = c(rep("Normal", 1000), rep("Anomaly", 3)))
#ggplot(Xdf, aes(x, y)) +
#  geom_point(aes(color = label))

# Newer version
lookobj1 <- lookout::lookout(
  X,
  alpha = 0.01,
  scale = TRUE,
  gamma = 0.98,
  old_version = FALSE
)
g9 <- autoplot(lookobj1)

# Older version
lookobj2 <- lookout::lookout(
  X,
  alpha = 0.01,
  scale = TRUE,
  gamma = 1,
  old_version = TRUE
)
g10 <- autoplot(lookobj2)

# New one is actually better. Gets 2/3 anomalies with some false positives.
# The old version doesn't get anomalies at all.

fig <- here::here(paste0("Figures/Showcase_Examples.pdf"))
cairo_pdf(file = fig, width = 12, height = 6)
print(grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, ncol = 2))
crop::dev.off.crop(fig)
