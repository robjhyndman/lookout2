# --------------------------------------------------------------
# TASK 01: TEST 1 N INCREASES AND OUTLIERS ARE ON THE BOUNDARY - bw_power = NA
# --------------------------------------------------------------

library(lookout)
library(tidyverse)
source(here::here("R/functions.R"))

# --------------------------------------------------------------
# TASK 01: TEST 1 N INCREASES AND OUTLIERS ARE ON THE BOUNDARY - bw_power = NA
# --------------------------------------------------------------

nnvals <- (1:10) * 1000
nnvals <- rep(nnvals, each = 10)
df3 <- df1 <- set_up_diff_metrics(length(nnvals))

for (ii in 1:length(nnvals)) {
  #
  nn <- nnvals[ii]
  num_outliers <- ceiling(5 / 1000 * nn) # 5 #
  meanx <- runif(num_outliers, min = -sqrt(2) * 2.2, max = sqrt(2) * 2.2)
  meany <- sqrt(2 * 2.2^2 - meanx^2)
  inds <- sample(1:num_outliers, ceiling(num_outliers / 2))
  meany[inds] <- -1 * meany[inds]
  X <- bind_rows(
    tibble(
      x = rnorm(nn),
      y = rnorm(nn)
    ),
    tibble(
      x = rnorm(num_outliers, mean = meanx, sd = 0.1), # meanx
      y = rnorm(num_outliers, mean = meany, sd = 0.1) # meany
    )
  )
  # plot(meanx, meany)
  labs <- c(rep(0, nn), rep(1, num_outliers))

  # New lookout - Normalize = FALSE, scale = TRUE
  lookobj1 <- lookout::lookout(
    X,
    alpha = 0.01,
    scale = TRUE,
    gamma = 0.95,
    old_version = FALSE
  )

  pred1 <- rep(0, NROW(X))
  pred1[lookobj1$outliers[, 1]] <- 1
  df1[ii, ] <- diff_metrics(labs, pred1)

  # Old lookout
  lookobjOld <- lookout::lookout(
    X,
    alpha = 0.01,
    scale = TRUE,
    gamma = 1,
    old_version = TRUE
  )

  pred3 <- rep(0, NROW(X))
  pred3[lookobjOld$outliers[, 1]] <- 1
  df3[ii, ] <- diff_metrics(labs, pred3)
}

df <- cbind.data.frame(X, labs)

# ggplot(df, aes(x, y, color = as.factor(labs))) +
#   geom_point() +
#   scale_color_discrete(labels = c("Non-anomalous", "Anomalous")) +
#   labs(color = "Points")

df1 <- df1 |>
  mutate(Algo = "New_Lookout")
df3 <- df3 |>
  mutate(Algo = "Old_Lookout")

df <- rbind(df1, df3)

write.csv(
  df,
  here::here(
    "Data_Output/For_Paper/Synthetic_Exp_03_Increasing_Increasing_N_Normal.csv"
  ),
  row.names = FALSE
)

dfl <- df |>
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

# ggplot(dfl, aes(x = N, y = value, color = Algo)) +
#   geom_point(size = 0.5) +
#   facet_grid(~name) +
#   xlab("Number of points") +
#   ylab("Value") +
#   geom_smooth() +
#   theme_bw()
