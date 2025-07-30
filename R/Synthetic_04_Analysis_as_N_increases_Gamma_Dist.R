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
df3 <- df1 <- df2 <- set_up_diff_metrics(length(nnvals))

experiment <- 2
shape1 <- 2
shape2 <- 2.2
rate1 <- 2
rate2 <- 2

for (ii in 1:length(nnvals)) {
  nn <- nnvals[ii]
  num_outliers <- ceiling(5 / 1000 * nn)

  X1 <- matrix(rgamma(n = 2 * nn, shape = shape1, rate = rate1), ncol = 2)
  X2 <- matrix(rgamma(n = 2 * nn, shape = shape2, rate = rate2), ncol = 2)

  x1dist <- apply(X1, 1, function(x) sqrt(x[1]^2 + x[2]^2))
  qq <- quantile(x1dist, probs = 0.99)
  x2dist <- apply(X2, 1, function(x) sqrt(x[1]^2 + x[2]^2))
  inds <- which(x2dist > qq)
  inds2 <- sample(inds, num_outliers)
  X <- rbind(X1, X2[inds2, ])
  labs <- c(rep(0, nn), rep(1, num_outliers))

  # New lookout - Normalize = TRUE, scale = FALSE
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
colnames(df)[1:2] <- c("x", "y")

#ggplot(df, aes(x, y, color = as.factor(labs))) +
#  geom_point()

df1 <- df1 |>
  mutate(Algo = "New_Lookout")

df3 <- df3 |>
  mutate(Algo = "Old_Lookout")

df <- rbind(df1, df3)

write.csv(
  df,
  here::here("Data_Output/For_Paper/Synthetic_Exp_04_Increasing_N_Gamma.csv"),
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
#   geom_point(size = 0.1) +
#   geom_jitter(size = 0.1, height = 0.02) +
#   facet_grid(~name) +
#   xlab("Number of points") +
#   ylab("Value") +
#   geom_smooth(se = FALSE) +
#   theme_bw()
