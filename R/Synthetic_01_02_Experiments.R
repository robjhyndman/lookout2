# ------------------------------------------------------------------------------
# This file has old-lookout and new-lookout comparison examples
# TASK 00: LOAD LIBRARIES AND FUNCTIONS
# TASK 01: EXP1 - GAMMA DISTRIBUTION TWO SEPARATE DISTRIBUTIONS
# TASK 02: EXP2 - NORMAL DISTRIBUTION TWO DISTRIBUTIONS
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# TASK 00: LOAD LIBRARIES AND FUNCTIONS
# ------------------------------------------------------------------------------
library(lookout)
library(ggplot2)
library(tidyverse)
source(here::here("R/functions.R"))

# ------------------------------------------------------------------------------
# TASK 01: EXP1 - GAMMA DISTRIBUTION TWO SEPARATE DISTRIBUTIONS
# ------------------------------------------------------------------------------

outrate <- (1:10) / 10
n1 <- 500
n2 <- 10
shape1 <- 2
shape2 <- 2
rate1 <- 2
rate2 <- outrate
bw <- 0.98

results_old <- results_new <- tibble(
  outrate = numeric(10 * length(rate2)),
  outliers = 0
) |>
  bind_cols(set_up_diff_metrics(10 * length(rate2)))
kk <- 1
for (jj in 1:10) {
  for (i in 1:length(rate2)) {
    # Generate random data
    X <- matrix(rgamma(n = 2 * n1, shape = shape1, rate = rate1), ncol = 2)
    X2 <- matrix(rgamma(n = 2 * n2, shape = shape2, rate = rate2[i]), ncol = 2)
    X <- rbind(X, X2)

    lookobj_new <- lookout::lookout(
      X,
      alpha = 0.01,
      scale = TRUE,
      gamma = 0.98,
      old_version = FALSE
    )

    lookobj_old <- lookout::lookout(
      X,
      alpha = 0.01,
      scale = TRUE,
      gamma = 1,
      old_version = TRUE
    )

    act <- c(rep(0, n1), rep(1, n2))
    preds_old <- preds_new <- rep(0, n1 + n2)
    preds_new[lookobj_new$outliers[, 1]] <- 1
    preds_old[lookobj_old$outliers[, 1]] <- 1
    results_new[kk, ] <- c(rate2[i], n2, diff_metrics(act, preds_new))
    results_old[kk, ] <- c(rate2[i], n2, diff_metrics(act, preds_old))
    kk <- kk + 1
  }
}
write.csv(
  results_new,
  here::here("Data_Output/For_Paper/Synthetic_Exp_01_gamma_new_lookout.csv"),
  row.names = FALSE
)
write.csv(
  results_old,
  here::here("Data_Output/For_Paper/Synthetic_Exp_01_gamma_old_lookout.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------------------------
# TASK 02: EXP2 - NORMAL DISTRIBUTION TWO DISTRIBUTIONS
# ------------------------------------------------------------------------------

reps <- 20
mm_seq <- seq(2.5, 4, by = 0.25)
n1 <- 1000
n2 <- 10
bw <- 0.98

results_old <- results_new <- tibble(
  mean = numeric(reps * length(mm_seq)),
  outliers = 0
) |>
  bind_cols(set_up_diff_metrics(reps * length(mm_seq)))
kk <- 1
for (jj in seq_len(reps)) {
  for (i in seq_along(mm_seq)) {
    mm <- mm_seq[i]
    # Generate random data
    X <- rbind(
      data.frame(x = rnorm(n1), y = rnorm(n1)),
      data.frame(
        x = rnorm(n2, mean = mm, sd = 0.2),
        y = rnorm(n2, mean = mm, sd = 0.2)
      )
    )

    lookobj_new <- lookout::lookout(
      X,
      alpha = 0.01,
      scale = TRUE,
      gamma = 0.98,
      old_version = FALSE
    )

    lookobj_old <- lookout::lookout(
      X,
      alpha = 0.01,
      scale = TRUE,
      gamma = 1,
      old_version = TRUE
    )

    act <- c(rep(0, n1), rep(1, n2))
    preds_old <- preds_new <- rep(0, n1 + n2)
    preds_new[lookobj_new$outliers[, 1]] <- 1
    preds_old[lookobj_old$outliers[, 1]] <- 1
    results_new[kk, ] <- c(mm, n2, diff_metrics(act, preds_new))
    results_old[kk, ] <- c(mm, n2, diff_metrics(act, preds_old))
    kk <- kk + 1
  }
}
write.csv(
  results_new,
  here::here("Data_Output/For_Paper/Synthetic_Exp_02_normal_new_lookout.csv"),
  row.names = FALSE
)
write.csv(
  results_old,
  here::here("Data_Output/For_Paper/Synthetic_Exp_02_normal_old_lookout.csv"),
  row.names = FALSE
)
