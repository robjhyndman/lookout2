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

diff_metrics <- function(act, pred) {
  # positives to be denoted by 1 and negatives with 0
  n <- length(act)
  tp <- sum((act == 1) & (pred == 1))
  tn <- sum((act == 0) & (pred == 0))
  fp <- sum((act == 0) & (pred == 1))
  fn <- sum((act == 1) & (pred == 0))
  prec <- (tp + tn) / n

  tpr <- tp / (tp + fn)
  tnr <- tn / (tn + fp)
  fpr <- fp / (fp + tn)
  fnr <- fn / (fn + tp)

  if (tp == 0) {
    tpr <- 0
  }
  if (fn == 0) {
    fnr <- 0
  }
  if (tn == 0) {
    tnr <- 0
  }
  if (fp == 0) {
    fpr <- 0
  }

  out <- data.frame(
    N = n,
    true_pos = tp,
    true_neg = tn,
    false_pos = fp,
    false_neg = fn,
    true_positive_rate = tpr,
    true_negative_rate = tnr,
    false_positive_rate = fpr,
    false_negative_rate = fnr
  )

  return(out)
}

# ------------------------------------------------------------------------------
# TASK 01: EXP1 - GAMMA DISTRIBUTION TWO SEPARATE DISTRIBUTIONS
# ------------------------------------------------------------------------------

set.seed(2025)
outrate <- (1:10) / 10
n1 <- 500
n2 <- 10
shape1 <- 2
shape2 <- 2
rate1 <- 2
rate2 <- outrate
bw <- 0.98

results_old <- results_new <- tibble(
  outrate = 0,
  outliers = 0,
  N = 0,
  true_pos = 0,
  true_neg = 0,
  false_pos = 0,
  false_neg = 0,
  true_pos_rate = 0,
  true_neg_rate = 0,
  false_pos_rate = 0,
  false_neg_rate = 0
)
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

set.seed(2025)
reps <- 20
mm_seq <- seq(2.5, 4, by = 0.25)
dfout_new <- dfout_old <- tibble(
  Algo = character(),
  mm = numeric(),
  N = numeric(),
  true_pos = numeric(),
  true_neg = numeric(),
  false_pos = numeric(),
  false_neg = numeric(),
  specificity = numeric()
)
n1 <- 1000
n2 <- 10
bw <- 0.98

results_old <- results_new <- tibble(
  mean = 0,
  outliers = 0,
  N = 0,
  true_pos = 0,
  true_neg = 0,
  false_pos = 0,
  false_neg = 0,
  true_pos_rate = 0,
  true_neg_rate = 0,
  false_pos_rate = 0,
  false_neg_rate = 0
)
kk <- 1
for (jj in 1:reps) {
  for (i in 1:length(mm_seq)) {
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
