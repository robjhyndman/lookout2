library(lookout)
library(dplyr)
source(here::here("R/functions.R"))

# ------------------------------------------------------------------------------
# TASK 01: EXP1 - GAMMA DISTRIBUTION TWO SEPARATE DISTRIBUTIONS
# ------------------------------------------------------------------------------

n1 <- 500
n2 <- 10
rate2 <- seq(10) / 10
bw <- 0.98

results_old <- results_new <- tibble(
  outrate = numeric(10 * length(rate2)),
  outliers = 0
) |>
  bind_cols(set_up_diff_metrics(10 * length(rate2)))

kk <- 1
for (jj in 1:10) {
  for (i in 1:length(rate2)) {
    X <- generate_exp1(n1, n2, rate2[i])
    lookobj_new <- lookout::lookout(
      X[, 1:2],
      alpha = 0.01,
      scale = TRUE,
      gamma = 0.98,
      old_version = FALSE
    )
    lookobj_old <- lookout::lookout(
      X[, 1:2],
      alpha = 0.01,
      scale = TRUE,
      gamma = 1,
      old_version = TRUE
    )
    act <- X$Points == "Anomaly"
    results_new[kk, ] <- c(
      rate2[i],
      n2,
      diff_metrics(act, which_outliers(lookobj_new))
    )
    results_old[kk, ] <- c(
      rate2[i],
      n2,
      diff_metrics(act, which_outliers(lookobj_old))
    )
    kk <- kk + 1
  }
}

write.csv(
  results_new,
  here::here("Data_Output/Synthetic_Exp1_gamma_new_lookout.csv"),
  row.names = FALSE
)
write.csv(
  results_old,
  here::here("Data_Output/Synthetic_Exp1_gamma_old_lookout.csv"),
  row.names = FALSE
)
