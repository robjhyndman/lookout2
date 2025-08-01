library(lookout)
library(dplyr)
source(here::here("R/functions.R"))

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
    X <- generate_exp2(n1, n2, mm)
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
      mm,
      n2,
      diff_metrics(act, which_outliers(lookobj_new))
    )
    results_old[kk, ] <- c(
      mm,
      n2,
      diff_metrics(act, which_outliers(lookobj_old))
    )
    kk <- kk + 1
  }
}

write.csv(
  results_new,
  here::here("Data_Output/Synthetic_Exp2_normal_new_lookout.csv"),
  row.names = FALSE
)
write.csv(
  results_old,
  here::here("Data_Output/Synthetic_Exp2_normal_old_lookout.csv"),
  row.names = FALSE
)
