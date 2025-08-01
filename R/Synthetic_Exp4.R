library(lookout)
library(dplyr)
source(here::here("R/functions.R"))

# --------------------------------------------------------------
# TASK 01: TEST 1 N INCREASES AND OUTLIERS ARE ON THE BOUNDARY - bw_power = NA
# --------------------------------------------------------------

nnvals <- rep((1:10) * 1000, each = 10)
df3 <- df1 <- df2 <- set_up_diff_metrics(length(nnvals))

for (ii in 1:length(nnvals)) {
  X <- generate_exp4(nnvals[[ii]])
  lookobj_new <- lookout::lookout(
    X[, 1:2],
    alpha = 0.01,
    scale = TRUE,
    gamma = 0.95,
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
  df1[ii, ] <- diff_metrics(act, which_outliers(lookobj_new))
  df3[ii, ] <- diff_metrics(act, which_outliers(lookobj_old))
}

df <- bind_rows(
  df1 |> mutate(Algo = "New_Lookout"),
  df3 |> mutate(Algo = "Old_Lookout")
)

write.csv(
  df,
  here::here("Data_Output/Synthetic_Exp4_Increasing_N_Gamma.csv"),
  row.names = FALSE
)
