# --------------------------------------------------------------
# TASK 01: TEST 1 N INCREASES AND OUTLIERS ARE ON THE BOUNDARY - bw_power = NA
# --------------------------------------------------------------

library(lookout)
library(tidyverse)

# To include in lookout R package.
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
    tpr = 0
  }
  if (fn == 0) {
    fnr = 0
  }
  if (tn == 0) {
    tnr = 0
  }
  if (fp == 0) {
    fpr = 0
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

# --------------------------------------------------------------
# TASK 01: TEST 1 N INCREASES AND OUTLIERS ARE ON THE BOUNDARY - bw_power = NA
# --------------------------------------------------------------

nnvals <- (1:10) * 1000
nnvals <- rep(nnvals, each = 10)
df3 <- df1 <- data.frame(
  N = numeric(100),
  true_pos = numeric(100),
  true_neg = numeric(100),
  false_pos = numeric(100),
  false_neg = numeric(100),
  true_positive_rate = numeric(100),
  true_negative_rate = numeric(100),
  false_positive_rate = numeric(100),
  false_negative_rate = numeric(100)
)

set.seed(2025)
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

  # New lookout - Normalize = FALSE, Unitize = TRUE
  lookobj1 <- lookout::lookout(
    X,
    alpha = 0.05,
    unitize = TRUE,
    normalize = FALSE,
    bw_para = 0.95,
    version = 2,
    bw_power = NA
  )

  pred1 <- rep(0, NROW(X))
  pred1[lookobj1$outliers[, 1]] <- 1
  df1[ii, ] <- diff_metrics(labs, pred1)

  # Old lookout
  lookobjOld <- lookout::lookout(
    X,
    alpha = 0.05,
    unitize = TRUE,
    normalize = FALSE,
    bw_para = 1,
    version = 1,
    bw_power = NA
  )

  pred3 <- rep(0, NROW(X))
  pred3[lookobjOld$outliers[, 1]] <- 1
  df3[ii, ] <- diff_metrics(labs, pred3)
}

df <- cbind.data.frame(X, labs)

ggplot(df, aes(x, y, color = as.factor(labs))) +
  geom_point() +
  scale_color_discrete(labels = c("Non-anomalous", "Anomalous")) +
  labs(color = "Points")

df1 <- df1 |>
  mutate(Algo = "New_Lookout")
df3 <- df3 |>
  mutate(Algo = "Old_Lookout")

df <- rbind(df1, df3)

write.csv(
  df,
  "Data_Output/For_Paper/Synthetic_Exp_03_Increasing_Increasing_N_Normal.csv",
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


ggplot(dfl, aes(x = N, y = value, color = Algo)) +
  geom_point(size = 0.5) +
  facet_grid(~name) +
  xlab("Number of points") +
  ylab("Value") +
  geom_smooth() +
  theme_bw()
