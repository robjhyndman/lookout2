# ------------------------------------------------------------------------------
# TASK 01: EXP1 - GAMMA DISTRIBUTION TWO SEPARATE DISTRIBUTIONS
# TASK 02: EXP2 - NORMAL DISTRIBUTION TWO SEPARATE DISTRIBUTIONS
# TASK 03: EXP3 - AS N INCREASES - NORMAL DISTRIBUTION
# TASK 04: EXP4 - AS N INCREASES - GAMMA DISTRIBUTION
# TASK 05: EXP5 - COMPARISON WITH OTHER METHODS
# TASK 06: EXP6 - COMPARISON WITH OTHER METHODS
# TASK 07: EXP7 - COMPARISON WITH OTHER METHODS
# ------------------------------------------------------------------------------

library(ggplot2)
library(tidyverse)

# Okabe-Ito colours
options(
  ggplot2.discrete.colour = c(
    "#D55E00",
    "#0072B2",
    "#009E73",
    "#CC79A7",
    "#E69F00",
    "#56B4E9",
    "#F0E442"
  ),
  ggplot2.discrete.fill = c(
    "#D55E00",
    "#0072B2",
    "#009E73",
    "#CC79A7",
    "#E69F00",
    "#56B4E9",
    "#F0E442"
  )
)
# # Fira Sans font for graphics
# ggplot2::theme_set(
#   ggplot2::theme_get() +
#     ggplot2::theme(text = ggplot2::element_text(family = "Fira Sans"))
# )

ggplot2::theme_set(theme_bw())

# ------------------------------------------------------------------------------
# TASK 01: EXP1 - GAMMA DISTRIBUTION TWO SEPARATE DISTRIBUTIONS
# ------------------------------------------------------------------------------

results_new <- read.csv(
  here::here("Data_Output/For_Paper/Synthetic_Exp_01_gamma_new_lookout.csv")
)
results_old <- read.csv(
  here::here("Data_Output/For_Paper/Synthetic_Exp_01_gamma_old_lookout.csv")
)

results_new <- results_new |>
  mutate(method = "New lookout")
results_old <- results_old |>
  mutate(method = "Old lookout")
results <- rbind(results_new, results_old)

results_lng <- results |>
  select(
    method,
    outrate,
    true_pos_rate,
    true_neg_rate,
    false_pos_rate,
    false_neg_rate
  ) |>
  rename(
    "True Positive Rate" = true_pos_rate,
    "False Positive Rate" = false_pos_rate,
    "False Negative Rate" = false_neg_rate,
    "True Negative Rate" = true_neg_rate
  ) |>
  pivot_longer(cols = 3:6, names_to = "metric", values_to = "value")

g1 <- ggplot(results_lng, aes(x = outrate, y = value, color = method)) +
  geom_jitter(width = 0.05, height = 0, alpha = 0.4) +
  geom_smooth() +
  facet_grid(metric ~ method) +
  labs(x = "Anomaly Rate", y = "Count") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 500, by = 2))

# ------------------------------------------------------------------------------
# PLOTTING THE DATA
# Generate random data
outrate <- (1:10) / 10
n1 <- 500
n2 <- 10
shape1 <- 2
shape2 <- 2
rate1 <- 2
rate2 <- outrate

set.seed(2025)
i <- 2
X <- matrix(rgamma(n = 2 * n1, shape = shape1, rate = rate1), ncol = 2)
X2 <- matrix(rgamma(n = 2 * n2, shape = shape2, rate = rate2[i]), ncol = 2)
X <- rbind(X, X2)
Points <- c(rep("Non-anomaly", n1), rep("Anomaly", n2))
df <- cbind.data.frame(X, Points)
colnames(df)[1:2] <- c("X1", "X2")

g2 <- ggplot(df, aes(X1, X2, color = Points)) +
  geom_point() +
  labs(color = "Points(rate = 0.2)")

i <- 9
X <- matrix(rgamma(n = 2 * n1, shape = shape1, rate = rate1), ncol = 2)
X2 <- matrix(rgamma(n = 2 * n2, shape = shape2, rate = rate2[i]), ncol = 2)
X <- rbind(X, X2)
Points <- c(rep("Non-anomaly", n1), rep("Anomaly", n2))
df <- cbind.data.frame(X, Points)
colnames(df)[1:2] <- c("X1", "X2")

g3 <- ggplot(df, aes(X1, X2, color = Points)) +
  geom_point() +
  labs(color = "Points(rate = 0.9)")

lay <- rbind(c(2, 1), c(3, 1))
gridExtra::grid.arrange(g1, g2, g3, layout_matrix = lay)

# ------------------------------------------------------------------------------
# TASK 02: EXP2 - NORMAL DISTRIBUTION TWO SEPARATE DISTRIBUTIONS
# ------------------------------------------------------------------------------
results_new <- read.csv(
  here::here("Data_Output/For_Paper/Synthetic_Exp_02_normal_new_lookout.csv")
)
results_old <- read.csv(
  here::here("Data_Output/For_Paper/Synthetic_Exp_02_normal_old_lookout.csv")
)

results_new <- results_new |>
  mutate(method = "New lookout")
results_old <- results_old |>
  mutate(method = "Old lookout")
results <- rbind(results_new, results_old)

results_lng <- results |>
  select(
    method,
    mean,
    true_pos_rate,
    true_neg_rate,
    false_pos_rate,
    false_neg_rate
  ) |>
  rename(
    "True Positive Rate" = true_pos_rate,
    "False Positive Rate" = false_pos_rate,
    "False Negative Rate" = false_neg_rate,
    "True Negative Rate" = true_neg_rate
  ) |>
  pivot_longer(cols = 3:6, names_to = "metric", values_to = "value")

g1 <- ggplot(results_lng, aes(x = mean, y = value, color = method)) +
  geom_jitter(width = 0.05, height = 0, alpha = 0.4) +
  geom_smooth() +
  facet_grid(metric ~ method) +
  labs(x = "Mean", y = "Rate") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 500, by = 2))

# ------------------------------------------------------------------------------
# PLOTTING THE DATA
# Generate random data
mm_seq <- seq(2.5, 4, by = 0.25)
n1 <- 1000
n2 <- 10
i <- 1
mm <- mm_seq[i]
# Generate random data
X <- rbind(
  data.frame(x = rnorm(n1), y = rnorm(n1)),
  data.frame(
    x = rnorm(n2, mean = mm, sd = 0.2),
    y = rnorm(n2, mean = mm, sd = 0.2)
  )
)
Points <- c(rep("Non-anomaly", n1), rep("Anomaly", n2))
df <- cbind.data.frame(X, Points)
colnames(df)[1:2] <- c("X1", "X2")

g2 <- ggplot(df, aes(X1, X2, color = Points)) +
  geom_point() +
  labs(color = paste0("Points(mean = ", mm, ")"))
g2

i <- 6
mm <- mm_seq[i]
# Generate random data
X <- rbind(
  data.frame(x = rnorm(n1), y = rnorm(n1)),
  data.frame(
    x = rnorm(n2, mean = mm, sd = 0.2),
    y = rnorm(n2, mean = mm, sd = 0.2)
  )
)
Points <- c(rep("Non-anomaly", n1), rep("Anomaly", n2))
df <- cbind.data.frame(X, Points)
colnames(df)[1:2] <- c("X1", "X2")

g3 <- ggplot(df, aes(X1, X2, color = Points)) +
  geom_point() +
  labs(color = paste0("Points(mean = ", mm, ")"))
g3

lay <- rbind(c(2, 1), c(3, 1))
gridExtra::grid.arrange(g1, g2, g3, layout_matrix = lay)

# ------------------------------------------------------------------------------
# TASK 03: EXP3 - AS N INCREASES - NORMAL DISTRIBUTION
# ------------------------------------------------------------------------------

results_new <- read.csv(
  here::here(
    "Data_Output/For_Paper/Exp3_Increasing_N_Normal_Distribution_New_Lookout.csv"
  )
)
results_old <- read.csv(
  here::here(
    "Data_Output/For_Paper/Exp3_Increasing_N_Normal_Distribution_Old_Lookout.csv"
  )
)

results_new <- results_new |>
  mutate(Algo = "New_Lookout")
results_old <- results_old |>
  mutate(Algo = "Old_Lookout")

results <- rbind(results_new, results_old)

results <- results |>
  select(
    Algo,
    N,
    true_positive_rate,
    true_negative_rate,
    false_positive_rate,
    false_negative_rate
  ) |>
  rename(
    "True Positive Rate" = true_positive_rate,
    "False Positive Rate" = false_positive_rate,
    "False Negative Rate" = false_negative_rate,
    "True Negative Rate" = true_negative_rate
  )

results_lng <- results |>
  pivot_longer(cols = 3:6)

ggplot(results_lng, aes(x = N, y = value, color = Algo)) +
  geom_point(size = 0.5) +
  facet_grid(~name) +
  xlab("Number of points") +
  ylab("Value") +
  geom_smooth()

# -----------------------------------------------------------------------------
# Generate Data to plot
nn <- 10000
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

df <- cbind.data.frame(X, labs)
colnames(df)[1:2] <- c("x", "y")

ggplot(df, aes(x, y, color = as.factor(labs))) +
  geom_point() +
  scale_color_discrete(labels = c("Non-anomalous", "Anomalous")) +
  labs(color = "Points")

# Saved 6.82 x 5.57 inches in pdf

# ------------------------------------------------------------------------------
# TASK 04: EXP4 - AS N INCREASES - GAMMA DISTRIBUTION
# ------------------------------------------------------------------------------

df <- read.csv(here::here(
  "Data_Output/For_Paper/Synthetic_Exp_04_Increasing_N_Gamma.csv"
))

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
  geom_point(size = 0.1) +
  geom_jitter(size = 0.1, height = 0.02) +
  facet_grid(~name) +
  xlab("Number of points") +
  ylab("Value") +
  geom_smooth(se = FALSE) +
  theme_bw()

# -----------------------------------------------------------------------------
# Generate Data to plot
nn <- 10000
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

df <- cbind.data.frame(X, labs)
colnames(df)[1:2] <- c("x", "y")

ggplot(df, aes(x, y, color = as.factor(labs))) +
  geom_point() +
  scale_color_discrete(labels = c("Non-anomalous", "Anomalous")) +
  labs(color = "Points")

# Saved 6.82 x 5.57 inches in pdf

# ------------------------------------------------------------------------------
# TASK 05: EXP5 - COMPARISON WITH OTHER METHODS
# ------------------------------------------------------------------------------
dfl <- read.csv(
  here::here(
    "Data_Output/For_Paper/Experiment_5_Comparison_with_Other_Methods_Results.csv"
  )
)
dfl[dfl$Algorithm == "rdos", "Algorithm"] <- "RDOS"
dfl[dfl$Algorithm == "kdeos", "Algorithm"] <- "KDEOS"

g4 <- ggplot(dfl, aes(x = Iteration, y = mean, color = Algorithm)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
  geom_line(aes(color = Algorithm), linewidth = 1) +
  ylab("Performance") +
  facet_wrap(~Metric) +
  scale_x_continuous(breaks = 2 * (1:5)) +
  theme_bw()
g4

# plot data
i <- 3
set.seed(1)
X <- bind_cols(
  x2 = rnorm(405),
  x3 = rnorm(405),
  x4 = rnorm(405),
  x5 = rnorm(405),
  x6 = rnorm(405)
)
x1_1 <- rnorm(400)
labels <- c(rep(0, 400), rep(1, 5))

x1_2 <- rnorm(5, mean = 2 + (i - 1) * 0.5, sd = 0.2)
X <- X %>% mutate(x1 = c(x1_1, x1_2))

g1 <- ggplot(X, aes(x1, x2)) +
  geom_point(aes(color = as.factor(labels))) +
  scale_color_discrete(labels = c("Non-anomalous", "Anomalous")) +
  labs(color = "Points")
g1

# plot data
i <- 9
set.seed(1)
X <- bind_cols(
  x2 = rnorm(405),
  x3 = rnorm(405),
  x4 = rnorm(405),
  x5 = rnorm(405),
  x6 = rnorm(405)
)
x1_1 <- rnorm(400)
labels <- c(rep(0, 400), rep(1, 5))

x1_2 <- rnorm(5, mean = 2 + (i - 1) * 0.5, sd = 0.2)
X <- X %>% mutate(x1 = c(x1_1, x1_2))

g2 <- ggplot(X, aes(x1, x2)) +
  geom_point(aes(color = as.factor(labels))) +
  scale_color_discrete(labels = c("Non-anomalous", "Anomalous")) +
  labs(color = "Points")
g2

g3 <- ggplot(X, aes(x3, x4)) +
  geom_point(aes(color = as.factor(labels))) +
  scale_color_discrete(labels = c("Non-anomalous", "Anomalous")) +
  labs(color = "Points")
g3

lay <- rbind(c(1, 4), c(2, 4), c(3, 4))
gridExtra::grid.arrange(g1, g2, g3, g4, layout_matrix = lay)

# 8.91 x 3.97
# EXP5_Data_and_Results

# ------------------------------------------------------------------------------
# TASK 06: EXP6 - COMPARISON WITH OTHER METHODS
# ------------------------------------------------------------------------------
dfl <- read.csv(
  here::here(
    "Data_Output/For_Paper/Experiment_6_Comparison_with_Other_Methods_Results.csv"
  )
)

dfl[dfl$Algorithm == "rdos", "Algorithm"] <- "RDOS"
dfl[dfl$Algorithm == "kdeos", "Algorithm"] <- "KDEOS"

g4 <- ggplot(dfl, aes(x = Iteration, y = mean, color = Algorithm)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
  geom_line(aes(color = Algorithm), linewidth = 1) +
  ylab("Performance") +
  facet_wrap(~Metric) +
  scale_x_continuous(breaks = 2 * (1:5)) +
  theme_bw()
g4

# plot data
i <- 3
set.seed(1)
nn <- 805
r1 <- runif(nn)
r2 <- rnorm(nn, mean = 5)
theta <- 2 * pi * r1
R2 <- 2
dist <- r2 + R2
X <- tibble(
  x1 = dist * cos(theta),
  x2 = dist * sin(theta),
  x3 = runif(nn)
)
labels <- c(rep(0, nn - 5), rep(1, 5))

X[nn - 5 + seq(5), 1] <- rnorm(5, 5 - (i - 1) * 0.5, sd = 0.1)
X[nn - 5 + seq(5), 2] <- rnorm(5, 0, sd = 0.1)

g1 <- ggplot(X, aes(x1, x2)) +
  geom_point(aes(color = as.factor(labels))) +
  scale_color_discrete(labels = c("Non-anomalous", "Anomalous")) +
  labs(color = "Points")
g1

# plot data
i <- 9
set.seed(1)
nn <- 805
r1 <- runif(nn)
r2 <- rnorm(nn, mean = 5)
theta <- 2 * pi * r1
R2 <- 2
dist <- r2 + R2
X <- tibble(
  x1 = dist * cos(theta),
  x2 = dist * sin(theta),
  x3 = runif(nn)
)
labels <- c(rep(0, nn - 5), rep(1, 5))

X[nn - 5 + seq(5), 1] <- rnorm(5, 5 - (i - 1) * 0.5, sd = 0.1)
X[nn - 5 + seq(5), 2] <- rnorm(5, 0, sd = 0.1)

g2 <- ggplot(X, aes(x1, x2)) +
  geom_point(aes(color = as.factor(labels))) +
  scale_color_discrete(labels = c("Non-anomalous", "Anomalous")) +
  labs(color = "Points")
g2

g3 <- ggplot(X, aes(x2, x3)) +
  geom_point(aes(color = as.factor(labels))) +
  scale_color_discrete(labels = c("Non-anomalous", "Anomalous")) +
  labs(color = "Points")
g3

lay <- rbind(c(1, 4), c(2, 4), c(3, 4))
gridExtra::grid.arrange(g1, g2, g3, g4, layout_matrix = lay)

# 8.91 x 3.97
# EXP6_Data_and_Results

# ------------------------------------------------------------------------------
# TASK 07: EXP7 - COMPARISON WITH OTHER METHODS
# ------------------------------------------------------------------------------

dfl <- read.csv(
  here::here(
    "Data_Output/For_Paper/Experiment_7_Comparison_with_Other_Methods_Results.csv"
  )
)
dfl[dfl$Algorithm == "rdos", "Algorithm"] <- "RDOS"
dfl[dfl$Algorithm == "kdeos", "Algorithm"] <- "KDEOS"

g4 <- ggplot(dfl, aes(x = Iteration, y = mean, color = Algorithm)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
  geom_line(aes(color = Algorithm), linewidth = 1) +
  ylab("Performance") +
  facet_wrap(~Metric) +
  scale_x_continuous(breaks = 2 * (1:10)) +
  theme_bw()
g4

# plot data
set.seed(1)
i <- 5
nn <- 500
X <- matrix(runif(nn * (dd + 1)), ncol = dd + 1, nrow = nn)
colnames(X) <- paste("x", 1:20, sep = "")
X[nn, 1:i] <- rep(0.9, i)
# pca <- prcomp(X)
# pcaX <- pca$x
# labels <- c(rep(0, 499), 1)
#
# g1 <- ggplot(pcaX, aes(PC1, PC2)) +
#   geom_point(aes(color = as.factor(labels))) +
#   scale_color_discrete(labels = c("Non-anomalous", "Anomalous")) +
#   labs(color = "Points")
# g1

dobout <- dobin::dobin(X)
dobX <- dobout$coords
colnames(dobX) <- paste0("D", 1:20)
g1 <- ggplot(dobX, aes(D1, D2)) +
  geom_point(aes(color = as.factor(labels))) +
  scale_color_discrete(labels = c("Non-anomalous", "Anomalous")) +
  labs(color = "Points")
g1

i <- 12
nn <- 500
X <- matrix(runif(nn * (dd + 1)), ncol = dd + 1, nrow = nn)
colnames(X) <- paste("x", 1:20, sep = "")
X[nn, 1:i] <- rep(0.9, i)

dobout <- dobin::dobin(X)
dobX <- dobout$coords
colnames(dobX) <- paste0("D", 1:20)
g2 <- ggplot(dobX, aes(D1, D2)) +
  geom_point(aes(color = as.factor(labels))) +
  scale_color_discrete(labels = c("Non-anomalous", "Anomalous")) +
  labs(color = "Points")
g2

i <- 20
nn <- 500
X <- matrix(runif(nn * (dd + 1)), ncol = dd + 1, nrow = nn)
colnames(X) <- paste("x", 1:20, sep = "")
X[nn, 1:i] <- rep(0.9, i)
# pca <- prcomp(X)
# pcaX <- pca$x
# labels <- c(rep(0, 499), 1)
#
# g2 <- ggplot(pcaX, aes(PC1, PC2)) +
#   geom_point(aes(color = as.factor(labels))) +
#   scale_color_discrete(labels = c("Non-anomalous", "Anomalous")) +
#   labs(color = "Points")
# g2

dobout <- dobin::dobin(X)
dobX <- dobout$coords
colnames(dobX) <- paste0("D", 1:20)
g3 <- ggplot(dobX, aes(D1, D2)) +
  geom_point(aes(color = as.factor(labels))) +
  scale_color_discrete(labels = c("Non-anomalous", "Anomalous")) +
  labs(color = "Points")
g3

lay <- rbind(c(1, 4), c(2, 4), c(3, 4))
gridExtra::grid.arrange(g1, g2, g3, g4, layout_matrix = lay)

# 8.91 x 3.97
# EXP7_Data_and_Results
