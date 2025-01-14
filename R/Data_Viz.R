# ------------------------------------------------------------------------------
# TASK 01: EXP1 - GAMMA DISTRIBUTION TWO SEPARATE DISTRIBUTIONS
# TASK 02: EXP2 - NORMAL DISTRIBUTION TWO SEPARATE DISTRIBUTIONS
# TASK 03: EXP3 - AS N INCREASES - NORMAL DISTRIBUTION
# TASK 04: EXP4 - AS N INCREASES - GAMMA DISTRIBUTION
# ------------------------------------------------------------------------------


library(ggplot2)
library(tidyverse)

# Okabe-Ito colours
options(
  ggplot2.discrete.colour = c("#D55E00", "#0072B2","#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442"),
  ggplot2.discrete.fill = c("#D55E00", "#0072B2","#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442")
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

results_new <- read.csv("Data_Output/For_Paper/EXP_1_gamma_new_lookout.csv")
results_old <- read.csv("Data_Output/For_Paper/EXP_1_gamma_old_lookout.csv")

results_new <-results_new |>
  mutate(method = 'New lookout')
results_old <- results_old |>
  mutate(method = 'Old lookout')
results <- rbind(results_new, results_old)

##### NOTE -- IF SHOWING TRUE POSITIVES, FALSE POSITIVES ETC . . . ..
results_lng <- results |>
  select(method, outrate, true_pos, true_neg, false_pos, false_neg) |>
  rename(
    "True Positives" = true_pos,
    "False Positives" = false_pos,
    "False Negatives" = false_neg,
    "True Negatives" = true_neg
  ) |>
  pivot_longer(cols = 3:6,
               names_to = "metric",
               values_to = "value")

ggplot(results_lng, aes(x = outrate, y = value, color = method)) +
  geom_jitter(width = 0.05, height = 0, alpha = 0.4) +
  geom_smooth() +
  facet_grid(metric~method, scales = "free_y") +
  labs(x = "Anomaly Rate",
       y = "Count") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0,500,by=2))

##### NOTE -- IF SHOWING TRUE POSITIVE RATE, FALSE POSITIVE RATE ETC . . . ..
results_lng <- results |>
  select(method, outrate, true_pos_rate, true_neg_rate, false_pos_rate, false_neg_rate) |>
  rename(
    "True Positive Rate" = true_pos_rate,
    "False Positive Rate" = false_pos_rate,
    "False Negative Rate" = false_neg_rate,
    "True Negative Rate" = true_neg_rate
  ) |>
  pivot_longer(cols = 3:6,
               names_to = "metric",
               values_to = "value")

ggplot(results_lng, aes(x = outrate, y = value, color = method)) +
  geom_jitter(width = 0.05, height = 0, alpha = 0.4) +
  geom_smooth() +
  facet_grid(metric~method) +
  labs(x = "Anomaly Rate",
       y = "Count") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0,500,by=2))


# ------------------------------------------------------------------------------
# TASK 02: EXP2 - NORMAL DISTRIBUTION TWO SEPARATE DISTRIBUTIONS
# ------------------------------------------------------------------------------
results_new <- read.csv("Data_Output/For_Paper/EXP_2_normal_new_lookout.csv")
results_old <- read.csv("Data_Output/For_Paper/EXP_2_normal_old_lookout.csv")

results_new <-results_new |>
  mutate(method = 'New lookout')
results_old <- results_old |>
  mutate(method = 'Old lookout')
results <- rbind(results_new, results_old)

##### NOTE -- IF SHOWING TRUE POSITIVES, FALSE POSITIVES ETC . . . ..
results_lng <- results |>
  select(method, outrate, true_pos, true_neg, false_pos, false_neg) |>
  rename(
    "True Positives" = true_pos,
    "False Positives" = false_pos,
    "False Negatives" = false_neg,
    "True Negatives" = true_neg
  ) |>
  pivot_longer(cols = 3:6,
               names_to = "metric",
               values_to = "value")

ggplot(results_lng, aes(x = outrate, y = value, color = method)) +
  geom_jitter(width = 0.05, height = 0, alpha = 0.4) +
  geom_smooth() +
  facet_grid(metric~method, scales = "free_y") +
  labs(x = "Anomaly Rate",
       y = "Count") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0,500,by=2))


##### NOTE -- IF SHOWING TRUE POSITIVE RATE, FALSE POSITIVE RATE ETC . . . ..
results_lng <- results |>
  select(method, outrate, true_pos_rate, true_neg_rate, false_pos_rate, false_neg_rate) |>
  rename(
    "True Positive Rate" = true_pos_rate,
    "False Positive Rate" = false_pos_rate,
    "False Negative Rate" = false_neg_rate,
    "True Negative Rate" = true_neg_rate
  ) |>
  pivot_longer(cols = 3:6,
               names_to = "metric",
               values_to = "value")

ggplot(results_lng, aes(x = outrate, y = value, color = method)) +
  geom_jitter(width = 0.05, height = 0, alpha = 0.4) +
  geom_smooth() +
  facet_grid(metric~method) +
  labs(x = "Anomaly Rate",
       y = "Count") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0,500,by=2))



# ------------------------------------------------------------------------------
# TASK 03: EXP3 - AS N INCREASES - NORMAL DISTRIBUTION
# ------------------------------------------------------------------------------

results_new <- read.csv("Data_Output/For_Paper/Exp3_Increasing_N_Normal_Distribution_New_Lookout.csv")
results_old <- read.csv("Data_Output/For_Paper/Exp3_Increasing_N_Normal_Distribution_Old_Lookout.csv")

results_new <- results_new |>
  mutate(Algo = "New_Lookout")
results_old <- results_old |>
  mutate(Algo = "Old_Lookout")

results <- rbind(results_new, results_old)

results <- results|>
  select(Algo,
         N,
         true_positive_rate,
         true_negative_rate,
         false_positive_rate,
         false_negative_rate) |>
  rename(
  "True Positive Rate" = true_positive_rate,
  "False Positive Rate" = false_positive_rate,
  "False Negative Rate" = false_negative_rate,
  "True Negative Rate" = true_negative_rate
)

results_lng <- results |>
  pivot_longer(cols = 3:6)


ggplot(results_lng, aes(x = N, y = value, color = Algo))+
  geom_point(size = 0.5)+
  facet_grid(~name)+
  xlab("Number of points")+
  ylab("Value")+
  geom_smooth()


# ------------------------------------------------------------------------------
# TASK 04: EXP4 - AS N INCREASES - GAMMA DISTRIBUTION
# ------------------------------------------------------------------------------

results_new <- read.csv("Data_Output/For_Paper/Exp4_Increasing_N_Gamma_Distribution_New_Lookout.csv")
results_old <- read.csv("Data_Output/For_Paper/Exp4_Increasing_N_Gamma_Distribution_Old_Lookout.csv")

results_new <- results_new |>
  mutate(Algo = "New_Lookout")
results_old <- results_old |>
  mutate(Algo = "Old_Lookout")

results <- rbind(results_new, results_old)

results <- results|>
  select(Algo,
         N,
         true_positive_rate,
         true_negative_rate,
         false_positive_rate,
         false_negative_rate) |>
  rename(
    "True Positive Rate" = true_positive_rate,
    "False Positive Rate" = false_positive_rate,
    "False Negative Rate" = false_negative_rate,
    "True Negative Rate" = true_negative_rate
  )

results_lng <- results |>
  pivot_longer(cols = 3:6)


ggplot(results_lng, aes(x = N, y = value, color = Algo))+
  geom_jitter(size = 0.5)+
  facet_grid(~name)+
  xlab("Number of points")+
  ylab("Value")+
  geom_smooth()
