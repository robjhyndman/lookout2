# ----------------------------------------------------------------------------------
# TASK 01 : EXPERIMENT 5
# TASK 02 : EXPERIMENT 6
# TASK 03 : EXPERIMENT 7
# ----------------------------------------------------------------------------------


# ----------------------------------------------------------------------------------
# TASK 01 : EXPERIMENT 5
# ----------------------------------------------------------------------------------
library(pROC)
library(HDoutliers)
library(stray)
library(lookout)
library(DDoutlier)
library(tidyverse)

diff_metrics <- function(act, pred) {
  # positives to be denoted by 1 and negatives with 0
  stopifnot(length(act) == length(pred))
  n <- length(act)
  tp <- sum((act == 1) & (pred == 1))
  tn <- sum((act == 0) & (pred == 0))
  fp <- sum((act == 0) & (pred == 1))
  fn <- sum((act == 1) & (pred == 0))
  prec <- (tp + tn) / n
  sn <- tp / (tp + fn)
  sp <- tn / (tn + fp)
  precision <- if_else(
    (tp + fp) == 0,
    0,
    tp / (tp + fp)
  )
  recall <- tp / (tp + fn)
  fmeasure <- if_else(
    (precision == 0) & (recall == 0),
    0,
    2 * precision * recall / (precision + recall)
  )
  tibble(
    N = n,
    true_pos = tp,
    true_neg = tn,
    false_pos = fp,
    false_neg = fn,
    accuracy = prec,
    sensitivity = sn,
    specificity = sp,
    gmean = sqrt(sn * sp),
    precision = precision,
    recall = recall,
    fmeasure = fmeasure
  )
}


set.seed(2025)
values <- rep(0, 10)
pp <- 10
hdoutliers_gmean <- hdoutliers_fmeasure <- lookout1_fmeasure <-
  lookoutOld_fmeasure <- lookout1_gmean <- lookoutOld_gmean <-
  stray_gmean <- stray_fmeasure <- matrix(0, nrow = pp, ncol = 10)

lookout1_roc <- lookoutOld_roc <- kdeos_roc <- rdos_roc <- matrix(0, nrow=pp, ncol=10)

hdoutliers_time <- lookout1_time <- lookoutOld_time <- stray_time <- kdeos_time <- rdos_time <- matrix(0, nrow=pp*10, ncol=5)

for (kk in seq(pp)) {
  X <- bind_cols(
    x2 = rnorm(405),
    x3 = rnorm(405),
    x4 = rnorm(405),
    x5 = rnorm(405),
    x6 = rnorm(405)
  )
  x1_1 <- rnorm(400)
  labs <- c(rep(0, 400), rep(1, 5))

  for (i in seq(10)) {
    x1_2 <- rnorm(5, mean = 2 + (i-1)*0.5, sd = 0.2)
    X <- X %>% mutate(x1 = c(x1_1, x1_2))

    ll <- (kk-1)*10 + i

    # STRAY
    tt <- system.time(strayout <- stray::find_HDoutliers(X, knnsearchtype = "kd_tree", alpha=0.05))
    straylabs <- rep(0, 405)
    straylabs[strayout$outliers] <- 1
    strayoutput <- diff_metrics(labs, straylabs)
    stray_gmean[kk, i] <- strayoutput$gmean
    stray_fmeasure[kk, i] <- strayoutput$fmeasure
    stray_time[ll, ] <- tt

    # LOOKOUT - NEW
    tt1 <- system.time(lookoutobj1 <- lookout::lookout(X,
                                                       alpha = 0.05,
                                                       unitize = TRUE,
                                                       normalize = FALSE,
                                                       bw_para = 0.98,
                                                       version = 2,
                                                       bw_power = NA))
    lookoutlabs1 <- rep(0, 405)
    lookoutlabs1[lookoutobj1$outliers[ ,1]] <- 1
    lookoutput1 <- diff_metrics(labs, lookoutlabs1)
    lookout1_gmean[kk, i] <- lookoutput1$gmean
    lookout1_fmeasure[kk, i] <- lookoutput1$fmeasure
    lookout1_scores <- lookoutobj1$outlier_scores
    roc_obj1 <- roc(labs, lookout1_scores, direction="<")
    lookout1_roc[kk, i] <- roc_obj1$auc
    lookout1_time[ll, ] <- tt1



    # LOOKOUT - OLD
    tt3 <- system.time(lookoutobjold <- lookout::lookout(X,
                                                         alpha = 0.05,
                                                         unitize = TRUE,
                                                         normalize = FALSE,
                                                         bw_para = 1,
                                                         version = 1,
                                                         bw_power = NA))
    lookoutlabsold <- rep(0, 405)
    lookoutlabsold[lookoutobjold$outliers[ ,1]] <- 1
    lookoutputold <- diff_metrics(labs, lookoutlabsold)
    lookoutOld_gmean[kk, i] <- lookoutputold$gmean
    lookoutOld_fmeasure[kk, i] <- lookoutputold$fmeasure
    lookoutOld_scores <- lookoutobjold$outlier_scores
    roc_objold <- roc(labs, lookoutOld_scores, direction="<")
    lookoutOld_roc[kk, i] <- roc_objold$auc
    lookoutOld_time[ll, ] <- tt3

    # HDOUTLIERS
    tt <- system.time(hdoutobj <- HDoutliers(X, alpha=0.05))
    hdoutlabs <- rep(0, dim(X)[1])
    hdoutlabs[hdoutobj] <- 1
    hdoutput <- diff_metrics(labs, hdoutlabs)
    hdoutliers_gmean[kk, i] <- hdoutput$gmean
    hdoutliers_fmeasure[kk, i] <- hdoutput$fmeasure
    hdoutliers_time[ll, ] <- tt

    # KDEOS
    tt <- system.time(kdeos_scores <- DDoutlier::KDEOS(X)) # using default parameters
    roc_obj <- roc(labs, kdeos_scores, direction="<")
    kdeos_roc[kk, i] <- roc_obj$auc
    kdeos_time[ll, ] <- tt

    # RDOS
    tt <- system.time(rdos_scores <- DDoutlier::RDOS(X)) # using default parameters
    roc_obj <- roc(labs, rdos_scores, direction="<")
    rdos_roc[kk, i] <- roc_obj$auc
    rdos_time[ll, ] <- tt

  }
}

# PLOT F-MEASURE
str_mean <- colMeans(stray_fmeasure)
lookout1_mean <- colMeans(lookout1_fmeasure)
lookoutOld_mean <- colMeans(lookoutOld_fmeasure)
hdoutliers_mean <- colMeans(hdoutliers_fmeasure)

str_se <- apply(stray_fmeasure, 2, sd)/sqrt(10)
lookout1_se <- apply(lookout1_fmeasure, 2, sd)/sqrt(10)
lookoutOld_se <- apply(lookoutOld_fmeasure, 2, sd)/sqrt(10)
hdoutliers_se <- apply(hdoutliers_fmeasure, 2, sd)/sqrt(10)

dfl1 <- tibble(
  Iteration = seq(10),
  stray = str_mean,
  lookoutNew = lookout1_mean,
  lookoutOld = lookoutOld_mean,
  HDoutliers = hdoutliers_mean
 ) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'Fmeasure') |>
  rename(mean = value)

dfl1se <- tibble(
  Iteration = seq(10),
  stray = str_se,
  lookoutNew = lookout1_se,
  lookoutOld = lookoutOld_se,
  HDoutliers = hdoutliers_se
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'Fmeasure') |>
  rename(se = value)

dfl1 <- dfl1 |>
  left_join(dfl1se, by = c("Iteration", "Algorithm", "Metric")) |>
  relocate(Iteration, Algorithm, Metric, mean, se)

# PLOT GEOMETRIC MEAN OF SENSITIVITY AND SPECIFICITY
str_mean <- colMeans(stray_gmean)
lookout1_mean <- colMeans(lookout1_gmean)
lookoutOld_mean <- colMeans(lookoutOld_gmean)
hdoutliers_mean <- colMeans(hdoutliers_gmean)

str_se <- apply(stray_gmean, 2, sd)/sqrt(10)
lookout1_se <- apply(lookout1_gmean, 2, sd)/sqrt(10)
lookoutOld_se <- apply(lookoutOld_gmean, 2, sd)/sqrt(10)
hdoutliers_se <- apply(hdoutliers_gmean, 2, sd)/sqrt(10)


dfl2 <- tibble(
  Iteration = seq(10),
  stray = str_mean,
  lookoutNew = lookout1_mean,
  lookoutOld = lookoutOld_mean,
  HDoutliers = hdoutliers_mean
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'Gmean') |>
  rename(mean = value)

dfl2se <- tibble(
  Iteration = seq(10),
  stray = str_se,
  lookoutNew = lookout1_se,
  lookoutOld = lookoutOld_se,
  HDoutliers = hdoutliers_se
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'Gmean') |>
  rename(se = value)

dfl2 <- dfl2 |>
  left_join(dfl2se, by = c("Iteration", "Algorithm", "Metric")) |>
  relocate(Iteration, Algorithm, Metric, mean, se)


# AUC FOR KDEOS, RDOS AND LOOKOUT
kdeos_mean <- colMeans(kdeos_roc)
lookout1_mean <-  colMeans(lookout1_roc)
lookoutOld_mean <-  colMeans(lookoutOld_roc)
rdos_mean <-  colMeans(rdos_roc)

kdeos_se <- apply(kdeos_roc, 2, sd)/sqrt(10)
lookout1_se <- apply(lookout1_roc, 2, sd)/sqrt(10)
lookoutOld_se <- apply(lookoutOld_roc, 2, sd)/sqrt(10)
rdos_se <- apply(rdos_roc, 2, sd)/sqrt(10)

dfl3 <- tibble(
  Iteration = seq(10),
  kdeos = kdeos_mean,
  lookoutNew = lookout1_mean,
  lookoutOld = lookoutOld_mean,
  rdos = rdos_mean
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'AUC') |>
  rename(mean = value)


dfl3se <- tibble(
  Iteration = seq(10),
  kdeos = kdeos_se,
  lookoutNew = lookout1_se,
  lookoutOld = lookoutOld_se,
  rdos = rdos_se
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'AUC') |>
  rename(se = value)

dfl3 <- dfl3 |>
  left_join(dfl3se, by = c("Iteration", "Algorithm", "Metric")) |>
  relocate(Iteration, Algorithm, Metric, mean, se)

dfl <- bind_rows(dfl1, dfl2, dfl3)
write.csv( dfl, "Data_Output/For_Paper/Experiment_5_Comparison_with_Other_Methods_Results.csv", row.names = FALSE)

g3 <- ggplot(dfl, aes(x=Iteration, y=mean, color = Algorithm)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
  geom_line(aes(color=Algorithm), linewidth=1) +
  ylab("Performance") +
  facet_wrap(~Metric) +
  scale_x_continuous(breaks=2*(1:5)) +
  theme_bw()
g3


# TIME TAKEN
dftime <- tibble(
  Run = seq(100),
  stray = stray_time[ ,3],
  lookoutNew = lookout1_time[ ,3],
  lookoutOld = lookoutOld_time[ ,3],
  HDoutliers = hdoutliers_time[ ,3],
  kdeos = kdeos_time[ ,3],
  rdos = rdos_time[ ,3]
) %>%
  pivot_longer(-Run, names_to = "Algorithm")

write.csv(dftime, "Data_Output/For_Paper/Time_Taken_For_Experiment_5_Comparison_with_Other_Methods_Results.csv", row.names = FALSE)




# ----------------------------------------------------------------------------------
# TASK 02 : EXPERIMENT 6
# ----------------------------------------------------------------------------------

set.seed(2025)
values <- rep(0, 10)
pp <- 10
hdoutliers_gmean <- hdoutliers_fmeasure <- lookout1_fmeasure <-
  lookoutOld_fmeasure <- lookout1_gmean <- lookoutOld_gmean <-
  stray_gmean <- stray_fmeasure <- matrix(0, nrow = pp, ncol = 10)

lookout1_roc <- lookoutOld_roc <- kdeos_roc <- rdos_roc <- matrix(0, nrow=pp, ncol=10)

hdoutliers_time <-  lookout1_time <- lookoutOld_time <- stray_time <- kdeos_time <- rdos_time <- matrix(0, nrow=pp*10, ncol=5)


for (kk in seq(pp)) {
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
  labs <- c(rep(0, nn-5), rep(1, 5))

  for (i in seq(10)) {
    X[nn - 5 + seq(5), 1] <- rnorm(5, 5 - (i-1)*0.5, sd = 0.1)
    X[nn - 5 + seq(5), 2] <- rnorm(5, 0, sd = 0.1)

    ll <- (kk-1)*10 + i

    # STRAY
    tt <- system.time(strayout <- stray::find_HDoutliers(X, knnsearchtype = "kd_tree", alpha=0.05))
    straylabs <- rep(0, nn)
    straylabs[strayout$outliers] <- 1
    strayoutput <- diff_metrics(labs, straylabs)
    stray_gmean[kk, i] <- strayoutput$gmean
    stray_fmeasure[kk, i] <- strayoutput$fmeasure
    stray_time[ll, ] <- tt

    # LOOKOUT - NEW
    tt1 <- system.time(lookoutobj1 <- lookout::lookout(X,
                                                       alpha = 0.05,
                                                       unitize = TRUE,
                                                       normalize = FALSE,
                                                       bw_para = 0.98,
                                                       version = 2,
                                                       bw_power = NA))
    lookoutlabs1 <- rep(0, nn)
    lookoutlabs1[lookoutobj1$outliers[ ,1]] <- 1
    lookoutput1 <- diff_metrics(labs, lookoutlabs1)
    lookout1_gmean[kk, i] <- lookoutput1$gmean
    lookout1_fmeasure[kk, i] <- lookoutput1$fmeasure
    lookout1_scores <- lookoutobj1$outlier_scores
    roc_obj1 <- roc(labs, lookout1_scores, direction="<")
    lookout1_roc[kk, i] <- roc_obj1$auc
    lookout1_time[ll, ] <- tt1


    # LOOKOUT - OLD
    tt3 <- system.time(lookoutobjold <-lookout::lookout(X,
                                                        alpha = 0.05,
                                                        unitize = TRUE,
                                                        normalize = FALSE,
                                                        bw_para = 1,
                                                        version = 1,
                                                        bw_power = NA))
    lookoutlabsold <- rep(0, nn)
    lookoutlabsold[lookoutobjold$outliers[ ,1]] <- 1
    lookoutputold <- diff_metrics(labs, lookoutlabsold)
    lookoutOld_gmean[kk, i] <- lookoutputold$gmean
    lookoutOld_fmeasure[kk, i] <- lookoutputold$fmeasure
    lookoutOld_scores <- lookoutobjold$outlier_scores
    roc_objold <- roc(labs, lookoutOld_scores, direction="<")
    lookoutOld_roc[kk, i] <- roc_objold$auc
    lookoutOld_time[ll, ] <- tt3

    # HDOUTLIERS
    tt <- system.time(hdoutobj <- HDoutliers(X, alpha=0.05))
    hdoutlabs <- rep(0, dim(X)[1])
    hdoutlabs[hdoutobj] <- 1
    hdoutput <- diff_metrics(labs, hdoutlabs)
    hdoutliers_gmean[kk, i] <- hdoutput$gmean
    hdoutliers_fmeasure[kk, i] <- hdoutput$fmeasure
    hdoutliers_time[ll, ] <- tt

    # KDEOS
    tt <- system.time(kdeos_scores <- DDoutlier::KDEOS(X)) # using default parameters
    roc_obj <- roc(labs, kdeos_scores, direction="<")
    kdeos_roc[kk, i] <- roc_obj$auc
    kdeos_time[ll, ] <- tt

    # RDOS
    tt <- system.time(rdos_scores <- DDoutlier::RDOS(X)) # using default parameters
    roc_obj <- roc(labs, rdos_scores, direction="<")
    rdos_roc[kk, i] <- roc_obj$auc
    rdos_time[ll, ] <- tt

  }
}

# PLOT F-MEASURE
str_mean <- colMeans(stray_fmeasure)
lookout1_mean <- colMeans(lookout1_fmeasure)
lookoutOld_mean <- colMeans(lookoutOld_fmeasure)
hdoutliers_mean <- colMeans(hdoutliers_fmeasure)

str_se <- apply(stray_fmeasure, 2, sd)/sqrt(10)
lookout1_se <- apply(lookout1_fmeasure, 2, sd)/sqrt(10)
lookoutOld_se <- apply(lookoutOld_fmeasure, 2, sd)/sqrt(10)
hdoutliers_se <- apply(hdoutliers_fmeasure, 2, sd)/sqrt(10)

dfl1 <- tibble(
  Iteration = seq(10),
  stray = str_mean,
  lookoutNew = lookout1_mean,
  lookoutOld = lookoutOld_mean,
  HDoutliers = hdoutliers_mean
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'Fmeasure') |>
  rename(mean = value)

dfl1se <- tibble(
  Iteration = seq(10),
  stray = str_se,
  lookoutNew = lookout1_se,
  lookoutOld = lookoutOld_se,
  HDoutliers = hdoutliers_se
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'Fmeasure') |>
  rename(se = value)

dfl1 <- dfl1 |>
  left_join(dfl1se, by = c("Iteration", "Algorithm", "Metric")) |>
  relocate(Iteration, Algorithm, Metric, mean, se)

# PLOT GEOMETRIC MEAN OF SENSITIVITY AND SPECIFICITY
str_mean <- colMeans(stray_gmean)
lookout1_mean <- colMeans(lookout1_gmean)
lookoutOld_mean <- colMeans(lookoutOld_gmean)
hdoutliers_mean <- colMeans(hdoutliers_gmean)

str_se <- apply(stray_gmean, 2, sd)/sqrt(10)
lookout1_se <- apply(lookout1_gmean, 2, sd)/sqrt(10)
lookoutOld_se <- apply(lookoutOld_gmean, 2, sd)/sqrt(10)
hdoutliers_se <- apply(hdoutliers_gmean, 2, sd)/sqrt(10)


dfl2 <- tibble(
  Iteration = seq(10),
  stray = str_mean,
  lookoutNew = lookout1_mean,
  lookoutOld = lookoutOld_mean,
  HDoutliers = hdoutliers_mean
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'Gmean') |>
  rename(mean = value)

dfl2se <- tibble(
  Iteration = seq(10),
  stray = str_se,
  lookoutNew = lookout1_se,
  lookoutOld = lookoutOld_se,
  HDoutliers = hdoutliers_se
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'Gmean') |>
  rename(se = value)

dfl2 <- dfl2 |>
  left_join(dfl2se, by = c("Iteration", "Algorithm", "Metric")) |>
  relocate(Iteration, Algorithm, Metric, mean, se)


# AUC FOR KDEOS, RDOS AND LOOKOUT
kdeos_mean <- colMeans(kdeos_roc)
lookout1_mean <-  colMeans(lookout1_roc)
lookoutOld_mean <-  colMeans(lookoutOld_roc)
rdos_mean <-  colMeans(rdos_roc)

kdeos_se <- apply(kdeos_roc, 2, sd)/sqrt(10)
lookout1_se <- apply(lookout1_roc, 2, sd)/sqrt(10)
lookoutOld_se <- apply(lookoutOld_roc, 2, sd)/sqrt(10)
rdos_se <- apply(rdos_roc, 2, sd)/sqrt(10)

dfl3 <- tibble(
  Iteration = seq(10),
  kdeos = kdeos_mean,
  lookoutNew = lookout1_mean,
  lookoutOld = lookoutOld_mean,
  rdos = rdos_mean
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'AUC') |>
  rename(mean = value)


dfl3se <- tibble(
  Iteration = seq(10),
  kdeos = kdeos_se,
  lookoutNew = lookout1_se,
  lookoutOld = lookoutOld_se,
  rdos = rdos_se
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'AUC') |>
  rename(se = value)

dfl3 <- dfl3 |>
  left_join(dfl3se, by = c("Iteration", "Algorithm", "Metric")) |>
  relocate(Iteration, Algorithm, Metric, mean, se)

dfl <- bind_rows(dfl1, dfl2, dfl3)
write.csv( dfl, "Data_Output/For_Paper/Experiment_6_Comparison_with_Other_Methods_Results.csv", row.names = FALSE)

g3 <- ggplot(dfl, aes(x=Iteration, y=mean, color = Algorithm)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
  geom_line(aes(color=Algorithm), linewidth=1) +
  ylab("Performance") +
  facet_wrap(~Metric) +
  scale_x_continuous(breaks=2*(1:5)) +
  theme_bw()
g3


# TIME TAKEN
dftime <- tibble(
  Run = seq(100),
  stray = stray_time[ ,3],
  lookoutNew = lookout1_time[ ,3],
  lookoutOld = lookoutOld_time[ ,3],
  HDoutliers = hdoutliers_time[ ,3],
  kdeos = kdeos_time[ ,3],
  rdos = rdos_time[ ,3]
) %>%
  pivot_longer(-Run, names_to = "Algorithm")

write.csv(dftime, "Data_Output/For_Paper/Time_Taken_For_Experiment_6_Comparison_with_Other_Methods_Results.csv", row.names = FALSE)



# ----------------------------------------------------------------------------------
# TASK 03 : EXPERIMENT 7
# ----------------------------------------------------------------------------------
set.seed(2025)
values <- rep(0, 10)
pp <- 10
hdoutliers_gmean <- hdoutliers_fmeasure <- lookout1_fmeasure <-
   lookoutOld_fmeasure <- lookout1_gmean <-  lookoutOld_gmean <-
  stray_gmean <- stray_fmeasure <- matrix(0, nrow = pp, ncol = 20)

lookout1_roc <- lookoutOld_roc <- kdeos_roc <- rdos_roc <- matrix(0, nrow=pp, ncol=20)

hdoutliers_time <-  lookout1_time <- lookoutOld_time <- stray_time <- kdeos_time <- rdos_time <- matrix(0, nrow=pp*20, ncol=5)

values <- rep(0, 10)
pp <- 10
dd <- 19
nn <- 500
labs <- c(rep(0, nn-1), 1)

for (kk in seq(pp)) {
  X <- matrix(runif(nn*(dd+1)), ncol=dd+1, nrow=nn)
  colnames(X) <- paste("x", 1:20, sep = "")

  for (i in seq(20)) {
    X[nn, seq(i)] <- rep(0.9, i)
    ll <- (kk-1)*10 + i

    # STRAY
    tt <- system.time(strayout <- stray::find_HDoutliers(X, knnsearchtype = "kd_tree", alpha=0.05))
    straylabs <- rep(0, nn)
    straylabs[strayout$outliers] <- 1
    strayoutput <- diff_metrics(labs, straylabs)
    stray_gmean[kk, i] <- strayoutput$gmean
    stray_fmeasure[kk, i] <- strayoutput$fmeasure
    stray_time[ll, ] <- tt

    # LOOKOUT - NEW
    tt1 <- system.time(lookoutobj1 <- lookout::lookout(X,
                                                       alpha = 0.05,
                                                       unitize = TRUE,
                                                       normalize = FALSE,
                                                       bw_para = 0.98,
                                                       version = 2,
                                                       bw_power = NA))
    lookoutlabs1 <- rep(0, nn)
    lookoutlabs1[lookoutobj1$outliers[ ,1]] <- 1
    lookoutput1 <- diff_metrics(labs, lookoutlabs1)
    lookout1_gmean[kk, i] <- lookoutput1$gmean
    lookout1_fmeasure[kk, i] <- lookoutput1$fmeasure
    lookout1_scores <- lookoutobj1$outlier_scores
    roc_obj1 <- roc(labs, lookout1_scores, direction="<")
    lookout1_roc[kk, i] <- roc_obj1$auc
    lookout1_time[ll, ] <- tt1


    # LOOKOUT - OLD
    tt3 <- system.time(lookoutobjold <- lookout::lookout(X,
                                                         alpha = 0.05,
                                                         unitize = TRUE,
                                                         normalize = FALSE,
                                                         bw_para = 1,
                                                         version = 1,
                                                         bw_power = NA))
    lookoutlabsold <- rep(0, nn)
    lookoutlabsold[lookoutobjold$outliers[ ,1]] <- 1
    lookoutputold <- diff_metrics(labs, lookoutlabsold)
    lookoutOld_gmean[kk, i] <- lookoutputold$gmean
    lookoutOld_fmeasure[kk, i] <- lookoutputold$fmeasure
    lookoutOld_scores <- lookoutobjold$outlier_scores
    roc_objold <- roc(labs, lookoutOld_scores, direction="<")
    lookoutOld_roc[kk, i] <- roc_objold$auc
    lookoutOld_time[ll, ] <- tt3

    # HDOUTLIERS
    tt <- system.time(hdoutobj <- HDoutliers(X, alpha=0.05))
    hdoutlabs <- rep(0, dim(X)[1])
    hdoutlabs[hdoutobj] <- 1
    hdoutput <- diff_metrics(labs, hdoutlabs)
    hdoutliers_gmean[kk, i] <- hdoutput$gmean
    hdoutliers_fmeasure[kk, i] <- hdoutput$fmeasure
    hdoutliers_time[ll, ] <- tt

    # KDEOS
    tt <- system.time(kdeos_scores <- DDoutlier::KDEOS(X)) # using default parameters
    roc_obj <- roc(labs, kdeos_scores, direction="<")
    kdeos_roc[kk, i] <- roc_obj$auc
    kdeos_time[ll, ] <- tt

    # RDOS
    tt <- system.time(rdos_scores <- DDoutlier::RDOS(X)) # using default parameters
    roc_obj <- roc(labs, rdos_scores, direction="<")
    rdos_roc[kk, i] <- roc_obj$auc
    rdos_time[ll, ] <- tt

  }
}

# PLOT F-MEASURE
str_mean <- colMeans(stray_fmeasure)
lookout1_mean <- colMeans(lookout1_fmeasure)
lookoutOld_mean <- colMeans(lookoutOld_fmeasure)
hdoutliers_mean <- colMeans(hdoutliers_fmeasure)

str_se <- apply(stray_fmeasure, 2, sd)/sqrt(20)
lookout1_se <- apply(lookout1_fmeasure, 2, sd)/sqrt(20)
lookoutOld_se <- apply(lookoutOld_fmeasure, 2, sd)/sqrt(20)
hdoutliers_se <- apply(hdoutliers_fmeasure, 2, sd)/sqrt(20)

dfl1 <- tibble(
  Iteration = seq(20),
  stray = str_mean,
  lookoutNew = lookout1_mean,
  lookoutOld = lookoutOld_mean,
  HDoutliers = hdoutliers_mean
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'Fmeasure') |>
  rename(mean = value)

dfl1se <- tibble(
  Iteration = seq(20),
  stray = str_se,
  lookoutNew = lookout1_se,
  lookoutOld = lookoutOld_se,
  HDoutliers = hdoutliers_se
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'Fmeasure') |>
  rename(se = value)

dfl1 <- dfl1 |>
  left_join(dfl1se, by = c("Iteration", "Algorithm", "Metric")) |>
  relocate(Iteration, Algorithm, Metric, mean, se)

# PLOT GEOMETRIC MEAN OF SENSITIVITY AND SPECIFICITY
str_mean <- colMeans(stray_gmean)
lookout1_mean <- colMeans(lookout1_gmean)
lookoutOld_mean <- colMeans(lookoutOld_gmean)
hdoutliers_mean <- colMeans(hdoutliers_gmean)

str_se <- apply(stray_gmean, 2, sd)/sqrt(20)
lookout1_se <- apply(lookout1_gmean, 2, sd)/sqrt(20)
lookoutOld_se <- apply(lookoutOld_gmean, 2, sd)/sqrt(20)
hdoutliers_se <- apply(hdoutliers_gmean, 2, sd)/sqrt(20)


dfl2 <- tibble(
  Iteration = seq(20),
  stray = str_mean,
  lookoutNew = lookout1_mean,
  lookoutOld = lookoutOld_mean,
  HDoutliers = hdoutliers_mean
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'Gmean') |>
  rename(mean = value)

dfl2se <- tibble(
  Iteration = seq(20),
  stray = str_se,
  lookoutNew = lookout1_se,
  lookoutOld = lookoutOld_se,
  HDoutliers = hdoutliers_se
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'Gmean') |>
  rename(se = value)

dfl2 <- dfl2 |>
  left_join(dfl2se, by = c("Iteration", "Algorithm", "Metric")) |>
  relocate(Iteration, Algorithm, Metric, mean, se)


# AUC FOR KDEOS, RDOS AND LOOKOUT
kdeos_mean <- colMeans(kdeos_roc)
lookout1_mean <-  colMeans(lookout1_roc)
lookoutOld_mean <-  colMeans(lookoutOld_roc)
rdos_mean <-  colMeans(rdos_roc)

kdeos_se <- apply(kdeos_roc, 2, sd)/sqrt(20)
lookout1_se <- apply(lookout1_roc, 2, sd)/sqrt(20)
lookoutOld_se <- apply(lookoutOld_roc, 2, sd)/sqrt(20)
rdos_se <- apply(rdos_roc, 2, sd)/sqrt(20)

dfl3 <- tibble(
  Iteration = seq(20),
  kdeos = kdeos_mean,
  lookoutNew = lookout1_mean,
  lookoutOld = lookoutOld_mean,
  rdos = rdos_mean
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'AUC') |>
  rename(mean = value)


dfl3se <- tibble(
  Iteration = seq(20),
  kdeos = kdeos_se,
  lookoutNew = lookout1_se,
  lookoutOld = lookoutOld_se,
  rdos = rdos_se
) %>%
  pivot_longer(-Iteration, names_to = "Algorithm") %>%
  mutate(Metric = 'AUC') |>
  rename(se = value)

dfl3 <- dfl3 |>
  left_join(dfl3se, by = c("Iteration", "Algorithm", "Metric")) |>
  relocate(Iteration, Algorithm, Metric, mean, se)

dfl <- bind_rows(dfl1, dfl2, dfl3)
write.csv( dfl, "Data_Output/For_Paper/Experiment_7_Comparison_with_Other_Methods_Results.csv", row.names = FALSE)

g3 <- ggplot(dfl, aes(x=Iteration, y=mean, color = Algorithm)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
  geom_line(aes(color=Algorithm), linewidth=1) +
  ylab("Performance") +
  facet_wrap(~Metric) +
  scale_x_continuous(breaks=2*(1:10)) +
  theme_bw()
g3


# TIME TAKEN
dftime <- tibble(
  Run = seq(200),
  stray = stray_time[ ,3],
  lookoutNew = lookout1_time[ ,3],
  lookoutOld = lookoutOld_time[ ,3],
  HDoutliers = hdoutliers_time[ ,3],
  kdeos = kdeos_time[ ,3],
  rdos = rdos_time[ ,3]
) %>%
  pivot_longer(-Run, names_to = "Algorithm")

write.csv(dftime, "Data_Output/For_Paper/Time_Taken_For_Experiment_7_Comparison_with_Other_Methods_Results.csv", row.names = FALSE)


