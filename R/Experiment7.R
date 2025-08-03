# Experiment 7: High-dimensional comparison

generate_exp7 <- function(iterate) {
  nn <- 500
  dd <- 19
  X <- matrix(runif(nn * (dd + 1)), ncol = dd + 1, nrow = nn)
  colnames(X) <- paste("x", seq(dd + 1), sep = "")
  X[nn, seq(iterate)] <- rep(0.9, iterate)
  X <- as_tibble(X)
  X$Points <- c(rep("Non-anomaly", nn - 1), rep("Anomaly", 1))
  X
}
run_synthetic_exp7 <- function() {
  pp <- 10
  hdoutliers_gmean <- hdoutliers_fmeasure <- lookout1_fmeasure <-
    lookoutOld_fmeasure <- lookout1_gmean <- lookoutOld_gmean <- stray_gmean <-
      stray_fmeasure <- lookout1_roc <- lookoutOld_roc <- kdeos_roc <- rdos_roc <-
        matrix(
          0,
          nrow = pp,
          ncol = 20
        )
  hdoutliers_time <- lookout1_time <- lookoutOld_time <- stray_time <- kdeos_time <- rdos_time <- matrix(
    0,
    nrow = pp * 20,
    ncol = 5
  )

  for (kk in seq(pp)) {
    for (i in seq(20)) {
      X <- generate_exp7(i)
      labs <- X$Points == "Anomaly"
      X <- X[, 1:20]
      ll <- (kk - 1) * 10 + i

      # STRAY
      tt <- system.time(
        strayout <- stray::find_HDoutliers(
          X,
          knnsearchtype = "kd_tree",
          alpha = 0.01
        )
      )
      straylabs <- rep(0, NROW(X))
      straylabs[strayout$outliers] <- 1
      strayoutput <- diff_metrics(labs, straylabs)
      stray_gmean[kk, i] <- strayoutput$gmean
      stray_fmeasure[kk, i] <- strayoutput$fmeasure
      stray_time[ll, ] <- tt

      # LOOKOUT - NEW
      tt1 <- system.time(
        lookoutobj1 <- lookout::lookout(
          X,
          alpha = 0.01,
          scale = TRUE,
          gamma = 0.98,
          old_version = FALSE
        )
      )
      lookoutlabs1 <- rep(0, NROW(X))
      lookoutlabs1[lookoutobj1$outliers[, 1]] <- 1
      lookoutput1 <- diff_metrics(labs, lookoutlabs1)
      lookout1_gmean[kk, i] <- lookoutput1$gmean
      lookout1_fmeasure[kk, i] <- lookoutput1$fmeasure
      lookout1_scores <- lookoutobj1$outlier_scores
      roc_obj1 <- suppressMessages(pROC::roc(
        labs,
        lookout1_scores,
        direction = "<"
      ))
      lookout1_roc[kk, i] <- roc_obj1$auc
      lookout1_time[ll, ] <- tt1

      # LOOKOUT - OLD
      tt3 <- system.time(
        lookoutobjold <- lookout::lookout(
          X,
          alpha = 0.01,
          scale = TRUE,
          gamma = 1,
          old_version = TRUE
        )
      )
      lookoutlabsold <- rep(0, NROW(X))
      lookoutlabsold[lookoutobjold$outliers[, 1]] <- 1
      lookoutputold <- diff_metrics(labs, lookoutlabsold)
      lookoutOld_gmean[kk, i] <- lookoutputold$gmean
      lookoutOld_fmeasure[kk, i] <- lookoutputold$fmeasure
      lookoutOld_scores <- lookoutobjold$outlier_scores
      roc_objold <- suppressMessages(pROC::roc(
        labs,
        lookoutOld_scores,
        direction = "<"
      ))
      lookoutOld_roc[kk, i] <- roc_objold$auc
      lookoutOld_time[ll, ] <- tt3

      # HDOUTLIERS
      tt <- system.time(hdoutobj <- HDoutliers::HDoutliers(X, alpha = 0.01))
      hdoutlabs <- rep(0, dim(X)[1])
      hdoutlabs[hdoutobj] <- 1
      hdoutput <- diff_metrics(labs, hdoutlabs)
      hdoutliers_gmean[kk, i] <- hdoutput$gmean
      hdoutliers_fmeasure[kk, i] <- hdoutput$fmeasure
      hdoutliers_time[ll, ] <- tt

      # KDEOS
      tt <- system.time(kdeos_scores <- DDoutlier::KDEOS(X)) # using default parameters
      roc_obj <- suppressMessages(pROC::roc(
        labs,
        kdeos_scores,
        direction = "<"
      ))
      kdeos_roc[kk, i] <- roc_obj$auc
      kdeos_time[ll, ] <- tt

      # RDOS
      tt <- system.time(rdos_scores <- DDoutlier::RDOS(X)) # using default parameters
      roc_obj <- suppressMessages(pROC::roc(labs, rdos_scores, direction = "<"))
      rdos_roc[kk, i] <- roc_obj$auc
      rdos_time[ll, ] <- tt
    }
  }

  # Process results (similar to exp5 but with 20 iterations)
  str_mean <- colMeans(stray_fmeasure)
  lookout1_mean <- colMeans(lookout1_fmeasure)
  lookoutOld_mean <- colMeans(lookoutOld_fmeasure)
  hdoutliers_mean <- colMeans(hdoutliers_fmeasure)

  str_se <- apply(stray_fmeasure, 2, sd) / sqrt(20)
  lookout1_se <- apply(lookout1_fmeasure, 2, sd) / sqrt(20)
  lookoutOld_se <- apply(lookoutOld_fmeasure, 2, sd) / sqrt(20)
  hdoutliers_se <- apply(hdoutliers_fmeasure, 2, sd) / sqrt(20)

  dfl1 <- tibble(
    Iteration = seq(20),
    stray = str_mean,
    lookoutNew = lookout1_mean,
    lookoutOld = lookoutOld_mean,
    HDoutliers = hdoutliers_mean
  ) |>
    pivot_longer(-Iteration, names_to = "Algorithm") |>
    mutate(Metric = "Fmeasure") |>
    rename(mean = value)

  dfl1se <- tibble(
    Iteration = seq(20),
    stray = str_se,
    lookoutNew = lookout1_se,
    lookoutOld = lookoutOld_se,
    HDoutliers = hdoutliers_se
  ) |>
    pivot_longer(-Iteration, names_to = "Algorithm") |>
    mutate(Metric = "Fmeasure") |>
    rename(se = value)

  dfl1 <- dfl1 |>
    left_join(dfl1se, by = c("Iteration", "Algorithm", "Metric")) |>
    relocate(Iteration, Algorithm, Metric, mean, se)

  # PLOT GEOMETRIC MEAN OF SENSITIVITY AND SPECIFICITY
  str_mean <- colMeans(stray_gmean)
  lookout1_mean <- colMeans(lookout1_gmean)
  lookoutOld_mean <- colMeans(lookoutOld_gmean)
  hdoutliers_mean <- colMeans(hdoutliers_gmean)

  str_se <- apply(stray_gmean, 2, sd) / sqrt(20)
  lookout1_se <- apply(lookout1_gmean, 2, sd) / sqrt(20)
  lookoutOld_se <- apply(lookoutOld_gmean, 2, sd) / sqrt(20)
  hdoutliers_se <- apply(hdoutliers_gmean, 2, sd) / sqrt(20)

  dfl2 <- tibble(
    Iteration = seq(20),
    stray = str_mean,
    lookoutNew = lookout1_mean,
    lookoutOld = lookoutOld_mean,
    HDoutliers = hdoutliers_mean
  ) |>
    pivot_longer(-Iteration, names_to = "Algorithm") |>
    mutate(Metric = "Gmean") |>
    rename(mean = value)

  dfl2se <- tibble(
    Iteration = seq(20),
    stray = str_se,
    lookoutNew = lookout1_se,
    lookoutOld = lookoutOld_se,
    HDoutliers = hdoutliers_se
  ) |>
    pivot_longer(-Iteration, names_to = "Algorithm") |>
    mutate(Metric = "Gmean") |>
    rename(se = value)

  dfl2 <- dfl2 |>
    left_join(dfl2se, by = c("Iteration", "Algorithm", "Metric")) |>
    relocate(Iteration, Algorithm, Metric, mean, se)

  # AUC FOR KDEOS, RDOS AND LOOKOUT
  kdeos_mean <- colMeans(kdeos_roc)
  lookout1_mean <- colMeans(lookout1_roc)
  lookoutOld_mean <- colMeans(lookoutOld_roc)
  rdos_mean <- colMeans(rdos_roc)

  kdeos_se <- apply(kdeos_roc, 2, sd) / sqrt(20)
  lookout1_se <- apply(lookout1_roc, 2, sd) / sqrt(20)
  lookoutOld_se <- apply(lookoutOld_roc, 2, sd) / sqrt(20)
  rdos_se <- apply(rdos_roc, 2, sd) / sqrt(20)

  dfl3 <- tibble(
    Iteration = seq(20),
    kdeos = kdeos_mean,
    lookoutNew = lookout1_mean,
    lookoutOld = lookoutOld_mean,
    rdos = rdos_mean
  ) |>
    pivot_longer(-Iteration, names_to = "Algorithm") |>
    mutate(Metric = "AUC") |>
    rename(mean = value)

  dfl3se <- tibble(
    Iteration = seq(20),
    kdeos = kdeos_se,
    lookoutNew = lookout1_se,
    lookoutOld = lookoutOld_se,
    rdos = rdos_se
  ) |>
    pivot_longer(-Iteration, names_to = "Algorithm") |>
    mutate(Metric = "AUC") |>
    rename(se = value)

  dfl3 <- dfl3 |>
    left_join(dfl3se, by = c("Iteration", "Algorithm", "Metric")) |>
    relocate(Iteration, Algorithm, Metric, mean, se)

  dfl <- bind_rows(dfl1, dfl2, dfl3)
  dfl[dfl$Algorithm == "rdos", "Algorithm"] <- "RDOS"
  dfl[dfl$Algorithm == "kdeos", "Algorithm"] <- "KDEOS"

  # TIME TAKEN
  dftime <- tibble(
    Run = seq(200),
    stray = stray_time[, 3],
    lookoutNew = lookout1_time[, 3],
    lookoutOld = lookoutOld_time[, 3],
    HDoutliers = hdoutliers_time[, 3],
    kdeos = kdeos_time[, 3],
    rdos = rdos_time[, 3]
  ) |>
    pivot_longer(-Run, names_to = "Algorithm")

  write.csv(
    dftime,
    here::here(
      "Data_Output/Time_Taken_For_Experiment_7_Comparison_with_Other_Methods_Results.csv"
    ),
    row.names = FALSE
  )

  dfl
}

create_figure_exp7 <- function(results) {
  set_ggplot_options()

  g4 <- ggplot(results, aes(x = Iteration, y = mean, color = Algorithm)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
    geom_line(aes(color = Algorithm), linewidth = 1) +
    ylab("Performance") +
    facet_wrap(~Metric) +
    scale_x_continuous(breaks = (1:4) * 5) +
    theme(legend.position = "bottom")

  # plot data
  dobX <- bind_rows(
    generate_exp7(5) |> as_dobin() |> mutate(iterate = 5),
    generate_exp7(12) |> as_dobin() |> mutate(iterate = 12),
    generate_exp7(20) |> as_dobin() |> mutate(iterate = 20)
  )

  p <- dobX |>
    ggplot() +
    aes(D1, D2) +
    geom_point(aes(color = labels)) +
    facet_wrap(~iterate, strip.position = "right", ncol = 1) +
    theme(legend.position = "bottom")

  # Create figure
  dir.create("Figures", showWarnings = FALSE)
  fig <- here::here("Figures/Exp7_Data_and_Results.pdf")
  cairo_pdf(file = fig, width = 8, height = 5)
  print(p | g4)
  crop::dev.off.crop(fig)

  fig
}
