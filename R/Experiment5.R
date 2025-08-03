# Experiment 5: Comparison with other methods

generate_exp5 <- function(iterate) {
  X <- data.frame(
    x2 = rnorm(405),
    x3 = rnorm(405),
    x4 = rnorm(405),
    x5 = rnorm(405),
    x6 = rnorm(405)
  )
  x1_1 <- rnorm(400)
  x1_2 <- rnorm(5, mean = 2 + (iterate - 1) * 0.5, sd = 0.2)
  X$x1 <- c(x1_1, x1_2)
  X$Points <- c(rep("Non-anomaly", 400), rep("Anomaly", 5))
  as_tibble(X)
}

run_synthetic_exp5 <- function() {
  pp <- 10
  hdoutliers_gmean <- hdoutliers_fmeasure <- lookout1_fmeasure <-
    lookoutOld_fmeasure <- lookout1_gmean <- lookoutOld_gmean <- stray_gmean <-
      stray_fmeasure <- lookout1_roc <- lookoutOld_roc <- kdeos_roc <- rdos_roc <-
        matrix(
          0,
          nrow = pp,
          ncol = 10
        )
  hdoutliers_time <- lookout1_time <- lookoutOld_time <- stray_time <-
    kdeos_time <- rdos_time <-
      matrix(
        0,
        nrow = pp * 10,
        ncol = 5
      )

  for (kk in seq(pp)) {
    for (i in seq(10)) {
      X <- generate_exp5(i)
      labs <- X$Points == "Anomaly"
      X <- X[, 1:6]
      ll <- (kk - 1) * 10 + i

      # STRAY
      tt <- system.time(
        strayout <- stray::find_HDoutliers(
          X,
          knnsearchtype = "kd_tree",
          alpha = 0.01
        )
      )
      straylabs <- rep(0, 405)
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
      lookoutlabs1 <- rep(0, 405)
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
      lookoutlabsold <- rep(0, 405)
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
      hdoutlabs <- rep(0, NROW(X))
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

  # Process results
  str_mean <- colMeans(stray_fmeasure)
  lookout1_mean <- colMeans(lookout1_fmeasure)
  lookoutOld_mean <- colMeans(lookoutOld_fmeasure)
  hdoutliers_mean <- colMeans(hdoutliers_fmeasure)

  str_se <- apply(stray_fmeasure, 2, sd) / sqrt(10)
  lookout1_se <- apply(lookout1_fmeasure, 2, sd) / sqrt(10)
  lookoutOld_se <- apply(lookoutOld_fmeasure, 2, sd) / sqrt(10)
  hdoutliers_se <- apply(hdoutliers_fmeasure, 2, sd) / sqrt(10)

  dfl1 <- tibble(
    Iteration = seq(10),
    stray = str_mean,
    lookoutNew = lookout1_mean,
    lookoutOld = lookoutOld_mean,
    HDoutliers = hdoutliers_mean
  ) |>
    pivot_longer(-Iteration, names_to = "Algorithm") |>
    mutate(Metric = "Fmeasure") |>
    rename(mean = value)

  dfl1se <- tibble(
    Iteration = seq(10),
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

  str_se <- apply(stray_gmean, 2, sd) / sqrt(10)
  lookout1_se <- apply(lookout1_gmean, 2, sd) / sqrt(10)
  lookoutOld_se <- apply(lookoutOld_gmean, 2, sd) / sqrt(10)
  hdoutliers_se <- apply(hdoutliers_gmean, 2, sd) / sqrt(10)

  dfl2 <- tibble(
    Iteration = seq(10),
    stray = str_mean,
    lookoutNew = lookout1_mean,
    lookoutOld = lookoutOld_mean,
    HDoutliers = hdoutliers_mean
  ) |>
    pivot_longer(-Iteration, names_to = "Algorithm") |>
    mutate(Metric = "Gmean") |>
    rename(mean = value)

  dfl2se <- tibble(
    Iteration = seq(10),
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

  kdeos_se <- apply(kdeos_roc, 2, sd) / sqrt(10)
  lookout1_se <- apply(lookout1_roc, 2, sd) / sqrt(10)
  lookoutOld_se <- apply(lookoutOld_roc, 2, sd) / sqrt(10)
  rdos_se <- apply(rdos_roc, 2, sd) / sqrt(10)

  dfl3 <- tibble(
    Iteration = seq(10),
    kdeos = kdeos_mean,
    lookoutNew = lookout1_mean,
    lookoutOld = lookoutOld_mean,
    rdos = rdos_mean
  ) |>
    pivot_longer(-Iteration, names_to = "Algorithm") |>
    mutate(Metric = "AUC") |>
    rename(mean = value)

  dfl3se <- tibble(
    Iteration = seq(10),
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
    Run = seq(100),
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
      "Data_Output/Time_Taken_For_Experiment_5_Comparison_with_Other_Methods_Results.csv"
    ),
    row.names = FALSE
  )

  dfl
}


create_figure_exp5 <- function(results) {
  set_ggplot_options()

  g4 <- ggplot(results, aes(x = Iteration, y = mean, color = Algorithm)) +
    geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.1) +
    geom_line(aes(color = Algorithm), linewidth = 1) +
    ylab("Performance") +
    facet_wrap(~Metric) +
    scale_x_continuous(breaks = 2 * (1:5)) +
    theme(legend.position = "bottom")

  # Plot data
  g1 <- generate_exp5(3) |>
    ggplot(aes(x1, x2)) +
    geom_point(aes(color = Points))
  X <- generate_exp5(9)
  g2 <- X |>
    ggplot(aes(x1, x2)) +
    geom_point(aes(color = Points))
  g3 <- X |>
    ggplot(aes(x3, x4)) +
    geom_point(aes(color = Points))

  p <- patchwork::wrap_plots(g1, g2, patchwork::guide_area(), g3) +
    patchwork::plot_layout(guides = "collect")

  # Create figure
  dir.create("Figures", showWarnings = FALSE)
  fig <- here::here("Figures/Exp5_Data_and_Results.pdf")
  cairo_pdf(file = fig, width = 8, height = 5)
  print(p | g4)
  crop::dev.off.crop(fig)

  fig
}
