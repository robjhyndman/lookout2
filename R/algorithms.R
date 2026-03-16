# ------------------------------------------------------
# Common functions for Experiments 5, 6 and 7
# ------------------------------------------------------

algorithm_configs <- function() {
  list(
    stray = list(
      func = function(X, scale, alpha, beta, gamma) {
        stray::find_HDoutliers(X, knnsearchtype = "kd_tree", alpha = 0.01)
      },
      process = process
    ),
    lookout_new = list(
      func = function(X, scale, alpha, beta, gamma) {
        lookout::lookout(
          X,
          scale = scale,
          alpha = alpha,
          beta = beta,
          gamma = gamma,
          fast = FALSE,
          old_version = FALSE
        )
      },
      process = process_lookout
    ),
    lookout_old = list(
      func = function(X, scale, alpha, beta, gamma) {
        lookout::lookout(
          X,
          scale = scale,
          alpha = alpha,
          beta = beta,
          gamma = gamma,
          fast = FALSE,
          old_version = TRUE
        )
      },
      process = process_lookout
    ),
    hdoutliers = list(
      func = function(X, scale, alpha, beta, gamma) {
        HDoutliers::HDoutliers(X, alpha = 0.01)
      },
      process = process
    ),
    kdeos = list(
      func = function(X, scale, alpha, beta, gamma) DDoutlier::KDEOS(X),
      process = process_roc
    ),
    rdos = list(
      func = function(X, scale, alpha, beta, gamma) DDoutlier::RDOS(X),
      process = process_roc
    )
  )
}

# Generic function to run any algorithm
run_algorithm <- function(X, labs, algorithm, scale, alpha, beta, gamma) {
  tt <- system.time(result <- algorithm$func(X, scale, alpha, beta, gamma))
  processed <- algorithm$process(result, X, labs)
  c(processed, list(time = tt))
}

# Helper function to create metric data frames
create_metric_df <- function(results, metric_name) {
  pp <- NROW(results[[1]])
  reps <- NCOL(results[[1]])
  subset <- results[grepl(paste0("_", metric_name), names(results))]
  names(subset) <- gsub(paste0("_", metric_name), "", names(subset))
  means <- tibble(
    Iteration = seq(reps),
    as.data.frame(lapply(subset, colMeans))
  ) |>
    tidyr::pivot_longer(-Iteration, names_to = "Algorithm", values_to = "mean")
  ses <- tibble(
    Iteration = seq(reps),
    as.data.frame(lapply(subset, function(alg) {
      apply(alg, 2, sd) / sqrt(pp)
    }))
  ) |>
    tidyr::pivot_longer(-Iteration, names_to = "Algorithm", values_to = "se")
  left_join(means, ses, by = c("Iteration", "Algorithm")) |>
    mutate(Metric = metric_name)
}

# Process and format results
process_results <- function(results) {
  # Fmeasure
  bind_rows(
    create_metric_df(results, "fmeasure"),
    create_metric_df(results, "gmean"),
    create_metric_df(results, "auc")
  ) |>
    relocate(Iteration, Algorithm, Metric, mean, se)
}

# Main function
compare_algorithms <- function(
  experiment,
  generate_function,
  reps,
  pp,
  scale,
  alpha,
  beta,
  gamma
) {
  # Set up the structure to store results
  configs <- algorithm_configs()
  algorithms <- names(configs)
  metrics <- c("gmean", "fmeasure", "auc")
  results <- list()
  for (alg in algorithms) {
    for (metric in metrics) {
      results[[paste0(alg, "_", metric)]] <- matrix(
        NA_real_,
        nrow = pp,
        ncol = reps
      )
    }
    tt <- system.time(1)
    results[[paste0(alg, "_time")]] <- matrix(
      NA_real_,
      nrow = pp * reps,
      ncol = length(tt)
    )
    colnames(results[[paste0(alg, "_time")]]) <- names(tt)
  }

  # Main computation loop
  for (kk in seq(pp)) {
    for (i in seq(reps)) {
      # Generate data
      X <- generate_function(i)
      labs <- X$Points == "Anomaly"
      X$Points <- NULL
      ll <- (kk - 1) * reps + i

      # Run all algorithms
      for (alg in algorithms) {
        alg_result <- run_algorithm(
          X,
          labs,
          configs[[alg]],
          scale,
          alpha,
          beta,
          gamma
        )
        if (!is.null(alg_result$gmean)) {
          results[[paste0(alg, "_gmean")]][kk, i] <- alg_result$gmean
        }
        if (!is.null(alg_result$fmeasure)) {
          results[[paste0(alg, "_fmeasure")]][kk, i] <- alg_result$fmeasure
        }
        if (!is.null(alg_result$auc)) {
          results[[paste0(alg, "_auc")]][kk, i] <- alg_result$auc
        }
        results[[paste0(alg, "_time")]][ll, ] <- alg_result$time
      }
    }
  }

  save_time(experiment, results)
  process_results(results)
}

process <- function(result, X, labs) {
  labels <- rep(0, NROW(X))
  if (!is.list(result)) {
    result <- list(outliers = result)
  } else if (inherits(result$outliers, "data.frame")) {
    result$outliers <- result$outliers[, 1]
  }
  labels[result$outliers] <- 1
  diff_metrics(labs, labels)
}

process_roc <- function(result, X, labs) {
  roc_obj <- suppressMessages(pROC::roc(labs, result, direction = "<"))
  list(auc = roc_obj$auc, scores = result)
}

process_lookout <- function(result, X, labs) {
  metrics <- process(result, X, labs)
  roc_obj <- process_roc(result$outlier_scores, X, labs)
  c(metrics, list(auc = roc_obj$auc, scores = result$scores))
}

# Save timing data
save_time <- function(experiment, results) {
  results <- results[grepl("_time", names(results))]
  algorithms <- gsub("_time", "", names(results))
  purrr::map2_dfr(results, algorithms, function(x, name) {
    tibble(Algorithm = name, Run = seq(NROW(x))) |>
      bind_cols(x)
  }) |>
    write.csv(
      here::here(
        paste0("Data_Output/Time_Taken_For_Experiment_", experiment, ".csv")
      ),
      row.names = FALSE
    )
}

create_figure_exp567 <- function(experiment, p, results) {
  set_ggplot_options()
  # Plot metrics
  scale <- pretty(results$Iteration)
  scale <- scale[scale == round(scale) & scale > 0]
  g4 <- results |>
    mutate(
      Algorithm = factor(
        Algorithm,
        levels = c(
          "lookout_new",
          "lookout_old",
          "hdoutliers",
          "stray",
          "kdeos",
          "rdos"
        ),
        labels = c(
          "New Lookout",
          "Old Lookout",
          "HDoutliers",
          "Stray",
          "KDEOS",
          "RDOS"
        )
      ),
      Metric = recode(
        Metric,
        "fmeasure" = "Fmeasure",
        "gmean" = "Gmean",
        "auc" = "AUC"
      ),
      lower = pmax(0, mean - 2 * se),
      upper = pmin(1, mean + 2 * se)
    ) |>
    ggplot(aes(x = Iteration)) +
    geom_ribbon(
      aes(ymin = lower, ymax = upper, fill = Algorithm),
      alpha = 0.4
    ) +
    geom_line(aes(y = mean, color = Algorithm), linewidth = 1) +
    ylab("Performance") +
    facet_wrap(~Metric) +
    theme(legend.position = "bottom") +
    scale_x_continuous(
      breaks = scale,
      limits = c(1, max(results$Iteration))
    )

  # Create figure
  dir.create("Figures", showWarnings = FALSE)
  fig <- here::here(paste0("Figures/Exp", experiment, "_Data_and_Results.pdf"))
  cairo_pdf(file = fig, width = 8, height = 4)
  print(p | g4)
  crop::dev.off.crop(fig)

  fig
}
