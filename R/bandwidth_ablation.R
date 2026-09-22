# Bandwidth sensitivity (Section 4): Experiments 1, 2, 3 and S1 rerun with the
# minimum spanning tree bandwidth h_n scaled by 1/4, 1/2, 1, 2 and 4, and with
# the bandwidth h_opt of Table 1 (the AMISE-optimal bandwidth for the
# Epanechnikov kernel and a standard normal density), all on the
# MCD-standardised data and with the exact leave-one-out estimate.

h_amise_normal <- function(n, m) {
  b_m <- pi^(m / 2) / base::gamma(m / 2 + 1)
  (8 * (4 * pi)^(m / 2) / (b_m * (m + 4)^(1 + m / 2)))^(1 / (m + 4)) *
    n^(-1 / (m + 4))
}

ablation_settings <- c("h/4", "h/2", "h", "2h", "4h", "h_opt")

ablation_one <- function(X, labs, alpha, beta, gamma) {
  Z <- lookout::mvscale(as.matrix(X))
  n <- NROW(Z)
  m <- NCOL(Z)
  h <- lookout::find_tda_bw(Z, gamma = gamma)
  bws <- c(h * c(1 / 4, 1 / 2, 1, 2, 4), h_amise_normal(n, m)) * sqrt(m + 4)
  purrr::map2_dfr(ablation_settings, bws, function(setting, bw) {
    # lookout() stops when the GPD cannot be fitted, which happens when more
    # than a fraction 1 - beta of the observations have no other observation
    # inside the kernel support, so that their surprisals tie at the maximum.
    # Record such runs as failures rather than stopping the experiment.
    fit <- tryCatch(
      lookout::lookout(
        Z,
        alpha = alpha,
        beta = beta,
        gamma = gamma,
        bw = bw,
        scale = FALSE,
        fast = FALSE
      ),
      error = function(e) NULL
    )
    if (is.null(fit)) {
      return(tibble(
        setting = setting,
        h = bw / sqrt(m + 4),
        auc = NA_real_,
        tpr = NA_real_,
        fpr = NA_real_,
        failed = TRUE
      ))
    }
    metrics <- diff_metrics(labs, which_outliers(fit))
    roc_obj <- suppressMessages(pROC::roc(labs, fit$outlier_scores, direction = "<"))
    tibble(
      setting = setting,
      h = bw / sqrt(m + 4),
      auc = as.numeric(roc_obj$auc),
      tpr = metrics$true_positive_rate,
      fpr = metrics$false_positive_rate,
      failed = FALSE
    )
  })
}

run_bandwidth_ablation <- function(alpha, beta, gamma, reps = 10) {
  run <- function(experiment, xvals, generate) {
    purrr::map_dfr(seq_len(reps), function(rep) {
      purrr::map_dfr(xvals, function(x) {
        X <- generate(x)
        labs <- X$Points == "Anomaly"
        X$Points <- NULL
        ablation_one(X, labs, alpha, beta, gamma) |>
          mutate(experiment = experiment, x = x, rep = rep)
      })
    })
  }
  # The order of the four runs fixes the random number stream; keep it.
  bind_rows(
    run("Experiment 1", seq(10) / 10, function(r) generate_exp1(500, 10, r)),
    run("Experiment S1", seq(2.5, 4, by = 0.25), function(mm) generate_expS1(1000, 10, mm)),
    run("Experiment 2", c(1000, 2000, 4000), generate_exp2),
    run("Experiment 3", seq(10), generate_exp3)
  )
}

create_figure_ablation <- function(results) {
  set_ggplot_options()
  df <- results |>
    mutate(setting = factor(setting, levels = ablation_settings)) |>
    group_by(experiment, x, setting) |>
    summarise(
      AUC = mean(auc, na.rm = TRUE),
      `True positive rate` = mean(tpr, na.rm = TRUE),
      `False positive rate` = mean(fpr, na.rm = TRUE),
      .groups = "drop"
    ) |>
    pivot_longer(
      c(AUC, `True positive rate`, `False positive rate`),
      names_to = "metric",
      values_to = "value"
    ) |>
    mutate(
      metric = factor(
        metric,
        levels = c("AUC", "True positive rate", "False positive rate")
      )
    )
  xlabs <- c(
    "Experiment 1" = "r",
    "Experiment 2" = "Number of points (n)",
    "Experiment 3" = "Iteration",
    "Experiment S1" = "Mean (µ)"
  )
  colours <- c(scales::viridis_pal(end = 0.85)(5), "black")
  plots <- purrr::map(names(xlabs), function(ex) {
    df |>
      filter(experiment == ex) |>
      ggplot(aes(x = x, y = value, colour = setting, linetype = setting)) +
      geom_line() +
      geom_point(size = 0.8) +
      facet_wrap(~metric, scales = "free_y", nrow = 1) +
      scale_colour_manual(values = colours, name = "Bandwidth") +
      scale_linetype_manual(
        values = c(rep("solid", 5), "dashed"),
        name = "Bandwidth"
      ) +
      labs(x = xlabs[[ex]], y = NULL, title = ex)
  })
  dir.create("Figures", showWarnings = FALSE)
  fig <- here::here("Figures/bandwidth_ablation.pdf")
  cairo_pdf(file = fig, width = 8, height = 10)
  print(
    patchwork::wrap_plots(plots, ncol = 1) +
      patchwork::plot_layout(guides = "collect") &
      theme(legend.position = "bottom")
  )
  crop::dev.off.crop(fig)
  fig
}
