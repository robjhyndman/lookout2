library(targets)

# Set target options
tar_option_set(
  packages = c(
    "crop",
    "DDoutlier",
    "dobin",
    "dplyr",
    "ggplot2",
    "HDoutliers",
    "here",
    "lookout",
    "patchwork",
    "pROC",
    "readr",
    "stray",
    "tidyr",
    "weird"
  ),
  seed = 2025
)

# Read all functions
tar_source()

# List of targets
list(
  # Should scaling be used?
  tar_target(scale, TRUE),
  # Value of alpha
  tar_target(alpha, 0.01),
  # Value of gamma
  tar_target(beta, 0.90),
  # Value of gamma
  tar_target(gamma, 0.95),

  # ------------------------------------------------------
  # Experiment 1: Gamma distribution
  # ------------------------------------------------------

  tar_target(
    exp1_results,
    run_synthetic_exp1(scale, alpha, beta, gamma)
  ),
  tar_target(
    fig_exp1,
    create_figure_exp1(exp1_results)
  ),

  # ------------------------------------------------------
  # Experiment 2: Normal distribution
  # ------------------------------------------------------

  tar_target(
    exp2_results,
    run_synthetic_exp2(scale, alpha, beta, gamma)
  ),
  tar_target(
    fig_exp2,
    create_figure_exp2(exp2_results)
  ),

  # ------------------------------------------------------
  # Experiment 3: Increasing N with normal distribution
  # ------------------------------------------------------

  tar_target(
    exp3_results,
    run_synthetic_exp3(scale, alpha, beta, gamma)
  ),
  tar_target(
    fig_exp3,
    create_figure_exp3(exp3_results)
  ),

  # ------------------------------------------------------
  # Experiment 4: Increasing N with gamma distribution
  # ------------------------------------------------------

  tar_target(
    exp4_results,
    run_synthetic_exp4(scale, alpha, beta, gamma)
  ),
  tar_target(
    fig_exp4,
    create_figure_exp4(exp4_results)
  ),
  # ------------------------------------------------------
  # Experiment 5: Comparison with other methods
  # ------------------------------------------------------

  tar_target(
    exp5_results,
    run_synthetic_exp5(reps = 10, pp = 10, scale, alpha, beta, gamma)
  ),
  tar_target(
    fig_exp5,
    create_figure_exp5(exp5_results)
  ),

  # ------------------------------------------------------
  # Experiment 6: Comparison with other methods (different setup)
  # ------------------------------------------------------

  tar_target(
    exp6_results,
    run_synthetic_exp6(reps = 10, pp = 10, scale, alpha, beta, gamma)
  ),
  tar_target(
    fig_exp6,
    create_figure_exp6(exp6_results)
  ),

  # ------------------------------------------------------
  # Experiment 7: High-dimensional comparison
  # ------------------------------------------------------

  tar_target(
    exp7_results,
    run_synthetic_exp7(reps = 20, pp = 10, scale, alpha, beta, gamma)
  ),
  tar_target(
    fig_exp7,
    create_figure_exp7(exp7_results)
  ),

  # ------------------------------------------------------
  # REAL WORLD DATA ANALYSIS
  # ------------------------------------------------------

  tar_target(
    old_faithful_results,
    analyze_old_faithful(scale, alpha, beta, gamma)
  ),
  tar_target(
    fig_old_faithful,
    create_old_faithful_figure(old_faithful_results)
  ),
  tar_target(
    wine_results,
    analyze_wine_data(scale, alpha, beta, gamma)
  ),
  tar_target(
    fig_wine,
    create_wine_figure(wine_results)
  ),

  # ------------------------------------------------------
  # SHOWCASE EXAMPLES
  # ------------------------------------------------------

  tar_target(
    showcase_results,
    generate_showcase_examples(scale, alpha, beta, gamma)
  ),
  tar_target(
    fig_showcase,
    create_showcase_figure(showcase_results)
  )
)
