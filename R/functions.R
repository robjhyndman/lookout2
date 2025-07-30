# Common settings and functions for scripts

set.seed(2025)

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
# Fira Sans font for graphics
ggplot2::theme_set(
  ggplot2::theme_get() +
    ggplot2::theme(text = ggplot2::element_text(family = "Fira Sans"))
)

# Test algorithm on gamma distribution data
gamma_outliers <- function(
  n1,
  n2,
  shape1,
  shape2,
  rate1,
  rate2,
  bw,
  old_version,
  ...
) {
  results <- tibble(
    outrate = rep(0, length(rate2)),
    outliers = 0
  )
  results <- results |>
    bind_cols(set_up_diff_metrics(length(rate2)))
  for (i in seq_along(rate2)) {
    # Generate random data
    X <- matrix(rgamma(n = 2 * n1, shape = shape1, rate = rate1, ...), ncol = 2)
    X2 <- matrix(
      rgamma(n = 2 * n2, shape = shape2, rate = rate2[i], ...),
      ncol = 2
    )
    X <- rbind(X, X2)

    lookobj <- lookout::lookout(
      X,
      scale = FALSE,
      gamma = bw,
      old_version = old_version
    )
    act <- c(rep(0, n1), rep(1, n2))
    preds <- rep(0, n1 + n2)
    preds[lookobj$outliers[, 1]] <- 1
    results[i, ] <- c(rate2[i], n2, diff_metrics(act, preds))
  }
  return(results)
}

# Test algorithm on normally distributed data
exp_normal <- function(
  n1 = 500,
  n2 = 5,
  mm = 10,
  bw = 0.95,
  old_version = FALSE
) {
  X <- rbind(
    data.frame(
      x = rnorm(n1),
      y = rnorm(n1)
    ),
    data.frame(
      x = rnorm(n2, mean = mm, sd = 0.2),
      y = rnorm(n2, mean = mm, sd = 0.2)
    )
  )
  lo <- lookout(X, gamma = bw, old_version = old_version)
  act <- c(rep(0, n1), rep(1, n2))
  preds <- rep(0, n1 + n2)
  preds[lo$outliers[, 1]] <- 1

  return(diff_metrics(act, preds))
}

# Function to calculate difference metrics
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

  tpr <- tp / (tp + fn)
  tnr <- tn / (tn + fp)
  fpr <- fp / (fp + tn)
  fnr <- fn / (fn + tp)

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

  if (tp == 0) {
    tpr <- 0
  }
  if (fn == 0) {
    fnr <- 0
  }
  if (tn == 0) {
    tnr <- 0
  }
  if (fp == 0) {
    fpr <- 0
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
    false_negative_rate = fnr,
    accuracy = prec,
    sensitivity = sn,
    specificity = sp,
    gmean = sqrt(sn * sp),
    precision = precision,
    recall = recall,
    fmeasure = fmeasure
  )

  return(out)
}

set_up_diff_metrics <- function(nrows) {
  # Create a tibble to store the results
  df <- diff_metrics(0, 0)
  as_tibble(df[rep(1, nrows), ])
}
