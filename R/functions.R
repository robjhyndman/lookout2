# Test algorithm on gamma distribution data
gamma_outliers <- function(
  n1,
  n2,
  shape1,
  shape2,
  rate1,
  rate2,
  bw,
  version,
  ...
) {
  results <- tibble(
    outrate = rep(0, length(rate2)),
    outliers = 0,
    N = 0,
    true_pos = 0,
    true_neg = 0,
    false_pos = 0,
    false_neg = 0,
    specificity = 0
  )
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
      unitize = FALSE,
      bw_para = bw,
      version = version
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
  version = 2
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
  lo <- lookout(X, bw_para = bw, version = version)
  act <- c(rep(0, n1), rep(1, n2))
  preds <- rep(0, n1 + n2)
  preds[lo$outliers[, 1]] <- 1

  return(diff_metrics(act, preds))
}

# Function to calculate difference metrics
diff_metrics <- function(act, pred) {
  # positives to be denoted by 1 and negatives with 0
  n <- length(act)
  tp <- sum((act == 1) & (pred == 1))
  tn <- sum((act == 0) & (pred == 0))
  fp <- sum((act == 0) & (pred == 1))
  fn <- sum((act == 1) & (pred == 0))

  sp <- tn / (tn + fp)

  out <- data.frame(
    N = n,
    true_pos = tp,
    true_neg = tn,
    false_pos = fp,
    false_neg = fn,
    specificity = sp
  )

  return(out)
}
