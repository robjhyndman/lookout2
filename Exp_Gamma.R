gamma_outliers <- function(
  n1,
  n2,
  shape1,
  shape2,
  rate1,
  rate2,
  bw,
  shape_zero = TRUE,
  normalize = TRUE,
  ...
) {
  results <- tibble(
    outrate = 0,
    outliers = 0,
    N = 0,
    true_pos = 0,
    true_neg = 0,
    false_pos = 0,
    false_neg = 0,
    specificity = 0
  )
  for (i in 1:length(rate2)) {
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
      normalize = normalize,
      bw_para = bw,
      shape_zero = shape_zero
    )
    act <- c(rep(0, n1), rep(1, n2))
    preds <- rep(0, n1 + n2)
    preds[lookobj$outliers[, 1]] <- 1
    results[i, ] <- c(rate2[i], n2, diff_metrics(act, preds))
  }
  return(results)
}
