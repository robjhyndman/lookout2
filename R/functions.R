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
Expnormal <- function(
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

# Take lookout object and return logical vector of which points are outliers
which_outliers <- function(object) {
  preds <- rep(FALSE, length(object$outlier_probability))
  preds[object$outliers[, 1]] <- TRUE
  preds
}

# Generate experiment 1 data

generate_exp1 <- function(n1, n2, rate) {
  # Generate random data
  out <- rbind(
    matrix(rgamma(n = 2 * n1, shape = 2, rate = 2), ncol = 2),
    matrix(rgamma(n = 2 * n2, shape = 2, rate = rate), ncol = 2)
  ) |>
    as.data.frame()
  colnames(out) <- c("X1", "X2")
  out$Points = c(rep("Non-anomaly", n1), rep("Anomaly", n2))
  as_tibble(out)
}

generate_exp2 <- function(n1, n2, mm) {
  out <- rbind(
    data.frame(x = rnorm(n1), y = rnorm(n1)),
    data.frame(
      x = rnorm(n2, mean = mm, sd = 0.2),
      y = rnorm(n2, mean = mm, sd = 0.2)
    )
  )
  colnames(out) <- c("X1", "X2")
  out$Points <- c(rep("Non-anomaly", n1), rep("Anomaly", n2))
  as_tibble(out)
}

generate_exp3 <- function(nn) {
  num_outliers <- ceiling(5 / 1000 * nn)
  meanx <- runif(num_outliers, min = -sqrt(2) * 2.2, max = sqrt(2) * 2.2)
  meany <- sqrt(2 * 2.2^2 - meanx^2)
  inds <- sample(seq(num_outliers), ceiling(num_outliers / 2))
  meany[inds] <- -1 * meany[inds]

  out <- bind_rows(
    data.frame(
      x = rnorm(nn),
      y = rnorm(nn)
    ),
    data.frame(
      x = rnorm(num_outliers, mean = meanx, sd = 0.1), # meanx
      y = rnorm(num_outliers, mean = meany, sd = 0.1) # meany
    )
  )
  colnames(out) <- c("X1", "X2")
  out$Points <- c(rep("Non-anomaly", nn), rep("Anomaly", num_outliers))
  as_tibble(out)
}

generate_exp4 <- function(nn) {
  num_outliers <- ceiling(5 / 1000 * nn)
  X1 <- matrix(rgamma(n = 2 * nn, shape = 2, rate = 2), ncol = 2)
  X2 <- matrix(rgamma(n = 2 * nn, shape = 2.2, rate = 2), ncol = 2)
  x1dist <- apply(X1, 1, function(x) sqrt(x[1]^2 + x[2]^2))
  qq <- quantile(x1dist, probs = 0.99)
  x2dist <- apply(X2, 1, function(x) sqrt(x[1]^2 + x[2]^2))
  inds <- which(x2dist > qq)
  inds2 <- sample(inds, num_outliers)
  out <- as.data.frame(rbind(X1, X2[inds2, ]))
  colnames(out) <- c("X1", "X2")
  out$Points = c(
    rep("Non-anomaly", NROW(X1)),
    rep("Anomaly", length(inds2))
  )
  as_tibble(out)
}

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

generate_exp6 <- function(iterate) {
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
  X[nn - 5 + seq(5), 1] <- rnorm(5, 5 - (iterate - 1) * 0.5, sd = 0.1)
  X[nn - 5 + seq(5), 2] <- rnorm(5, 0, sd = 0.1)
  X$Points <- c(rep("Non-anomaly", nn - 5), rep("Anomaly", 5))
  X
}

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

as.data.frame.lookoutliers <- function(object, ...) {
  varnames <- colnames(object$data)
  if (is.null(varnames)) {
    varnames <- paste0("V", seq(NCOL(object$data)))
  }
  X <- as.data.frame(object$data)
  colnames(X) <- varnames
  X$outliers <- which_outliers(object)
  return(X)
}

as_dobin <- function(object) {
  labels <- object$Points
  object$Points <- NULL
  dobout <- dobin::dobin(object)
  dobX <- dobout$coords
  colnames(dobX) <- paste0("D", seq(NCOL(dobX)))
  as_tibble(dobX) |>
    mutate(labels = labels)
}
