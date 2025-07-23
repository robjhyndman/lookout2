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

everysecond <- function(x) {
  x <- sort(unique(x))
  x[seq(2, length(x), 2)] <- ""
  x
}

exp_normal <- function(
  n1 = 500,
  n2 = 5,
  mm = 10,
  bw = 0.95,
  shape_zero = TRUE,
  transform = TRUE
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
  lo <- lookout(X, normalize = transform, bw_para = bw, shape_zero = shape_zero)
  act <- c(rep(0, n1), rep(1, n2))
  preds <- rep(0, n1 + n2)
  preds[lo$outliers[, 1]] <- 1

  return(diff_metrics(act, preds))
}

loop_normal <- function(st_mm = 10, en_mm = 3, step = 0.5, n1 = 500, n2 = 5) {
  set.seed(1)
  mm <- seq(st_mm, en_mm, by = -step)
  mm <- rep(mm, each = 50)
  out1 <- mapply(
    exp_normal,
    mm = mm,
    MoreArgs = list(n1 = n1, n2 = n2, bw = 0.95)
  )
  out2 <- mapply(exp_normal, mm = mm, MoreArgs = list(n1 = n1, n2 = n2, bw = 1))
  out1 <- matrix(unlist(out1), byrow = TRUE, ncol = 6)
  out2 <- matrix(unlist(out2), byrow = TRUE, ncol = 6)
  colnames(out1) <- colnames(out2) <- c(
    "N",
    "true_pos",
    "true_neg",
    "false_pos",
    "false_neg",
    "specificity"
  )
  # make out to a dataframe
  out1 <- cbind.data.frame(out_mean = mm, bw_para = 0.95, out1)
  out2 <- cbind.data.frame(out_mean = mm, bw_para = 1, out2)
  out <- rbind.data.frame(out1, out2)
  return(out)
}

plot_normal <- function(df) {
  df2 <- df |>
    select(-true_neg, -N) |>
    pivot_longer(cols = 3:(NCOL(df) - 2))
  ggplot(df2, aes(as.factor(out_mean), value)) +
    geom_boxplot() +
    geom_point() +
    facet_wrap(name ~ bw_para, scales = "free", nrow = 2) +
    xlab("Mean of outlier distribution") +
    scale_x_discrete(labels = everysecond(df2$out_mean))
}

exp_gamma <- function(n1 = 500, n2 = 5, rr = 0.5, bw = 0.95, transform = TRUE) {
  X <- rbind(
    data.frame(
      x = rgamma(n1, shape = 2, rate = 2),
      y = rgamma(n1, shape = 2, rate = 2)
    ),
    data.frame(
      x = rgamma(n2, shape = 2, rate = rr),
      y = rgamma(n2, shape = 2, rate = rr)
    )
  )
  # if(transform){
  #   X <- transform_normal(X)
  # }
  lo <- lookout(X, bw_para = bw, normalize = transform)
  act <- c(rep(0, n1), rep(1, n2))
  preds <- rep(0, n1 + n2)
  preds[lo$outliers[, 1]] <- 1

  return(diff_metrics(act, preds))
}

loop_gamma <- function(
  st_rr = 0.1,
  en_rr = 1,
  step = 0.1,
  n1 = 500,
  n2 = 5,
  transform = TRUE
) {
  set.seed(1)
  rseq <- seq(st_rr, en_rr, by = step)
  rseq <- rep(rseq, each = 50)
  out1 <- mapply(
    exp_gamma,
    rr = rseq,
    MoreArgs = list(n1 = n1, n2 = n2, bw = 0.95, transform = transform)
  )
  out2 <- mapply(
    exp_gamma,
    rr = rseq,
    MoreArgs = list(n1 = n1, n2 = n2, bw = 1, transform = transform)
  )
  out1 <- matrix(unlist(out1), byrow = TRUE, ncol = 6)
  out2 <- matrix(unlist(out2), byrow = TRUE, ncol = 6)
  colnames(out1) <- colnames(out2) <- c(
    "N",
    "true_pos",
    "true_neg",
    "false_pos",
    "false_neg",
    "specificity"
  )
  # make out to a dataframe
  out1 <- cbind(out_rate = rseq, bw_para = 0.95, out1)
  out2 <- cbind(out_rate = rseq, bw_para = 1, out2)
  out <- rbind(out1, out2)
  return(out)
}

normal_outliers_2 <- function(
  n1,
  n2,
  n3,
  mean2,
  mean3,
  bw,
  shape_zero,
  transform = TRUE,
  unitize = TRUE
) {
  results <- tibble(
    outliers = 0,
    mean2 = 0,
    mean3 = 0,
    N = 0,
    true_pos = 0,
    true_neg = 0,
    false_pos = 0,
    false_neg = 0,
    specificity = 0
  )
  max_2_3 <- max(length(mean2), length(mean3))
  for (i in 1:max_2_3) {
    if (length(mean2) == 1) {
      m2 <- mean2
    } else {
      m2 <- mean2[i]
    }
    if (length(mean3) == 1) {
      m3 <- mean3
    } else {
      m3 <- mean3[i]
    }

    X <- rbind(
      data.frame(
        x = rnorm(n1),
        y = rnorm(n1)
      ),
      data.frame(
        x = rnorm(n2, mean = m2, sd = 0.2),
        y = rnorm(n2, mean = m2, sd = 0.2)
      ),
      data.frame(
        x = rnorm(n3, mean = m3, sd = 0.2),
        y = rnorm(n3, mean = m3, sd = 0.2)
      )
    )
    lo <- lookout::lookout(
      X,
      bw_para = bw,
      normalize = transform,
      shape_zero = shape_zero,
      unitize = unitize
    )
    act <- c(rep(0, n1), rep(1, n2), rep(1, n3))
    preds <- rep(0, n1 + n2 + n3)
    preds[lo$outliers[, 1]] <- 1
    numout <- n2 + n3
    results[i, ] <- c(numout, m2, m3, diff_metrics(act, preds))
  }

  return(results)
}

normal_outliers_3 <- function(
  n1,
  n2,
  n3,
  n4,
  mean2,
  mean3,
  mean4,
  bw,
  shape_zero,
  transform = TRUE,
  unitize = TRUE
) {
  results <- tibble(
    outliers = 0,
    mean2 = 0,
    mean3 = 0,
    mean4 = 0,
    N = 0,
    true_pos = 0,
    true_neg = 0,
    false_pos = 0,
    false_neg = 0,
    specificity = 0
  )
  max_2_3 <- max(length(mean2), length(mean3), length(mean4))
  for (i in 1:max_2_3) {
    if (length(mean2) == 1) {
      m2 <- mean2
    } else {
      m2 <- mean2[i]
    }
    if (length(mean3) == 1) {
      m3 <- mean3
    } else {
      m3 <- mean3[i]
    }
    if (length(mean4) == 1) {
      m4 <- mean4
    } else {
      m4 <- mean4[i]
    }

    X <- rbind(
      data.frame(
        x = rnorm(n1),
        y = rnorm(n1)
      ),
      data.frame(
        x = rnorm(n2, mean = m2, sd = 0.2),
        y = rnorm(n2, mean = m2, sd = 0.2)
      ),
      data.frame(
        x = rnorm(n3, mean = m3, sd = 0.2),
        y = rnorm(n3, mean = m3, sd = 0.2)
      ),
      data.frame(
        x = rnorm(n4, mean = m4, sd = 0.2),
        y = rnorm(n4, mean = m4, sd = 0.2)
      )
    )
    lo <- lookout::lookout(
      X,
      bw_para = bw,
      normalize = transform,
      shape_zero = shape_zero,
      unitize = unitize
    )
    act <- c(rep(0, n1), rep(1, n2), rep(1, n3), rep(1, n4))
    preds <- rep(0, n1 + n2 + n3 + n4)
    preds[lo$outliers[, 1]] <- 1
    numout <- n2 + n3 + n4
    results[i, ] <- c(numout, m2, m3, m4, diff_metrics(act, preds))
  }

  return(results)
}
