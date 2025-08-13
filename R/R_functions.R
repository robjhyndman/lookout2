# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

set_ggplot_options <- function() {
  colours <- c(
    "#D55E00",
    "#0072B2",
    "#009E73",
    "#CC79A7",
    "#E69F00",
    "#56B4E9",
    "#F0E442"
  )
  options(
    ggplot2.discrete.colour = colours,
    ggplot2.discrete.fill = colours
  )
  ggplot2::theme_set(
    ggplot2::theme_get() +
      ggplot2::theme(text = ggplot2::element_text(family = "Fira Sans"))
  )
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

# Set up a tibble to store difference metrics
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

# Convert lookout object to a data frame with outlier labels
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

# Apply dobin to data frame with labelled outliers
as_dobin <- function(object) {
  labels <- object$Points
  object$Points <- NULL
  dobout <- dobin::dobin(object)
  dobX <- dobout$coords
  colnames(dobX) <- paste0("D", seq(NCOL(dobX)))
  as_tibble(dobX) |>
    mutate(labels = labels)
}

get_legend <- function(plot) {
  tmp <- ggplot_gtable(ggplot_build(plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if (length(leg) > 0) {
    legend <- tmp$grobs[[leg]]
    return(wrap_elements(panel = legend))
  } else {
    return(plot_spacer())
  }
}


# Function to plot data on left, anomaly rates on right with legend in bottom left quadrant
experiment_plot <- function(g1, g2, filename, height = 5, width = 8, ...) {
  set_ggplot_options()
  legend1 <- get_legend(g1)
  legend2 <- get_legend(g2)
  combined_legends <- wrap_plots(legend1, legend2, ncol = 2)

  left <- ((g1 + theme(legend.position = "none")) / combined_legends) +
    plot_layout(heights = c(3, 1))
  right <- g2 + theme(legend.position = "none")

  # Create figure
  dir.create("Figures", showWarnings = FALSE)
  fig <- here::here("Figures", filename)
  cairo_pdf(file = fig, height = height, width = width, ...)
  print(patchwork::wrap_plots(left, right, ncol = 2, widths = c(1, 1)))
  crop::dev.off.crop(fig)
  return(fig)
}
