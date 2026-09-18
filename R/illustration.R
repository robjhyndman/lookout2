# Illustration (Section 2): a small two-dimensional sample with its minimum
# spanning tree, the single linkage dendrogram cut at h_n, and the kernel
# density estimate with the detected anomalies.

generate_illustration <- function(alpha, beta, gamma) {
  n <- 300
  X <- rbind(
    matrix(rnorm(2 * n), n, 2),
    matrix(c(4.0, 3.5, -3.6, 3.0, 0.8, -4.3), ncol = 2, byrow = TRUE)
  )
  colnames(X) <- c("Y1", "Y2")
  emst <- mlpack::emst(X)
  edges <- tibble(
    from = emst[, 1] + 1,
    to = emst[, 2] + 1,
    length = emst[, 3]
  )
  h <- lookout::find_tda_bw(X, gamma = gamma)
  fit <- lookout::lookout(
    X,
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    scale = FALSE,
    fast = FALSE
  )
  # Kernel density estimate on a grid, Epanechnikov kernel with support radius
  # sqrt(m + 4) h, as in lookout:::lookde
  m <- 2
  r <- sqrt(m + 4) * h
  k0 <- (m + 2) / (2 * pi * r^m)
  g <- seq(-5, 5, length.out = 121)
  grid <- expand.grid(Y1 = g, Y2 = g)
  d2 <- outer(grid$Y1, X[, 1], "-")^2 + outer(grid$Y2, X[, 2], "-")^2
  grid$density <- k0 / NROW(X) * rowSums(pmax(1 - d2 / r^2, 0))
  list(
    X = as_tibble(X) |> mutate(anomaly = which_outliers(fit)),
    edges = edges,
    h = h,
    hc = hclust(dist(X), method = "single"),
    grid = grid
  )
}

# Segments of a dendrogram, computed from the merge table of hclust
dendrogram_segments <- function(hc) {
  n <- length(hc$order)
  xpos <- numeric(n)
  xpos[hc$order] <- seq_len(n)
  cx <- numeric(n - 1)
  ch <- numeric(n - 1)
  segs <- vector("list", n - 1)
  for (i in seq_len(n - 1)) {
    a <- hc$merge[i, 1]
    b <- hc$merge[i, 2]
    xa <- if (a < 0) xpos[-a] else cx[a]
    xb <- if (b < 0) xpos[-b] else cx[b]
    ha <- if (a < 0) 0 else ch[a]
    hb <- if (b < 0) 0 else ch[b]
    hi <- hc$height[i]
    cx[i] <- (xa + xb) / 2
    ch[i] <- hi
    segs[[i]] <- tibble(
      x = c(xa, xb, xa),
      xend = c(xa, xb, xb),
      y = c(ha, hb, hi),
      yend = c(hi, hi, hi)
    )
  }
  bind_rows(segs)
}

create_figure_illustration <- function(obj) {
  set_ggplot_options()
  X <- obj$X
  edges <- obj$edges |>
    mutate(
      x = X$Y1[from], y = X$Y2[from],
      xend = X$Y1[to], yend = X$Y2[to],
      long = length > obj$h
    )
  p1 <- ggplot() +
    geom_segment(
      data = edges,
      aes(x = x, y = y, xend = xend, yend = yend, colour = long),
      linewidth = 0.4
    ) +
    geom_point(data = X, aes(Y1, Y2), size = 0.8) +
    scale_colour_manual(
      values = c(`FALSE` = "#999999", `TRUE` = "#D55E00"),
      labels = c("Length at most h", "Length above h"),
      name = NULL
    ) +
    coord_fixed() +
    labs(x = "Y1", y = "Y2", title = "Minimum spanning tree") +
    theme(legend.position = "bottom")
  segs <- dendrogram_segments(obj$hc)
  p2 <- ggplot(segs) +
    geom_segment(aes(x = x, xend = xend, y = y, yend = yend), linewidth = 0.3) +
    geom_hline(yintercept = obj$h, colour = "#D55E00", linetype = "dashed") +
    labs(x = NULL, y = "Merge height", title = "Single linkage dendrogram") +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  p3 <- ggplot() +
    geom_contour(
      data = obj$grid,
      aes(Y1, Y2, z = density),
      colour = "#0072B2",
      bins = 8,
      linewidth = 0.3
    ) +
    geom_point(data = X, aes(Y1, Y2, colour = anomaly), size = 0.8) +
    scale_colour_manual(
      values = c(`FALSE` = "#999999", `TRUE` = "red"),
      labels = c("Not anomalous", "Anomalous"),
      name = NULL
    ) +
    coord_fixed() +
    labs(title = "Kernel density estimate") +
    theme(legend.position = "bottom")
  dir.create("Figures", showWarnings = FALSE)
  fig <- here::here("Figures/mst_illustration.pdf")
  cairo_pdf(file = fig, width = 10, height = 4)
  print(patchwork::wrap_plots(p1, p2, p3, ncol = 3))
  crop::dev.off.crop(fig)
  fig
}
