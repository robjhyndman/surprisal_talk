kde_kernels <- function(data, h) {
  h <- rep(h, NCOL(data))
  H <- diag(h^2)
  colnames(data) <- c("xx", "yy")
  data$row <- seq_len(NROW(data))
  k <- tidyr::expand_grid(
    x = seq(
      min(data[, "xx"]) - 3 * h[1],
      max(data[, "xx"]) + 3 * h[1],
      l = 100
    ),
    y = seq(min(data[, "yy"]) - 3 * h[2], max(data[, "yy"]) + 3 * h[2], l = 100)
  ) |>
    mutate(z = mvtnorm::dmvnorm(x = cbind(x, y), sigma = H))
  df <- data |>
    mutate(k = list(k)) |>
    tidyr::unnest(k) |>
    mutate(x = x + xx, y = y + yy)
  ggplot() +
    geom_contour(
      data = df,
      aes(x = x, y = y, group = row, z = z),
      bins = 4,
      col = "gray"
    ) +
    geom_point(data = data, mapping = aes(x = xx, y = yy))
}

make_kde_plot <- function(data, h, i) {
  p1 <- kde_kernels(data, h) +
    lims(x = c(-0.86, 0.78), y = c(-0.95, 0.89))
  p2 <- data |>
    dist_kde(H = diag(rep(h^2, NCOL(data)))) |>
    gg_density(show_points = TRUE) +
    labs(x = "x", y = "y") +
    lims(x = c(-0.86, 0.78), y = c(-0.95, 0.89))
  # combine the plots
  p <- patchwork::wrap_plots(p1, p2, nrow = 1)
  file <- paste0("figs/kde_", i, ".pdf")
  Cairo::CairoPDF(file = file, width = 8, height = 4)
  print(p)
  crop::dev.off.crop(file)
  return(file)
}
