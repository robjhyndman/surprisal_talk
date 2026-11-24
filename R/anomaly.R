# What is an anomaly? Chi-square samples and spot-the-anomaly figures

# A strip of chi-square draws, with the largest in orange
chisq_strip <- function(data) {
  ggplot(data, aes(x = x, y = 0)) +
    geom_point(alpha = 0.45, size = 3, colour = slide_colours[["ink"]]) +
    geom_point(
      data = filter(data, x == max(x)),
      size = 3.5,
      colour = slide_colours[["orange"]]
    ) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(limits = c(0, 26), breaks = seq(0, 25, by = 5)) +
    labs(x = NULL, y = NULL)
}

make_spot_anomaly_data <- function() {
  set.seed(2)
  x <- runif(2500, -1, 1)
  y <- runif(2500, -1, 1)
  tibble(x = x, y = y) |>
    mutate(r = x^2 + y^2) |>
    filter(r < 1)
}

# Points of a circle, for marking an anomaly
circle_path <- function(x, y, r, n = 200) {
  theta <- seq(0, 2 * pi, length.out = n)
  tibble(x = x + r * cos(theta), y = y + r * sin(theta))
}

# Spot-the-anomaly scatterplots: (a) uniform on the disc plus a point outside
# it; (b) uniform on the disc with a hole round the point nearest the centre.
# `circle` adds a circle round the anomaly.
spot_anomaly_plot <- function(data, version = c("a", "b"), circle = FALSE) {
  version <- match.arg(version)
  if (version == "a") {
    data <- bind_rows(data, tibble(x = 0.75, y = -0.75))
    mark <- circle_path(0.75, -0.75, 0.09)
  } else {
    data <- filter(data, r > 0.025 | r == min(r))
    centre <- filter(data, r == min(r))
    mark <- circle_path(centre$x, centre$y, 0.12)
  }
  p <- ggplot(data, aes(x = x, y = y)) +
    geom_point(alpha = 0.5, size = 1.3, colour = slide_colours[["ink"]]) +
    coord_fixed(xlim = c(-1.05, 1.05), ylim = c(-1.05, 1.05), expand = FALSE) +
    labs(x = NULL, y = NULL)
  if (circle) {
    p <- p +
      geom_path(
        data = mark,
        colour = slide_colours[["orange"]],
        linewidth = 1.6
      )
  }
  p
}
