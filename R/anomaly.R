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

# Centre and radius of the mark around the anomaly in each spot-the-anomaly
# plot, and the data with that anomaly: (a) uniform on the disc plus a point
# outside it; (b) uniform on the disc with a hole round the point nearest the
# centre
spot_anomaly <- function(data, version = c("a", "b")) {
  version <- match.arg(version)
  if (version == "a") {
    data <- bind_rows(data, tibble(x = 0.75, y = -0.75))
    mark <- list(x = 0.75, y = -0.75, r = 0.09)
  } else {
    data <- filter(data, r > 0.025 | r == min(r))
    centre <- filter(data, r == min(r))
    mark <- list(x = centre$x, y = centre$y, r = 0.12)
  }
  list(data = data, mark = mark)
}

spot_anomaly_plot <- function(data, version = c("a", "b")) {
  ggplot(spot_anomaly(data, version)$data, aes(x = x, y = y)) +
    geom_point(alpha = 0.5, size = 1.3, colour = slide_colours[["ink"]]) +
    coord_fixed(xlim = c(-1.05, 1.05), ylim = c(-1.05, 1.05), expand = FALSE) +
    labs(x = NULL, y = NULL)
}

# The square round the anomaly in spot_anomaly_plot() saved as a `size`-inch
# square PNG, as fractions of the image (left, top, width), for placing a
# hand-drawn circle over it on the slide
spot_anomaly_box <- function(data, version = c("a", "b"), size = 4.6) {
  mark <- spot_anomaly(data, version)$mark
  file <- tempfile(fileext = ".png")
  on.exit(unlink(file))
  with_slide_style({
    ragg::agg_png(file, width = size, height = size, units = "in", res = 100)
    on.exit(grDevices::dev.off(), add = TRUE, after = FALSE)
    grid::grid.newpage()
    grid::grid.draw(ggplotGrob(spot_anomaly_plot(data, version)))
    grid::grid.force() # keep the gtable's viewports, to find the panel's
    panel <- grep("^panel", grid::grid.ls(grobs = FALSE, viewports = TRUE, print = FALSE)$name,
      value = TRUE)[1]
    grid::seekViewport(panel)
    # The panel spans [-1.05, 1.05] in both directions
    npc <- function(v) grid::unit((v + 1.05) / 2.1, "npc")
    loc <- grid::deviceLoc(
      npc(c(mark$x - mark$r, mark$x + mark$r)),
      npc(c(mark$y + mark$r, mark$y - mark$r))
    )
  })
  x <- grid::convertX(loc$x, "in", valueOnly = TRUE) / size
  y <- 1 - grid::convertY(loc$y, "in", valueOnly = TRUE) / size
  c(left = x[1], top = y[1], width = x[2] - x[1])
}
