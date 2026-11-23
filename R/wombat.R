# Figures and computations for wombat_2026.qmd, drawn in the style of the
# uconn_2026.qmd slides (theme and colours applied by save_png())

wombat_colours <- c(
  ink = "#1e293b",
  faint = "#94a3b8",
  blue = "#1d6fa5",
  amber = "#F39B00",
  orange = "#c14b14"
)

# What is an anomaly? ------------------------------------------------------

# A strip of chi-square draws, with the largest in orange
chisq_strip <- function(data) {
  ggplot(data, aes(x = x, y = 0)) +
    geom_point(alpha = 0.45, size = 3, colour = wombat_colours[["ink"]]) +
    geom_point(
      data = filter(data, x == max(x)),
      size = 3.5,
      colour = wombat_colours[["orange"]]
    ) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(limits = c(0, 26), breaks = seq(0, 25, by = 5)) +
    labs(x = NULL, y = NULL)
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
    geom_point(alpha = 0.5, size = 1.3, colour = wombat_colours[["ink"]]) +
    coord_fixed(xlim = c(-1.05, 1.05), ylim = c(-1.05, 1.05), expand = FALSE) +
    labs(x = NULL, y = NULL)
  if (circle) {
    p <- p +
      geom_path(
        data = mark,
        colour = wombat_colours[["orange"]],
        linewidth = 1.6
      )
  }
  p
}

# Old Faithful eruptions -----------------------------------------------------

wombat_of_labels <- labs(
  x = "Duration (seconds)",
  y = "Waiting time (seconds)"
)

old_faithful_scatter <- function(oldfaithful) {
  oldfaithful |>
    ggplot(aes(x = duration, y = waiting)) +
    geom_point(alpha = 0.3, colour = wombat_colours[["ink"]]) +
    wombat_of_labels +
    labs(title = "Eruptions from 14 January 2017 to 29 December 2023")
}

# Kernel density estimate of the eruptions, with the points
old_faithful_kde <- function(oldfaithful) {
  oldfaithful |>
    select(duration, waiting) |>
    dist_kde() |>
    gg_density(show_points = TRUE, alpha = 0.15, colors = wombat_colours[["blue"]]) +
    wombat_of_labels
}

old_faithful_hdrboxplot <- function(oldfaithful) {
  oldfaithful |>
    gg_hdrboxplot(duration, waiting, color = wombat_colours[["blue"]]) +
    wombat_of_labels
}

# Surprisal probabilities of the eruptions, as on the slide
old_faithful_probs <- function(oldfaithful) {
  oldfaithful$p <- surprisals_prob(
    oldfaithful[, c("duration", "waiting")],
    loo = TRUE,
    approximation = "gpd"
  )
  oldfaithful |> arrange(p)
}

# Wine quality and prices ------------------------------------------------------

wine_title <- function(wine) {
  paste0(
    format(NROW(wine), big.mark = ","),
    " Shiraz/Syrah wines reviewed by Wine Enthusiast"
  )
}

# Price against points, on a log scale if `log`
wine_scatter <- function(wine, log = FALSE) {
  p <- wine |>
    ggplot(aes(x = points, y = price)) +
    geom_point(alpha = 0.3, colour = wombat_colours[["ink"]]) +
    labs(x = "Points", y = "Price (US$)", title = wine_title(wine))
  if (log) {
    p <- p + scale_y_log10()
  }
  p
}

# The log-scale scatter plot with the fitted regression line and, at each of
# `at` points, the normal density of log price drawn sideways (over ±2.5 sd,
# which stays within the range of the data so the axes match wine_scatter())
wine_model_scatter <- function(wine, at = c(82, 86, 90, 94, 98), width = 1.8) {
  wine_lm <- lm(log(price) ~ points, data = wine)
  sigma <- sigma(wine_lm)
  mu <- predict(wine_lm, newdata = tibble(points = at))
  # Each density closed along its baseline at `at`, so it can be filled
  z <- c(2.5, -2.5, seq(-2.5, 2.5, length.out = 101))
  densities <- tibble(
    group = rep(seq_along(at), each = length(z)),
    points = rep(at, each = length(z)) +
      width * rep(c(0, 0, exp(-z[-(1:2)]^2 / 2)), length(at)),
    price = exp(rep(mu, each = length(z)) + sigma * rep(z, length(at))),
    curve = rep(seq_along(z) > 2, length(at))
  )
  line <- tibble(points = range(wine$points)) |>
    mutate(price = exp(predict(wine_lm, newdata = pick(points))))
  wine_scatter(wine, log = TRUE) +
    geom_polygon(
      data = densities,
      aes(group = group),
      fill = wombat_colours[["orange"]],
      alpha = 0.25
    ) +
    # Outline the curves only, not the baselines
    geom_path(
      data = densities |> filter(curve),
      aes(group = group),
      colour = wombat_colours[["orange"]],
      linewidth = 0.8
    ) +
    geom_line(data = line, colour = wombat_colours[["blue"]], linewidth = 1)
}

# Wines with surprisal probability below `alpha` in orange, with the fitted line
wine_anomaly_plot <- function(wine, log = TRUE, alpha = 0.001) {
  wine <- wine |>
    mutate(anomaly = p < alpha) |>
    arrange(anomaly)
  p <- wine |>
    ggplot(aes(x = points, y = price)) +
    geom_point(aes(colour = anomaly, alpha = anomaly)) +
    geom_line(aes(y = fitted), colour = wombat_colours[["blue"]], linewidth = 1) +
    scale_colour_manual(
      values = c(`FALSE` = wombat_colours[["ink"]], `TRUE` = wombat_colours[["orange"]]),
      guide = "none"
    ) +
    scale_alpha_manual(values = c(`FALSE` = 0.25, `TRUE` = 0.9), guide = "none") +
    labs(x = "Points", y = "Price (US$)", title = wine_title(wine))
  if (log) {
    p <- p + scale_y_log10()
  }
  p
}

# Surprisal probabilities of the wines under the regression model, with the
# fitted prices for plotting (`p` as on the slide, from `approximation`)
wine_probs <- function(wine, approximation = "none") {
  wine_lm <- lm(log(price) ~ points, data = wine)
  wine |>
    mutate(
      p = surprisals_prob(wine_lm, loo = TRUE, approximation = approximation),
      fitted = exp(fitted(wine_lm))
    )
}

# N(0,1) example -------------------------------------------------------------

# The tibble after each step of the slide code: y generated, p added, sorted
normal_example_steps <- function(seed = 2026, n = 1000) {
  set.seed(seed)
  N01 <- dist_normal(mean = 0, sd = 1)
  generated <- tibble(y = generate(N01, times = n)[[1]])
  with_p <- generated |> mutate(p = surprisals_prob(y, distribution = N01))
  list(generated, with_p, arrange(with_p, p))
}

# Output of library(weird), as printed when the package is attached.
# `lockfile` is unused: it makes the target depend on the package library.
weird_attach_message <- function(lockfile = NULL) {
  callr::r(function() {
    options(cli.num_colors = 1, cli.width = 72, width = 72)
    msgs <- character()
    withCallingHandlers(
      library(weird),
      packageStartupMessage = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    sub("\n$", "", paste(msgs, collapse = ""))
  })
}
