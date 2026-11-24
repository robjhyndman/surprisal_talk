# Wine quality and prices

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
    geom_point(alpha = 0.3, colour = slide_colours[["ink"]]) +
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
      fill = slide_colours[["orange"]],
      alpha = 0.25
    ) +
    # Outline the curves only, not the baselines
    geom_path(
      data = densities |> filter(curve),
      aes(group = group),
      colour = slide_colours[["orange"]],
      linewidth = 0.8
    ) +
    geom_line(data = line, colour = slide_colours[["blue"]], linewidth = 1)
}

# Wines with surprisal probability below `alpha` in orange, with the fitted line
wine_anomaly_plot <- function(wine, log = TRUE, alpha = 0.001) {
  wine <- wine |>
    mutate(anomaly = p < alpha) |>
    arrange(anomaly)
  p <- wine |>
    ggplot(aes(x = points, y = price)) +
    geom_point(aes(colour = anomaly, alpha = anomaly)) +
    geom_line(aes(y = fitted), colour = slide_colours[["blue"]], linewidth = 1) +
    scale_colour_manual(
      values = c(`FALSE` = slide_colours[["ink"]], `TRUE` = slide_colours[["orange"]]),
      guide = "none"
    ) +
    scale_alpha_manual(values = c(`FALSE` = 0.25, `TRUE` = 0.9), guide = "none") +
    labs(x = "Points", y = "Price (US$)", title = wine_title(wine))
  if (log) {
    p <- p + scale_y_log10()
  }
  p
}
