# Wine quality and prices

# Price against points, on a log scale if `log`
wine_scatter <- function(wine, log = FALSE) {
  p <- wine |>
    ggplot(aes(x = points, y = price)) +
    geom_point(alpha = 0.3, colour = slide_colours[["ink"]]) +
    labs(x = "Points", y = "Price (US$)")
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
    labs(x = "Points", y = "Price (US$)")
  if (log) {
    p <- p + scale_y_log10()
  }
  p
}

# Interactive version of wine_anomaly_plot(): the anomalies can be hovered
# over, with the id from wine_anomaly_ids() as data_id for wine_anomaly_card()
wine_anomaly_girafe <- function(wine, log = TRUE, alpha = 0.001, width = 9, height = 4.4) {
  anomalies <- wine_anomaly_ids(wine, alpha)
  p <- wine_anomaly_plot(wine, log = log, alpha = alpha) +
    geom_point(data = anomalies, colour = slide_colours[["orange"]], size = 2.5) +
    # Invisible, larger targets, so the small points are easy to hover over.
    # The hovered point is marked by the line to its card (hover_card()), not
    # by ggiraph's hover style, which is turned off.
    ggiraph::geom_point_interactive(
      data = anomalies,
      aes(data_id = id),
      size = 7,
      alpha = 0
    )
  ggiraph::girafe(
    ggobj = p,
    font_set = girafe_fonts(),
    width_svg = width,
    height_svg = height,
    options = list(
      ggiraph::opts_hover(css = "cursor:pointer;"),
      ggiraph::opts_sizing(rescale = TRUE, width = 1),
      ggiraph::opts_toolbar(saveaspng = FALSE, hidden = c("selection", "zoom", "misc"))
    )
  )
}

# The anomalies, with an id naming the wines at each (points, price): "winery
# year", or several of them joined by "and" when wines share a point (e.g.
# "Tua Rita 2012 and Tua Rita 2013"), as one point can only be hovered over once
wine_anomaly_ids <- function(wine, alpha = 0.001) {
  wine |>
    filter(p < alpha) |>
    arrange(winery, year) |>
    mutate(
      n = n(),
      id = paste(paste(winery, year), collapse = " and "),
      .by = c(points, price)
    )
}

# Card for wine_anomaly_girafe() describing the hovered wine. Wines sharing a
# point share a card, with the first sentence of each review.
wine_anomaly_card <- function(wine, alpha = 0.001) {
  # Probability as "a × 10^b"; the slides' CSS reset neutralises <sup>
  sci <- function(x) {
    e <- floor(log10(x))
    sprintf(
      "%.1f &times; 10<span style='font-size:70%%; vertical-align:super'>&minus;%d</span>",
      x / 10^e, -e
    )
  }
  first_sentence <- function(x) sub("^(.*?[.!?])\\s.*$", "\\1", x, perl = TRUE)
  anomalies <- wine_anomaly_ids(wine, alpha) |>
    mutate(
      review = if_else(
        n > 1,
        paste0("<strong>", winery, " ", year, ":</strong> ", htmltools::htmlEscape(first_sentence(description))),
        htmltools::htmlEscape(description)
      )
    ) |>
    summarise(
      label = paste0(
        paste(unique(paste0(state, ", ", country)), collapse = "; "), "<br>",
        first(points), " points, US$", first(price), "<br>",
        "expected US$", round(first(fitted)), ", <span style='font-style:italic'>p</span> = ", sci(first(p)),
        paste0("<div class='wine-review'>", review, "</div>", collapse = "")
      ),
      .by = id
    )
  hover_card(
    stats::setNames(anomalies$label, anomalies$id),
    sep = "<br>",
    class = "wine-card",
    connect = TRUE
  )
}
