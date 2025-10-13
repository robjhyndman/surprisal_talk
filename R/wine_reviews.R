analyse_wine_data <- function(wine_reviews, scale, alpha, beta, gamma) {
  wine_reviews2 <- wine_reviews |>
    filter(variety %in% c("Shiraz", "Syrah")) |>
    select(points, price)

  lookobjNew <- lookout::lookout(
    wine_reviews2,
    scale = scale,
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    old_version = FALSE
  ) |>
    as.data.frame.lookoutliers() |>
    mutate(method = "New Lookout")
}

create_wine_figure <- function(results, show_anomalies = TRUE) {
  set_ggplot_options()
  df <- results |>
    mutate(alpha = 0.4 + 0.6 * as.numeric(outliers))
  if (show_anomalies) {
    p <- df |>
      ggplot(aes(x = points, y = price, color = outliers))
  } else {
    p <- df |>
      ggplot(aes(x = points, y = price))
  }
  p <- p +
    geom_point(alpha = df$alpha)
  if (show_anomalies) {
    p <- p +
      scale_color_manual(
        values = c(`FALSE` = "#999999", `TRUE` = "red"),
      ) +
      guides(color = "none")
  }
  p +
    labs(
      x = "Points",
      y = "Price ($US)",
      title = paste(
        "Reviews of",
        NROW(results),
        "Shiraz/Syrah wines from 'Wine Enthusiast', 15 June 2017"
      )
    )
}
