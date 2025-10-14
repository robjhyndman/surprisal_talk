analyse_wine_data <- function(wine_reviews, scale, alpha, beta, gamma) {
  lookobjNew <- lookout::lookout(
    wine_reviews,
    scale = scale,
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    old_version = FALSE
  ) |>
    as.data.frame.lookoutliers() |>
    mutate(method = "New Lookout")
}

create_wine_figure <- function(results, show_anomalies = TRUE, alpha = 0.001) {
  set_ggplot_options()
  if (
    show_anomalies &
      !("outliers" %in% colnames(results)) &
      ("prob" %in% colnames(results))
  ) {
    results <- results |>
      mutate(outliers = prob < alpha)
  }
  p <- results |>
    ggplot(aes(x = points, y = price))
  if (show_anomalies) {
    p <- p +
      geom_point(
        aes(color = outliers),
        alpha = 0.4 + 0.6 * as.numeric(results$outliers)
      ) +
      scale_color_manual(
        values = c(`FALSE` = "#999999", `TRUE` = "red"),
      ) +
      guides(color = "none")
  } else {
    p <- p +
      geom_point(alpha = 0.4)
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

augment_wine <- function(wine_reviews) {
  wine_lm <- lm(log(price) ~ points, data = wine_reviews)
  broom::augment(wine_lm, data = wine_reviews, interval = "prediction") |>
    mutate(
      lwr = exp(.lower),
      upr = exp(.upper),
      location = case_when(
        price < lwr ~ "below",
        price > upr ~ "above",
        TRUE ~ "within"
      ),
      std_loo_res = .resid / (.sigma * sqrt(1 - .hat)),
      surprisals = -dnorm(.std.resid, log = TRUE),
      loo_surprisals = -dnorm(std_loo_res, log = TRUE),
      prob = surprisals(wine_lm, approximation = "gpd")
    )
}

create_wine_model_plot <- function(wine_aug) {
  set_ggplot_options()
  wine_aug |>
    ggplot(aes(y = price, x = points, col = location)) +
    geom_jitter(height = 0, width = 0, alpha = 0.5) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "#cccccc", alpha = 0.25) +
    geom_line(aes(y = exp(.fitted)), color = "#666666") +
    scale_y_log10() +
    guides(fill = "none", col = "none") +
    scale_color_manual(values = c("#0072B2", "#D55E00", "#AAAAAA")) +
    labs(y = "Price", x = "Points")
}

plot_wine_loo_surprisals <- function(
  wine_aug,
  show_anomalies = FALSE,
  alpha = 0.001
) {
  set_ggplot_options()
  if (show_anomalies) {
    wine_aug <- wine_aug |>
      mutate(anomaly = prob < alpha)
    p <- wine_aug |>
      ggplot(aes(x = points, y = std_loo_res, col = anomaly)) +
      scale_color_manual(values = c(`FALSE` = "#999999", `TRUE` = "red"))
  } else {
    p <- wine_aug |>
      ggplot(aes(x = points, y = std_loo_res, col = location)) +
      scale_color_manual(values = c("#0072B2", "#D55E00", "#AAAAAA"))
  }
  p +
    geom_jitter(height = 0, width = 0, alpha = 0.5) +
    geom_hline(yintercept = 0, color = "#666666") +
    labs(x = "Points", y = "Standardized LOO residuals") +
    guides(fill = "none", col = "none")
}
