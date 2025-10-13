analyse_old_faithful <- function(oldfaithful, scale, alpha, beta, gamma) {
  lookobjNew <- lookout::lookout(
    oldfaithful[, c("duration", "waiting")],
    scale = scale,
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    old_version = FALSE
  )
  as.data.frame.lookoutliers(lookobjNew) |>
    mutate(method = "New Lookout")
}

create_old_faithful_figure <- function(results, show_anomalies = TRUE) {
  set_ggplot_options()

  p <- results |>
    ggplot(aes(x = duration, y = waiting))
  if (show_anomalies) {
    p <- p +
      geom_point(aes(color = outliers), alpha = 0.4 + 0.6 * results$outliers) +
      scale_color_manual(
        values = c(`FALSE` = "#999999", `TRUE` = "red")
      ) +
      guides(color = "none")
  } else {
    p <- p + geom_point(alpha = 0.4)
  }
  p +
    labs(
      x = "Eruption duration (seconds)",
      y = "Waiting time to next eruption (seconds)",
      title = "Old Faithful eruptions from 14 January 2017 to 29 December 2023"
    )
}
