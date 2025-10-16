of_labels <- labs(
  x = "Eruption duration (seconds)",
  y = "Waiting time to next eruption (seconds)",
  title = "Old Faithful eruptions from 14 January 2017 to 29 December 2023"
)

analyse_old_faithful <- function(oldfaithful, scale, alpha, beta, gamma) {
  lookobjNew <- lookout::lookout(
    oldfaithful[, c("duration", "waiting")],
    scale = scale,
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    old_version = FALSE
  )
  as.data.frame.lookoutliers(lookobjNew)
}

create_old_faithful_figure <- function(
  results,
  scale = FALSE,
  contour = FALSE,
  show_anomalies = FALSE
) {
  set_ggplot_options()
  alpha <- 500 / NROW(results)
  if (scale) {
    df <- mvscale(results[, 1:2])
    colnames(df) <- c("duration", "waiting")
    df$outliers <- results$outliers
  } else {
    df <- results
  }
  if (contour) {
    p <- dist_kde(df[, 1:2]) |>
      gg_density(show_points = !show_anomalies, colors = "black")
  } else {
    p <- df |>
      ggplot(aes(x = duration, y = waiting))
    if (!show_anomalies) {
      p <- p +
        geom_point(alpha = alpha, color = "black")
    }
  }
  if (show_anomalies) {
    p <- p +
      geom_point(
        data = df,
        aes(x = duration, y = waiting, color = outliers),
        alpha = alpha + (1 - alpha) * results$outliers
      ) +
      scale_color_manual(
        values = c(`FALSE` = "#999999", `TRUE` = "red")
      ) +
      guides(color = "none")
  }
  if (scale) {
    p +
      labs(
        x = "Z1",
        y = "Z2",
        title = "Standardized Old Faithful eruptions from 14 January 2017 to 29 December 2023"
      )
  } else {
    p + of_labels
  }
}

get_of_lookout_prob <- function(oldfaithful, alpha = 0.01) {
  of <- oldfaithful |>
    select(duration, waiting)
  oldfaithful$prob <- lookout_prob(of)
  oldfaithful |>
    filter(prob < alpha) |>
    arrange(duration)
}
