analyse_old_faithful <- function(scale, alpha, beta, gamma) {
  oldfaithful2 <- weird::oldfaithful |>
    filter(duration < 7200, waiting < 7200)

  lookobjNew <- lookout::lookout(
    oldfaithful2[, 2:3],
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

  df <- results |>
    mutate(alpha = 0.4 + 0.6 * as.numeric(outliers))
  if(show_anomalies) {
  p <- df |>
      ggplot(aes(x = duration, y = waiting, color = outliers)) 
  } else {
    p <- df |>
      ggplot(aes(x = duration, y = waiting))
  }
  p <- p + 
      geom_point(alpha = df$alpha) 
  if(show_anomalies) {
    p <- p +
      scale_color_manual(
        values = c(`FALSE` = "#999999", `TRUE` = "red")
      ) +
      guides(color = "none")
  } 
  p +
    labs(
      x = "Eruption duration (seconds)",
      y = "Waiting time to next eruption (seconds)",
      title = 
        "Old Faithful eruptions from 1 January 2015 to 31 December 2024"
      )
    
}
