# Experiment 4: Increasing N with gamma distribution

generate_exp4 <- function(nn) {
  num_outliers <- ceiling(5 / 1000 * nn)
  Y1 <- matrix(rgamma(n = 2 * nn, shape = 2, rate = 2), ncol = 2)
  Y2 <- matrix(rgamma(n = 2 * nn, shape = 2.2, rate = 2), ncol = 2)
  x1dist <- apply(Y1, 1, function(x) sqrt(x[1]^2 + x[2]^2))
  qq <- quantile(x1dist, probs = 0.99)
  x2dist <- apply(Y2, 1, function(x) sqrt(x[1]^2 + x[2]^2))
  inds <- sample(which(x2dist > qq), num_outliers)
  out <- as.data.frame(rbind(Y1, Y2[inds, ]))
  colnames(out) <- c("Y1", "Y2")
  out$Points <- c(
    rep("Non-anomaly", NROW(Y1)),
    rep("Anomaly", length(inds))
  )
  as_tibble(out)
}

run_synthetic_exp4 <- function(scale, alpha, beta, gamma) {
  nnvals <- rep((1:10) * 1000, each = 10)
  df3 <- df1 <- set_up_diff_metrics(length(nnvals))

  for (ii in 1:length(nnvals)) {
    X <- generate_exp4(nnvals[[ii]])
    lookobj_new <- lookout::lookout(
      X[, 1:2],
      scale = scale,
      alpha = alpha,
      beta = beta,
      gamma = gamma,
      old_version = FALSE
    )
    act <- X$Points == "Anomaly"
    df1[ii, ] <- diff_metrics(act, which_outliers(lookobj_new))
  }

  df1 |>
    select(
      N,
      true_positive_rate,
      true_negative_rate,
      false_positive_rate,
      false_negative_rate
    ) |>
    rename(
      "True Positive Rate" = true_positive_rate,
      "False Positive Rate" = false_positive_rate,
      "False Negative Rate" = false_negative_rate,
      "True Negative Rate" = true_negative_rate
    ) |>
    tidyr::pivot_longer(cols = 2:5)
}

create_figure_exp4 <- function(results) {
  g1 <- results |>
    ggplot(aes(x = N, y = value)) +
    geom_jitter(size = 0.75, width = 1000 / 4, height = 0.0, alpha = 0.4) +
    facet_wrap(name ~ .) +
    labs(x = "Number of points (n)", y = "Anomaly rate") +
    geom_smooth()

  # Generate Data to plot
  nn <- 10000
  df_data <- generate_exp4(nn) |>
    mutate(alpha = 0.4 + 0.6 * (Points == "Anomaly"))

  g2 <- ggplot(df_data, aes(Y1, Y2, color = Points)) +
    geom_point(alpha = df_data$alpha, size = 1) +
    coord_fixed() +
    scale_color_manual(
      values = c("Non-anomaly" = "#999999", "Anomaly" = "red"),
      name = "Points",
      breaks = c("Non-anomaly", "Anomaly"),
      labels = c("Gamma(2, 2) x Gamma(2, 2)", "Gamma(2.2, 2) x Gamma(2.2, 2) | |Y| > c")
    )

  experiment_plot(g2, g1, "Exp4.pdf")
}
