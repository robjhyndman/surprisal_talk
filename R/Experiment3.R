# Experiment 3: Increasing N with normal distribution

generate_exp3 <- function(nn) {
  num_outliers <- ceiling(5 / 1000 * nn)
  meanx <- runif(num_outliers, min = -sqrt(2) * 2.2, max = sqrt(2) * 2.2)
  meany <- sqrt(2 * 2.2^2 - meanx^2)
  inds <- sample(seq(num_outliers), ceiling(num_outliers / 2))
  meany[inds] <- -1 * meany[inds]

  out <- bind_rows(
    data.frame(
      x = rnorm(nn),
      y = rnorm(nn)
    ),
    data.frame(
      x = rnorm(num_outliers, mean = meanx, sd = 0.1), # meanx
      y = rnorm(num_outliers, mean = meany, sd = 0.1) # meany
    )
  )
  colnames(out) <- c("Y1", "Y2")
  out$Points <- c(rep("Non-anomaly", nn), rep("Anomaly", num_outliers))
  as_tibble(out)
}

run_synthetic_exp3 <- function(scale, alpha, beta, gamma) {
  nnvals <- seq(10) * 1000
  nnvals <- rep(nnvals, each = 10)
  df3 <- df1 <- set_up_diff_metrics(length(nnvals))

  for (ii in seq_along(nnvals)) {
    X <- generate_exp3(nnvals[ii])
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

create_figure_exp3 <- function(results) {
  g1 <- ggplot(results, aes(x = N, y = value)) +
    geom_jitter(width = 1000 / 4, height = 0, alpha = 0.4, size = 0.75) +
    facet_wrap(name ~ .) +
    labs(x = "Number of points (n)", y = "Anomaly rate") +
    geom_smooth()

  # Generate Data to plot
  nn <- 10000
  df <- generate_exp3(nn) |>
    mutate(alpha = 0.4 + 0.6 * (Points == "Anomaly"))
  g2 <- ggplot(df, aes(Y1, Y2, color = Points)) +
    geom_point(alpha = df$alpha, size = 1) +
    coord_fixed() +
    scale_color_manual(
      values = c("Non-anomaly" = "#999999", "Anomaly" = "red"),
      name = "Points",
      breaks = c("Non-anomaly", "Anomaly"),
      labels = c("N(0, 1) x N(0,1)", "N(µ1, 0.1) x N(µ2, 0.1) | µ1^2 + µ2^2 = 2(2.2^2)")
    )
  experiment_plot(g2, g1, "Exp3.pdf")
}
