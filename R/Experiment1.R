# Experiment 1: Gamma distribution

generate_exp1 <- function(n1, n2, rate) {
  # Generate random data
  out <- rbind(
    matrix(rgamma(n = 2 * n1, shape = 2, rate = 2), ncol = 2),
    matrix(rgamma(n = 2 * n2, shape = 2, rate = rate), ncol = 2)
  ) |>
    as.data.frame()
  colnames(out) <- c("Y1", "Y2")
  out$Points <- c(rep("Non-anomaly", n1), rep("Anomaly", n2))
  as_tibble(out)
}

run_synthetic_exp1 <- function(scale, alpha, beta, gamma) {
  n1 <- 500
  n2 <- 10
  rate2 <- seq(10) / 10

  results_old <- results_new <- tibble(
    outrate = numeric(10 * length(rate2)),
    outliers = 0
  ) |>
    bind_cols(set_up_diff_metrics(10 * length(rate2)))

  kk <- 1
  for (jj in 1:10) {
    for (i in 1:length(rate2)) {
      X <- generate_exp1(n1, n2, rate2[i])
      lookobj_new <- lookout::lookout(
        X[, 1:2],
        scale = scale,
        alpha = alpha,
        beta = beta,
        gamma = gamma,
        old_version = FALSE
      )
      act <- X$Points == "Anomaly"
      results_new[kk, ] <- c(
        rate2[i],
        n2,
        diff_metrics(act, which_outliers(lookobj_new))
      )
      kk <- kk + 1
    }
  }
  # Process the results data directly
  results_new |>
    select(
      outrate,
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
    tidyr::pivot_longer(cols = 2:5, names_to = "metric", values_to = "value")
}

create_figure_exp1 <- function(results) {
  g1 <- ggplot(results, aes(x = outrate, y = value)) +
    geom_jitter(width = 0.1 / 4, height = 0, alpha = 0.4, size = 0.75) +
    geom_smooth() +
    facet_wrap(metric ~ .) +
    labs(x = "r", y = "Anomaly rate") +
    guides(color = guide_legend(title = "Algorithm")) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2))

  # Generate random data for plotting
  n1 <- 500
  n2 <- 10
  rate2 <- c(0.2, 0.9)

  df <- bind_rows(
    generate_exp1(n1, n2, rate2[1]),
    generate_exp1(n1, n2, rate2[2])
  ) |>
    mutate(
      rate = paste("r =", rep(rate2, each = n1 + n2)),
      alpha = 0.4 + 0.6 * (Points == "Anomaly")
    )

  g2 <- ggplot(df, aes(Y1, Y2, color = Points)) +
    geom_point(alpha = df$alpha, size = 0.75) +
    facet_wrap(~rate, strip.position = "top", nrow = 1) +
    coord_fixed() +
    scale_color_manual(
      values = c("Non-anomaly" = "#999999", "Anomaly" = "red"),
      name = "Points",
      breaks = c("Non-anomaly", "Anomaly"),
      labels = c("Gamma(2, 2) x Gamma(2, 2)", "Gamma(2, r) x Gamma(2, r)")
    )

  experiment_plot(g2, g1, "Exp1_Gamma_Rates.pdf")
}
