create_fig_density <- function(distribution, y) {
  sup <- support(distribution) |> unclass()
  sup <- sup$lim[[1]]
  if (sup[1] == -Inf) {
    sup[1] <- quantile(distribution, 0.001)
  }
  if (sup[2] == Inf) {
    sup[2] <- quantile(distribution, 0.999)
  }
  df <- tibble(
    y = seq(sup[1], sup[2], l = 501),
    fy = density(distribution, at = y)[[1]]
  )
  falpha <- density(distribution, at = y)
  fy <- hdr_table(
    distribution,
    prob = seq(min(falpha, 0.01), max(falpha, 0.99), l = 20)
  )
  fy$below <- fy$density < falpha
  fy <- hdr_table(
    distribution,
    prob = seq(max(fy$prob[!fy$below]), min(fy$prob[fy$below]), l = 10)
  )
  palpha <- approx(fy$density, fy$prob, xout = falpha)$y |> suppressWarnings()
  hdr <- hdr_table(distribution, prob = palpha) |> select(lower, upper)
  # Invert HDRs
  hdr_inverse <- tibble(
    lower = c(sup[1], hdr$upper),
    upper = c(hdr$lower, sup[2])
  )
  xbreaks <- c(pretty(sup), y)
  p <- df |>
    ggplot(aes(x = y, y = fy)) +
    geom_line() +
    labs(x = "y", y = "Probability Density Function: f(y)") +
    geom_hline(aes(yintercept = falpha), col = "#D55E00", linetype = "dashed")
  for (i in seq(NROW(hdr_inverse))) {
    df_subset <- df |>
      filter(y >= hdr_inverse[i, ]$lower & y <= hdr_inverse[i, ]$upper)
    p <- p +
      geom_polygon(
        fill = "#D55E00",
        data = bind_rows(
          tibble(y = hdr_inverse[i, ]$lower, fy = 0),
          df_subset,
          tibble(y = hdr_inverse[i, ]$upper, fy = 0),
          tibble(y = hdr_inverse[i, ]$lower, fy = 0)
        )
      )
  }
  p +
    scale_x_continuous(
      breaks = xbreaks,
      labels = latex2exp::TeX(c(head(xbreaks, -1), "$y_i$")),
      minor_breaks = NULL
    ) +
    scale_y_continuous(
      breaks = c(-Inf, falpha),
      labels = latex2exp::TeX(c("Inf", "$f(y_i)$")),
      minor_breaks = NULL
    )
}
