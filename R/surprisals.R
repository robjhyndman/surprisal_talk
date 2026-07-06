create_fig_surprisal_prob <- function(step = 0L) {
  den <- dist_chisq(5)
  hdr <- hdr(den, size = 90) |> unlist() %>% `[`(c("lower", "upper"))
  falpha <- density(den, at = hdr["lower"])
  df <- tibble(
    y = seq(0, 15, l = 501),
    fy = density(den, at = y)[[1]],
    s = -log(fy),
    p = surprisals_prob(y, distribution = den)
  )
  palpha <- 0.1
  p <- df |>
    ggplot(aes(x = y, y = fy)) +
    geom_line() +
    labs(x = "y", y = "Probability Density Function: f(y)")
  if (step == 0L) {
    p <- p +
      scale_x_continuous(
        breaks = seq(0, 15, by = 5),
        minor_breaks = NULL
      )
    scale_y_continuous(
      breaks = c(seq(0, 0.15, by = 0.05)),
      labels = latex2exp::TeX(sprintf("%.2f", seq(0, 0.15, by = 0.05))),
      minor_breaks = NULL
    )
  } else if (step > 0) {
    p <- p +
      scale_x_continuous(
        breaks = c(seq(0, 15, by = 5), hdr[2]),
        labels = latex2exp::TeX(c(seq(0, 15, by = 5), "$y_i$")),
        minor_breaks = NULL
      ) +
      scale_y_continuous(
        breaks = c(seq(0, 0.15, by = 0.05), falpha),
        labels = latex2exp::TeX(c(
          sprintf("%.2f", seq(0, 0.15, by = 0.05)),
          "$f(y_i)$"
        )),
        minor_breaks = NULL
      ) +
      theme(panel.grid.major.x = element_blank()) +
      geom_hline(
        aes(yintercept = falpha),
        col = "#D55E00",
        linetype = "dashed"
      ) +
      geom_segment(
        aes(x = hdr[2], xend = hdr[2], y = 0, yend = falpha),
        col = "#D55E00",
        linetype = "dashed"
      )
    p$layers <- append(
      p$layers,
      list(geom_vline(xintercept = seq(0, 15, by = 5), colour = "white")),
      after = 0
    )
  }
  if (step == 2L) {
    p <- p +
      geom_polygon(
        fill = "#D55E00",
        data = df |>
          filter(y >= hdr[2]) |>
          bind_rows(
            tibble(y = c(hdr[2], hdr[2], max(df$y)), fy = c(0, falpha, 0))
          ) |>
          arrange(fy, y)
      ) +
      geom_polygon(
        fill = "#D55E00",
        data = df |>
          filter(y <= hdr["lower"]) |>
          bind_rows(
            tibble(y = c(hdr[1], hdr[1], 0), fy = c(0, falpha, 0))
          ) |>
          arrange(fy, desc(y))
      )
  }
  return(p)
}
