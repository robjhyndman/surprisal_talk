create_fig_surprisal_prob <- function(
  step = 0L,
  den = dist_chisq(5),
  clean = FALSE,
  size = 90
) {
  if (identical(den, dist_chisq(5))) {
    ygrid <- seq(0, 15, l = 501)
    xbreaks <- seq(0, 15, by = 5)
    ybreaks <- seq(0, 0.15, by = 0.05)
  } else {
    ygrid <- seq(-4, 4, l = 501)
    xbreaks <- seq(-4, 4, by = 1)
    ybreaks <- seq(0, 0.5, by = 0.10)
  }
  hdr <- hdr(den, size = size) |> unlist() %>% `[`(c("lower", "upper"))
  falpha <- density(den, at = hdr["lower"])
  df <- tibble(
    y = ygrid,
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
        breaks = xbreaks,
        minor_breaks = NULL
      )
    scale_y_continuous(
      breaks = ybreaks,
      labels = latex2exp::TeX(sprintf("%.2f", ybreaks)),
      minor_breaks = NULL
    )
  } else if (step > 0) {
    p <- p +
      scale_x_continuous(
        breaks = c(xbreaks, hdr[2]),
        labels = latex2exp::TeX(c(xbreaks, "$y_i$")),
        minor_breaks = NULL
      ) +
      scale_y_continuous(
        breaks = c(ybreaks, falpha),
        labels = latex2exp::TeX(c(sprintf("%.2f", ybreaks), "$f(y_i)$")),
        minor_breaks = NULL
      ) +
      theme(panel.grid.major.x = element_blank())
    if (step == 1L) {
      p <- p +
        geom_segment(
          data = tibble(x = hdr[2], falpha = falpha),
          aes(x = x, xend = x, y = 0, yend = falpha),
          col = "#D55E00",
          linetype = "dashed"
        ) # +
      #geom_segment(
      #  data = tibble(x = hdr[1], falpha = falpha),
      #  aes(x = x, xend = x, y = 0, yend = falpha),
      #  col = "#D55E00",
      #  linetype = "dashed"
      #)
    }
    if (!clean) {
      p <- p +
        geom_hline(
          aes(yintercept = falpha),
          col = "#D55E00",
          linetype = "dashed"
        )
      p$layers <- append(
        p$layers,
        list(geom_vline(xintercept = xbreaks, colour = "white")),
        after = 0
      )
    }
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
            tibble(y = c(hdr[1], hdr[1], min(df$y)), fy = c(0, falpha, 0))
          ) |>
          arrange(fy, desc(y))
      )
  }
  if (clean && identical(den, dist_normal())) {
    p <- p +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()
      )
    file <- "figs/dist_normal.pdf"
    Cairo::CairoPDF(file = file, width = 8, height = 4)
    print(p)
    crop::dev.off.crop(file)
    return(file)
  }
  return(p)
}
