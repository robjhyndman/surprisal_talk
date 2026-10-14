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
          col = "#F39B00",
          linetype = "dashed"
        ) # +
      #geom_segment(
      #  data = tibble(x = hdr[1], falpha = falpha),
      #  aes(x = x, xend = x, y = 0, yend = falpha),
      #  col = "#F39B00",
      #  linetype = "dashed"
      #)
    }
    if (!clean) {
      p <- p +
        geom_hline(
          aes(yintercept = falpha),
          col = "#F39B00",
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
        fill = "#F39B00",
        data = df |>
          filter(y >= hdr[2]) |>
          bind_rows(
            tibble(y = c(hdr[2], hdr[2], max(df$y)), fy = c(0, falpha, 0))
          ) |>
          arrange(fy, y)
      ) +
      geom_polygon(
        fill = "#F39B00",
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

# Data for the stages of the GPD approximation to the surprisal probabilities:
# surprisals of N(0,1) data under an assumed t(4) density, the threshold u
# (leaving the largest beta above it), the fitted GPD, and one observation.
gpd_stage_data <- function(n = 1000, beta = 0.1, obs_rank = 20, seed = 1,
                           binwidth = 0.1) {
  set.seed(seed)
  s <- -log(dt(rnorm(n), df = 4))
  u <- unname(stats::quantile(s, prob = 1 - beta, type = 8))
  gpd <- evd::fpot(s, threshold = u, std.err = FALSE)$estimate
  s_obs <- sort(s, decreasing = TRUE)[obs_rank]
  p_obs <- beta *
    evd::pgpd(s_obs, loc = u, scale = gpd["scale"], shape = gpd["shape"],
      lower.tail = FALSE)
  xmax <- max(s) + 0.1
  # beta times the GPD density, on the scale of the histogram of all surprisals.
  # dgpd() is zero at the threshold itself, so start just above it.
  gpd_den <- tibble(x = seq(u + 1e-6, xmax, length.out = 301)) |>
    mutate(y = beta * evd::dgpd(x, loc = u, scale = gpd["scale"], shape = gpd["shape"]))
  breaks <- seq(u - binwidth * ceiling((u - min(s)) / binwidth), xmax + binwidth,
    by = binwidth)
  heights <- hist(s, breaks = breaks, plot = FALSE)$counts / (n * binwidth)
  list(
    s = s, n = n, beta = beta, u = u, s_obs = s_obs, p_obs = p_obs,
    xmin = breaks[1], xmax = xmax,
    binwidth = binwidth, gpd_den = gpd_den,
    # Top of the y-axis for the full view and for the tail
    top_full = max(heights) * 1.05,
    top_tail = max(heights[breaks[-1] > u], gpd_den$y) * 1.15
  )
}

# One stage of the GPD figure: (1) histogram of the surprisals; (2) threshold u,
# with the largest beta of them highlighted; (3) the GPD density, scaled by
# beta; (4) the estimated probability for one observation as a shaded area.
# `zoom` moves the view from all surprisals (0) to the tail above u (1).
gpd_stage_plot <- function(d, level, zoom = if (level >= 3) 1 else 0) {
  colours <- c(bulk = "#cbd5e1", tail = "#F39B00", gpd = "#c14b14", u = "#0f2a44")
  ease <- (1 - cos(pi * zoom)) / 2
  xlim <- c(d$xmin + ease * (d$u - d$xmin), d$xmax)
  ytop <- exp(log(d$top_full) + ease * (log(d$top_tail) - log(d$top_full)))
  math <- function(label, ...) {
    annotate("text", label = label, parse = TRUE, family = "KaTeX", size = 6.5, ...)
  }
  df <- tibble(s = d$s, tail = level >= 2 & d$s > d$u)
  p <- ggplot(df, aes(x = s)) +
    geom_histogram(
      aes(y = after_stat(count) / (d$n * d$binwidth), fill = tail),
      binwidth = d$binwidth, boundary = d$u, colour = "#fbf9f4", linewidth = 0.2
    ) +
    scale_fill_manual(values = c(`FALSE` = colours[["bulk"]], `TRUE` = colours[["tail"]]),
      guide = "none") +
    labs(x = "Surprisal", y = "Density")
  if (level >= 2) {
    # The label fades out as the zoom starts
    fade <- max(0, 1 - zoom / 0.3)
    p <- p +
      annotate("segment", x = d$u, xend = d$u, y = 0, yend = Inf,
        colour = colours[["u"]], linetype = "dashed") +
      math("italic(u)", x = d$u, y = Inf, hjust = -0.4, vjust = 1.2, colour = colours[["u"]])
    if (fade > 0) {
      p <- p +
        annotate("text", label = "largest 10%\nof surprisals", family = "Fira Sans",
          size = 5.5, x = (d$u + d$xmax) / 2, y = d$top_full * 0.35,
          colour = colours[["tail"]], alpha = fade, lineheight = 0.9)
    }
  }
  if (level >= 4) {
    area <- d$gpd_den |>
      filter(x >= d$s_obs) |>
      bind_rows(tibble(x = c(d$xmax, d$s_obs), y = 0))
    gpd_at <- function(x) approx(d$gpd_den$x, d$gpd_den$y, xout = x)$y
    # Arrow from the label down into the shaded region
    tip <- d$s_obs + 0.25
    p <- p +
      geom_polygon(data = area, aes(x = x, y = y), fill = colours[["gpd"]], alpha = 0.5) +
      annotate("segment", x = d$s_obs, xend = d$s_obs, y = 0, yend = gpd_at(d$s_obs),
        colour = colours[["gpd"]]) +
      annotate("segment", x = tip + 0.3, xend = tip, y = d$top_tail * 0.45,
        yend = gpd_at(tip) * 0.5, colour = colours[["gpd"]],
        arrow = arrow(length = unit(0.12, "inches"), type = "closed")) +
      math("hat(italic(p))[italic(t)]", x = tip + 0.3, y = d$top_tail * 0.45,
        vjust = -0.2, colour = colours[["gpd"]])
  }
  if (level >= 3) {
    top <- max(d$gpd_den$y)
    p <- p +
      geom_line(data = d$gpd_den, aes(x = x, y = y), colour = colours[["gpd"]],
        linewidth = 1) +
      math("italic(\"\u03b2\")~hat(italic(g))(italic(s))", x = d$u + 0.25, y = top * 0.9, hjust = 0,
        colour = colours[["gpd"]]) +
      annotate("text", label = "GPD fitted to the excesses", family = "Fira Sans",
        size = 5.5, x = d$xmax, y = d$top_tail * 0.95, hjust = 1, vjust = 1,
        colour = colours[["gpd"]])
  }
  # Tick labels in markdown in every stage, so they sit at the same height when
  # the last stage adds s_t (in KaTeX fonts) below the axis
  obs <- if (level >= 4) d$s_obs
  s_t <- paste0(
    "<span style='font-size:15pt; color:", colours[["gpd"]], "'>",
    katex_it("s"), "<sub>", katex_it("t"), "</sub></span>"
  )
  p +
    scale_x_continuous(
      breaks = function(lim) c(scales::breaks_extended()(lim), obs),
      labels = function(b) {
        regular <- !(b %in% obs)
        out <- rep(s_t, length(b))
        out[regular] <- format(b[regular], trim = TRUE)
        out
      }
    ) +
    theme(axis.text.x.bottom = ggtext::element_markdown()) +
    coord_cartesian(xlim = xlim, ylim = c(0, ytop), expand = FALSE)
}

# PNGs of the four stages, and an MP4 zooming from stage 2 into the tail
# (ending at stage 3 without the GPD). All are drawn from grobs aligned
# together, at the same resolution (text is laid out slightly differently at
# other resolutions), so the video and the PNGs overlay exactly. The video is
# then scaled down to `video_width` pixels. Returns the PNG files,
# then the MP4 and its poster (the last frame).
gpd_stage_files <- function(d, dir, seconds = 1.5, fps = 30, width = 9, height = 3.8,
                            res = 400, video_width = 1800, background = "#fbf9f4") {
  register_katex_font()
  zoom <- seq(0, 1, length.out = seconds * fps)
  plots <- c(
    lapply(1:4, function(level) gpd_stage_plot(d, level)),
    lapply(zoom, function(z) gpd_stage_plot(d, 2, zoom = z))
  )
  # ggplotGrob() measures text on the current device, so open a matching one
  tmp <- tempfile(fileext = ".png")
  ragg::agg_png(tmp, width = width, height = height, units = "in", res = res)
  grobs <- with_uconn_style(align_ggplots(plots))
  grDevices::dev.off()
  unlink(tmp)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  pngs <- file.path(dir, paste0("fig_gpd", 1:4, ".png"))
  mapply(save_png, grobs[1:4], pngs,
    MoreArgs = list(width = width, height = height, res = res))
  frames_dir <- tempfile("frames")
  dir.create(frames_dir)
  frames <- file.path(frames_dir, sprintf("frame%03d.png", seq_along(zoom)))
  for (i in seq_along(zoom)) {
    ragg::agg_png(frames[i], width = width, height = height, units = "in",
      res = res, background = background)
    grid::grid.draw(grobs[[4 + i]])
    grDevices::dev.off()
  }
  video <- file.path(dir, "fig_gpd_zoom.mp4")
  poster <- file.path(dir, "fig_gpd_zoom.png")
  file.copy(frames[length(frames)], poster, overwrite = TRUE)
  av::av_encode_video(frames, video, framerate = fps,
    vfilter = sprintf("scale=%d:-2", video_width), verbose = FALSE)
  unlink(frames_dir, recursive = TRUE)
  c(pngs, video, poster)
}
