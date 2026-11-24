# Pharmaceutical surveillance: PBS scripts

# Surveillance: PBS totals by ATC2 group (thousands of scripts)
make_pbs <- function(PBS) {
  PBS |>
    arrange(ATC2, Month) |>
    group_by(ATC2) |>
    summarise(Scripts = sum(Scripts) / 1e3, .groups = "drop") |>
    mutate(t = as.numeric(Month - min(Month) + 1))
}

# The PBS data after each step of the pipeline shown on the slide (raw,
# grouped, summarised, with time index), for revealing the output in steps
pbs_steps <- function(PBS) {
  grouped <- PBS |> group_by(ATC2)
  summarised <- grouped |> summarise(Scripts = sum(Scripts) / 1e3)
  list(
    PBS,
    grouped,
    summarised,
    summarised |> mutate(t = as.numeric(Month - min(Month) + 1))
  )
}

# Surveillance anomalies for several ATC2 groups, one panel each; `series` is
# a vector of group descriptions named by ATC2 code
pbs_plot <- function(pbs, anomalies, series) {
  label <- function(x) paste0(x, " (", series[x], ")")
  dates <- anomalies |>
    as_tibble() |>
    filter(ATC2 %in% names(series)) |>
    mutate(ATC2 = factor(label(ATC2), levels = label(names(series))))
  pbs |>
    filter(ATC2 %in% names(series)) |>
    mutate(ATC2 = factor(label(ATC2), levels = label(names(series)))) |>
    ggplot(aes(x = Month, y = Scripts)) +
    scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
    geom_vline(data = dates, aes(xintercept = Month), colour = "#C14b14") +
    geom_line() +
    facet_wrap(vars(ATC2), ncol = 1, scales = "free_y") +
    labs(x = NULL, y = "Scripts (thousands)")
}

# Surveillance: one-step ETS forecasts from the first `i` months, for series
# with at least 36 observations from their first non-zero value. Returns the
# data after each step shown on the slide (the last is the forecasts)
pbs_forecast_steps <- function(pbs, i = 36) {
  window <- pbs |> filter(t <= i)
  long <- window |>
    group_by_key() |>
    filter(cumsum(Scripts) > 0, n() >= 36) |>
    ungroup()
  fit <- long |> model(ets = ETS(Scripts))
  list(pbs, window, long, fit, fit |> forecast(h = 1))
}

# Surprisal probabilities of the observations in the month forecast by
# `pbs_fc`. Returns the data after each step shown on the slide (the last has
# the probabilities)
pbs_forecast_probs_steps <- function(pbs_fc, pbs) {
  renamed <- pbs_fc |> rename(fcast = Scripts)
  joined <- renamed |> left_join(pbs, by = c("ATC2", "Month"))
  list(
    pbs_fc,
    renamed,
    joined,
    joined |> mutate(prob = surprisals_prob(Scripts, distribution = fcast))
  )
}

# One-step ETS forecasts of one series from each origin in `origins` (as in
# compute_pbs_scores()): the Normal forecast mean and sd, the observation at
# the next time and its surprisal probability
pbs_roll_data <- function(pbs, series = "S01", origins = 36:95) {
  pbs <- pbs |> filter(ATC2 == series)
  fc <- lapply(origins, function(i) {
    fc <- pbs |>
      filter(t <= i) |>
      model(ets = ETS(Scripts)) |>
      forecast(h = 1)
    obs <- pbs |> filter(t == i + 1)
    par <- distributional::parameters(fc$Scripts)
    tibble(
      origin = i,
      mu = par$mu,
      sigma = par$sigma,
      y = obs$Scripts,
      prob = surprisals_prob(obs$Scripts, distribution = fc$Scripts)
    )
  }) |>
    bind_rows()
  list(
    series = as_tibble(pbs) |> select(t, Month, Scripts),
    fc = fc
  )
}

# One frame of the rolling surveillance figure, at forecast origin `origin`:
# (1) the data up to the origin; (2) the one-step forecast density, drawn
# sideways; (3) the observation and its surprisal; (4) the surprisal
# probability as the shaded tails. With `labels = FALSE` (the video frames)
# nothing is labelled, and earlier observations are shown coloured by their p.
pbs_roll_plot <- function(d, origin, level = 4, labels = TRUE, width = 100) {
  colours <- c(density = "#0f2a44", fill = "#cbd5e1", origin = "#64748b",
    low = "#94a3b8", mid = "#F39B00", high = "#c14b14")
  p_colour <- function(p) {
    unname(colours[ifelse(p < 0.001, "high", ifelse(p < 0.01, "mid", "low"))])
  }
  # Label text in a darker amber, legible on the grey panel
  text_colour <- function(p) {
    col <- p_colour(p)
    ifelse(col == colours[["mid"]], "#a86900", col)
  }
  date <- function(t) as.Date(d$series$Month[match(t, d$series$t)])
  math <- function(label, ...) {
    annotate("text", label = label, parse = TRUE, family = "KaTeX", size = 6, ...)
  }
  fc <- d$fc |> filter(.data$origin == !!origin)
  x0 <- date(origin + 1)
  z_obs <- (fc$y - fc$mu) / fc$sigma
  # Values at least as surprising as the observation: both Normal tails
  cut <- abs(z_obs)
  # Sideways Normal density with its peak `width` days wide, evaluated at the
  # tail boundaries so the shaded tails end exactly there
  z <- seq(-1, 1, length.out = 401) * max(4, cut + 0.5)
  dens <- tibble(z = sort(unique(c(z, -cut, cut)))) |>
    mutate(y = fc$mu + fc$sigma * z, x = x0 + width * exp(-z^2 / 2))
  # Fixed extents over all origins, so all frames overlay exactly
  ylim <- range(d$series$Scripts[d$series$t <= max(d$fc$origin) + 1],
    d$fc$mu - 3 * d$fc$sigma, d$fc$mu + 3 * d$fc$sigma)
  xlim <- c(date(1) - 30, date(max(d$fc$origin) + 1) + width + 60)
  ytop <- ylim[2]
  p <- ggplot(d$series |> filter(t <= origin), aes(x = as.Date(Month), y = Scripts)) +
    annotate("segment", x = date(origin), xend = date(origin), y = ylim[1], yend = ytop,
      colour = colours[["origin"]], linetype = "dashed") +
    math("italic(t)", x = date(origin), y = ytop, hjust = 1.4, vjust = 1,
      colour = colours[["origin"]]) +
    geom_line() +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE) +
    labs(x = NULL, y = "Scripts (thousands)",
      title = "Scripts for ATC group S01 (Ophthalmologicals)")
  if (!labels) {
    past <- d$fc |>
      filter(.data$origin < !!origin) |>
      mutate(x = date(.data$origin + 1))
    p <- p +
      geom_point(data = past, aes(x = x, y = y), colour = p_colour(past$prob), size = 1.6)
  }
  if (level >= 2) {
    p <- p +
      geom_polygon(data = bind_rows(dens, tibble(x = x0, y = rev(range(dens$y)))),
        aes(x = x, y = y), fill = colours[["fill"]], colour = NA) +
      geom_path(data = dens, aes(x = x, y = y), colour = colours[["density"]])
    if (labels) {
      p <- p +
        math("italic(f)[italic(t)+1*\"|\"*italic(t)](italic(y))", x = x0 + width + 20, y = fc$mu,
          hjust = 0, colour = colours[["density"]])
    }
  }
  if (level >= 4) {
    tails <- bind_rows(
      dens |> filter(z <= -cut) |> mutate(tail = "lower"),
      dens |> filter(z >= cut) |> mutate(tail = "upper")
    ) |>
      group_by(tail) |>
      group_modify(~ bind_rows(.x, tibble(x = x0, y = rev(range(.x$y))))) |>
      ungroup()
    p <- p +
      geom_polygon(data = tails, aes(x = x, y = y, group = tail),
        fill = p_colour(fc$prob))
    # Thin tails (p < 0.05) are hard to see, so also trace them along the
    # curve, which is then nearly vertical, so the trace ends are horizontal
    if (cut > 2) {
      p <- p +
        geom_path(data = dens |> filter(abs(z) >= cut), aes(x = x, y = y, group = z > 0),
          colour = p_colour(fc$prob), linewidth = 1.5)
    }
    if (labels) {
      p_label <- sprintf("italic(p)[italic(t)+1] == %s",
        formatC(signif(fc$prob, 2), format = "fg", digits = 2))
      p <- p +
        math(p_label, x = x0 + width + 20, y = fc$y - 3.5 * fc$sigma,
          hjust = 0, colour = text_colour(fc$prob))
    }
  }
  if (level >= 3) {
    p <- p +
      geom_point(aes(x = x0, y = fc$y), data = tibble(), colour = p_colour(fc$prob),
        size = 3)
    if (labels) {
      p <- p +
        math("italic(s)[italic(t)+1] == -log~italic(f)[italic(t)+1*\"|\"*italic(t)](italic(y)[italic(t)+1])",
          x = x0 + width + 20, y = fc$y, hjust = 0, colour = text_colour(fc$prob))
    }
  }
  p
}

# PNGs of the four stages at the first origin, then an MP4 rolling the origin
# forward (slowly at first) and its poster (the last frame). All are drawn
# from grobs aligned together, at the same resolution, so the video overlays
# the PNGs exactly; the video is then scaled down to `video_width` pixels.
pbs_roll_files <- function(d, dir, fps = 20, slow = 4, slow_frames = 10, width = 9,
                           height = 4.2, res = 400, video_width = 1800,
                           background = "#fbf9f4") {
  register_katex_font()
  origins <- d$fc$origin
  plots <- c(
    lapply(1:4, function(level) pbs_roll_plot(d, origins[1], level)),
    lapply(origins[-1], function(i) pbs_roll_plot(d, i, labels = FALSE))
  )
  # ggplotGrob() measures text on the current device, so open a matching one
  tmp <- tempfile(fileext = ".png")
  ragg::agg_png(tmp, width = width, height = height, units = "in", res = res)
  grobs <- with_slide_style(align_ggplots(plots))
  grDevices::dev.off()
  unlink(tmp)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  pngs <- file.path(dir, paste0("fig_pbs_roll", 1:4, ".png"))
  mapply(save_png, grobs[1:4], pngs,
    MoreArgs = list(width = width, height = height, res = res))
  # Video starts from stage 4; the first `slow` steps are held for
  # `slow_frames` frames each, the rest for one frame, and the end for a second
  frames_dir <- tempfile("frames")
  dir.create(frames_dir)
  steps <- c(4, 4 + seq_along(origins[-1]))
  hold <- c(rep(slow_frames, slow + 1), rep(1, length(steps) - slow - 2), fps)
  drawn <- file.path(frames_dir, sprintf("step%03d.png", seq_along(steps)))
  for (i in seq_along(steps)) {
    ragg::agg_png(drawn[i], width = width, height = height, units = "in",
      res = res, background = background)
    grid::grid.draw(grobs[[steps[i]]])
    grDevices::dev.off()
  }
  frames <- rep(drawn, hold)
  video <- file.path(dir, "fig_pbs_roll.mp4")
  poster <- file.path(dir, "fig_pbs_roll_end.png")
  file.copy(drawn[length(drawn)], poster, overwrite = TRUE)
  optimise_png(poster)
  av::av_encode_video(frames, video, framerate = fps,
    vfilter = sprintf("scale=%d:-2", video_width), verbose = FALSE)
  unlink(frames_dir, recursive = TRUE)
  c(pngs, video, poster)
}

# Heat map of the surprisal probabilities of all series (rows) over time,
# filled in up to month number `upto` (0 for an empty map), with a line at the
# current month. Colours as in pbs_roll_plot().
pbs_heatmap_plot <- function(pbs_scores, upto) {
  colours <- c(low = "#d5dbe3", mid = "#F39B00", high = "#c14b14")
  p_lab <- function(x) paste(katex_it("p"), x)
  d <- pbs_scores |>
    as_tibble() |>
    mutate(
      x = lubridate::year(Month) + (lubridate::month(Month) - 1) / 12,
      row = match(ATC2, sort(unique(ATC2))),
      level = factor(
        ifelse(prob < 0.001, "high", ifelse(prob < 0.01, "mid", "low")),
        levels = names(colours)
      )
    )
  months <- sort(unique(d$x))
  groups <- sort(unique(d$ATC2))
  # Label the first group of each anatomical main group (first letter)
  first <- which(!duplicated(substr(groups, 1, 1)))
  now <- if (upto > 0) months[upto] else months[1] - 1 / 12
  ggplot(d |> filter(!is.na(prob), x <= now), aes(x = x, y = row, fill = level)) +
    geom_raster() +
    annotate("segment", x = now + 1 / 24, xend = now + 1 / 24, y = 0.5,
      yend = length(groups) + 0.5, colour = "#0f2a44", linewidth = 0.8) +
    scale_fill_manual(
      values = colours, limits = names(colours), drop = FALSE,
      labels = c(p_lab("&ge; 0.01"), p_lab("&lt; 0.01"), p_lab("&lt; 0.001")),
      name = NULL
    ) +
    scale_x_continuous(breaks = seq(1995, 2008, by = 1), expand = c(0, 0)) +
    scale_y_reverse(breaks = first, labels = substr(groups[first], 1, 1),
      expand = c(0, 0)) +
    coord_cartesian(xlim = c(months[1] - 1 / 24, max(months) + 1 / 24),
      ylim = c(length(groups) + 0.5, 0.5)) +
    labs(x = NULL, y = "ATC group") +
    theme(
      panel.grid.major = element_blank(),
      legend.position = "top",
      legend.justification = "right",
      legend.text = ggtext::element_markdown(size = 14),
      legend.margin = margin(0, 0, 0, 0)
    )
}

# PNG of the empty heat map and an MP4 filling it in month by month (one
# month per frame, holding the end for a second), with its poster (the last
# frame). Frames are drawn from grobs aligned with the PNG, as in
# pbs_roll_files().
pbs_heatmap_files <- function(pbs_scores, dir, fps = 24, width = 9, height = 4.2,
                              res = 400, video_width = 1800,
                              background = "#fbf9f4") {
  register_katex_font()
  n <- length(unique(pbs_scores$Month))
  plots <- lapply(0:n, function(m) pbs_heatmap_plot(pbs_scores, m))
  tmp <- tempfile(fileext = ".png")
  ragg::agg_png(tmp, width = width, height = height, units = "in", res = res)
  grobs <- with_slide_style(align_ggplots(plots))
  grDevices::dev.off()
  unlink(tmp)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  png <- save_png(grobs[[1]], file.path(dir, "fig_pbs_heatmap.png"),
    width = width, height = height, res = res)
  frames_dir <- tempfile("frames")
  dir.create(frames_dir)
  drawn <- file.path(frames_dir, sprintf("step%03d.png", seq_along(grobs)))
  for (i in seq_along(grobs)) {
    ragg::agg_png(drawn[i], width = width, height = height, units = "in",
      res = res, background = background)
    grid::grid.draw(grobs[[i]])
    grDevices::dev.off()
  }
  frames <- rep(drawn, c(rep(1, length(drawn) - 1), fps))
  video <- file.path(dir, "fig_pbs_heatmap.mp4")
  poster <- file.path(dir, "fig_pbs_heatmap_end.png")
  file.copy(drawn[length(drawn)], poster, overwrite = TRUE)
  optimise_png(poster)
  av::av_encode_video(frames, video, framerate = fps,
    vfilter = sprintf("scale=%d:-2", video_width), verbose = FALSE)
  unlink(frames_dir, recursive = TRUE)
  c(png, video, poster)
}

# Surveillance across series: number of flagged series each month, and the
# probability of at least that many under a Binomial with pi estimated from
# all previous months
compute_pbs_joint <- function(pbs_scores, threshold = 0.001) {
  pbs_scores |>
    as_tibble() |>
    filter(!is.na(prob)) |>
    group_by(Month) |>
    summarise(M = n(), n = sum(prob < threshold), .groups = "drop") |>
    mutate(
      pi = (cumsum(n) - n) / (cumsum(M) - M),
      prob = pbinom(n - 1, M, pi, lower.tail = FALSE)
    ) |>
    filter(row_number() > 12)
}

pbs_joint_plot <- function(pbs_joint, threshold = 0.001) {
  pbs_joint |>
    ggplot(aes(x = Month, y = n)) +
    geom_col(aes(fill = prob < threshold), show.legend = FALSE) +
    scale_fill_manual(values = c(`FALSE` = "grey35", `TRUE` = "#C14b14")) +
    scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
    labs(y = "Number of flagged ATC groups")
}

# Interactive version of pbs_joint_plot(): each significant month has an
# invisible band `halfwidth` months either side of its bar (which is too narrow
# to hover over easily), with the month as data_id for pbs_joint_card().
# Hovering over a band makes its bar glow. ggiraph styles every element with
# the hovered data_id alike, so the glow is only a drop-shadow filter, which
# leaves the unfilled band invisible.
pbs_joint_girafe <- function(
  pbs_joint,
  threshold = 0.001,
  width = 9,
  height = 4.4,
  halfwidth = 5,
  glow_css = "filter:drop-shadow(0 0 3px #e0601a);"
) {
  flagged <- pbs_joint |>
    filter(prob < threshold) |>
    mutate(month = format(Month))
  p <- pbs_joint_plot(pbs_joint, threshold) +
    # The significant bars again, now interactive so they can glow
    ggiraph::geom_col_interactive(
      data = flagged,
      aes(data_id = month),
      fill = "#C14b14",
      # Same bar width as the full series, not the sparse flagged months
      width = 0.9 * resolution(as.numeric(as.Date(pbs_joint$Month)), zero = FALSE)
    ) +
    ggiraph::geom_rect_interactive(
      data = flagged,
      aes(
        xmin = Month - halfwidth, xmax = Month + halfwidth,
        ymin = 0, ymax = max(pbs_joint$n),
        data_id = month
      ),
      inherit.aes = FALSE,
      fill = NA,
      colour = NA
    )
  ggiraph::girafe(
    ggobj = p,
    font_set = girafe_fonts(),
    width_svg = width,
    height_svg = height,
    options = list(
      ggiraph::opts_hover(css = glow_css),
      ggiraph::opts_sizing(rescale = TRUE, width = 1),
      ggiraph::opts_toolbar(saveaspng = FALSE, hidden = c("selection", "zoom", "misc"))
    )
  )
}

# Card for pbs_joint_girafe() showing n_t, P(N_t >= n_t) and `explanations`
# (a named vector keyed by month, e.g. "1997 Dec") for the hovered month. It
# sits in the empty top-left of the plot.
pbs_joint_card <- function(pbs_joint, explanations, threshold = 0.001) {
  # The slides' CSS reset neutralises <i>, <sub> and <sup>, so style them inline
  sub <- function(x) sprintf("<span style='font-size:70%%; vertical-align:sub'>%s</span>", x)
  sup <- function(x) sprintf("<span style='font-size:70%%; vertical-align:super'>%s</span>", x)
  it <- function(x) sprintf("<span style='font-style:italic'>%s</span>", x)
  # Probability as "a × 10^b"
  sci <- function(x) {
    e <- floor(log10(x))
    paste0(sprintf("%.1f &times; 10", x / 10^e), sup(paste0("&minus;", -e)))
  }
  flagged <- pbs_joint |>
    filter(prob < threshold) |>
    mutate(
      month = format(Month),
      label = paste0(
        it("n"), sub("t"), " = ", n, "<br>",
        "P(", it("N"), sub("t"), " &ge; ", it("n"), sub("t"), ") = ", sci(prob), "<br>",
        explanations[month]
      )
    )
  hover_card(
    stats::setNames(flagged$label, flagged$month),
    sep = "<br>",
    class = "plot-top-left"
  )
}
