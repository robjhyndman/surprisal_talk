# Time series examples, following the "Time series data" chapter of
# That's Weird (OTexts.com/weird/timeseries.html)

# Three anomaly detection paradigms
create_fig_tsparadigms <- function(fr_mortality) {
  a12 <- tsibbledata::PBS |>
    group_by(ATC2) |>
    summarise(Scripts = sum(Scripts) / 1e3) |>
    ungroup() |>
    filter(ATC2 == "A12", Month <= yearmonth("2006 Feb"))
  p1 <- fr_mortality |>
    filter(Age == 25, Sex == "Male") |>
    ggplot(aes(x = Year, y = Mortality)) +
    geom_line() +
    theme(axis.title = element_blank()) +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL)
  p2 <- fr_mortality |>
    filter(Age %in% c(2:6, 60), Sex == "Female") |>
    ggplot(aes(x = Year, y = Mortality, colour = as.factor(Age))) +
    geom_line() +
    guides(col = "none") +
    theme(axis.title = element_blank()) +
    scale_x_continuous(breaks = NULL) +
    scale_y_log10(breaks = NULL)
  p3 <- a12 |>
    autoplot(Scripts) +
    geom_point(data = tail(a12, 1)) +
    theme(axis.title = element_blank()) +
    scale_x_yearmonth(breaks = NULL) +
    scale_y_continuous(breaks = NULL)
  patchwork::wrap_plots(p1, p2, p3, ncol = 1)
}

# Rolling origin diagram
tscv_plot <- function(.init, .step, h = 1) {
  expand.grid(
    time = seq(26),
    .id = seq(trunc(11 / .step))
  ) |>
    group_by(.id) |>
    mutate(
      observation = case_when(
        time <= ((min(.id) - 1) * .step + .init) ~ "train",
        time %in% c((min(.id) - 1) * .step + .init + h) ~ "test",
        TRUE ~ "unused"
      )
    ) |>
    ungroup() |>
    filter(.id <= 26 - .init) |>
    ggplot(aes(x = time, y = .id)) +
    geom_segment(
      aes(x = 0, xend = 27, y = .id, yend = .id),
      arrow = arrow(length = unit(0.015, "npc")),
      col = "black",
      linewidth = 0.25
    ) +
    geom_point(aes(col = observation), size = 2) +
    scale_y_reverse() +
    scale_colour_manual(
      values = c(train = "#65775c", test = "#c14b14", unused = "grey")
    ) +
    guides(col = "none") +
    labs(x = "time", y = "") +
    theme_void() +
    theme(axis.title.x = element_text(margin = margin(t = 2)))
}

# Weird series: features of all PBS series
compute_pbs_features <- function(PBS) {
  PBS |>
    features(Scripts, feature_set(pkgs = "feasts"))
}

# Surveillance: PBS totals by ATC2 group (thousands of scripts)
make_pbs <- function(PBS) {
  PBS |>
    arrange(ATC2, Month) |>
    group_by(ATC2) |>
    summarise(Scripts = sum(Scripts) / 1e3, .groups = "drop") |>
    mutate(t = as.numeric(Month - min(Month) + 1))
}

# Surveillance: rolling origin one-step surprisal probabilities
compute_pbs_scores <- function(pbs) {
  pbs$prob <- NA_real_
  # Loop over all time periods starting with I=36
  for (i in seq(36, max(pbs$t) - 1)) {
    # Fit ETS model to all series with at least 36 observations up to time i
    # (counting from each series' first non-zero value) and compute 1-step forecasts
    pbs_fc <- pbs |>
      filter(t <= i) |>
      group_by_key() |>
      filter(cumsum(Scripts) > 0) |>
      filter(n() >= 36) |>
      ungroup() |>
      model(ets = ETS(Scripts)) |>
      forecast(h = 1) |>
      rename(fcast = Scripts)
    # Calculate surprisal probabilities from observations in time i+1
    pbs_scores <- pbs |>
      filter(t == i + 1) |>
      right_join(pbs_fc, by = c("Month", "ATC2")) |>
      mutate(surprisal_prob = surprisals_prob(Scripts, distribution = fcast)) |>
      select(Month, ATC2, surprisal_prob)
    # Add probabilities to pbs data set
    pbs <- pbs |>
      left_join(pbs_scores, by = c("Month", "ATC2")) |>
      mutate(prob = if_else(!is.na(surprisal_prob), surprisal_prob, prob)) |>
      select(-surprisal_prob)
  }
  return(pbs)
}

# Surveillance anomalies for one ATC2 group
pbs_plot <- function(pbs, anomalies, series) {
  dates <- anomalies |>
    filter(ATC2 == series) |>
    pull(Month) |>
    as.Date()
  pbs |>
    filter(ATC2 == series) |>
    ggplot(aes(x = Month, y = Scripts)) +
    scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y") +
    geom_vline(xintercept = dates, colour = "#C14b14") +
    geom_line() +
    labs(y = "Scripts (thousands)", title = paste("Scripts for ATC group", series))
}


# US weekly mortality by age group, with STL anomalies (prob < threshold) as
# points, restricted to weeks in [from, to]. Anomalies are grouped into
# `episodes` (a tibble with columns start, end [yearweek] and episode [label]),
# shown as shaded bands with opacity `band_alpha`. The same function draws the
# static, animated and interactive (ggiraph) versions, so they match exactly.
us_mortality_plot <- function(
  us_stmf,
  stmf_scores,
  episodes = NULL,
  from = min(us_stmf$Week),
  to = max(us_stmf$Week),
  threshold = 0.02,
  band_alpha = 0,
  interactive = FALSE,
  anomaly_colour = "#c14b14"
) {
  episode_of <- function(week) {
    if (is.null(episodes)) {
      return(rep(NA_character_, length(week)))
    }
    i <- vapply(
      week,
      \(w) {
        k <- which(episodes$start <= w & w <= episodes$end)
        if (length(k) == 0) NA_integer_ else k[1]
      },
      integer(1)
    )
    episodes$episode[i]
  }
  series <- us_stmf |>
    tibble::as_tibble() |>
    filter(Week >= from, Week <= to)
  anomalies <- stmf_scores |>
    tibble::as_tibble() |>
    filter(Week >= from, Week <= to, prob < threshold) |>
    mutate(
      episode = episode_of(Week),
      tooltip = paste0("Week: ", format(Week), "<br>Age: ", `Age group`)
    )
  geom_rect_ <- if (interactive) ggiraph::geom_rect_interactive else geom_rect
  geom_point_ <- if (interactive) ggiraph::geom_point_interactive else geom_point
  p <- ggplot(series, aes(x = as.Date(Week), y = Mortality))
  if (!is.null(episodes) && band_alpha > 0) {
    bands <- episodes |>
      filter(start >= from, end <= to) |>
      mutate(xmin = as.Date(start) - 3, xmax = as.Date(end) + 4)
    p <- p +
      geom_rect_(
        data = bands,
        aes(
          xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
          data_id = episode, tooltip = episode
        ),
        inherit.aes = FALSE,
        fill = anomaly_colour,
        alpha = band_alpha
      )
  }
  p +
    geom_line(aes(colour = `Age group`)) +
    geom_point_(
      data = anomalies,
      aes(data_id = episode, tooltip = tooltip),
      colour = anomaly_colour,
      size = 2
    ) +
    facet_grid(`Age group` ~ ., scales = "free_y") +
    guides(colour = "none") +
    labs(x = NULL, y = "Mortality")
}

# Interactive version of us_mortality_plot(): hovering over an anomaly or an
# episode's band highlights the whole episode in every age group
us_mortality_girafe <- function(
  ...,
  width = 9,
  height = 4.25,
  tooltip_css = paste(
    "background:#0f2a44; color:white; padding:6px 10px; border-radius:6px;",
    "font-family:'Fira Sans', sans-serif; font-size:20px;"
  )
) {
  ggiraph::girafe(
    ggobj = us_mortality_plot(..., interactive = TRUE),
    width_svg = width,
    height_svg = height,
    options = list(
      ggiraph::opts_tooltip(css = tooltip_css, use_fill = FALSE),
      ggiraph::opts_hover(css = "stroke:#0f2a44; stroke-width:1.5px;"),
      ggiraph::opts_hover_inv(css = "opacity:0.25;"),
      ggiraph::opts_sizing(rescale = TRUE, width = 1),
      ggiraph::opts_toolbar(saveaspng = FALSE, hidden = c("selection", "zoom", "misc"))
    )
  )
}

# MP4 of us_mortality_plot() zooming from the full period into [from, to]:
# the window shrinks with an ease-in-out, each panel's y-axis follows the data
# in view, and the episode bands fade in at the end. The first and last frames
# match us_mortality_plot() for the full period and for [from, to].
us_mortality_zoom_video <- function(
  us_stmf,
  stmf_scores,
  episodes,
  from,
  to,
  file = tempfile(fileext = ".mp4"),
  seconds = 2,
  fps = 30,
  band_alpha = 0.08,
  width = 9,
  height = 4.25,
  res = 200,
  background = "#fbf9f4"
) {
  n <- seconds * fps
  start <- as.numeric(as.Date(min(us_stmf$Week)))
  end <- as.numeric(as.Date(max(us_stmf$Week)))
  ease <- (1 - cos(pi * seq(0, 1, length.out = n))) / 2
  lo <- start + ease * (as.numeric(as.Date(from)) - start)
  hi <- end + ease * (as.numeric(as.Date(to)) - end)
  fade <- pmax(0, (seq_len(n) - 0.75 * n) / (0.25 * n))
  dir <- tempfile("frames")
  dir.create(dir)
  frames <- file.path(dir, sprintf("frame%03d.png", seq_len(n)))
  for (i in seq_len(n)) {
    week_lo <- if (i == n) from else tsibble::yearweek(as.Date(lo[i], origin = "1970-01-01"))
    week_hi <- if (i == n) to else tsibble::yearweek(as.Date(hi[i], origin = "1970-01-01"))
    p <- us_mortality_plot(
      us_stmf, stmf_scores, episodes,
      from = week_lo, to = week_hi,
      band_alpha = band_alpha * fade[i]
    )
    ragg::agg_png(frames[i], width = width, height = height, units = "in",
      res = res, background = background)
    print(p)
    grDevices::dev.off()
  }
  av::av_encode_video(frames, file, framerate = fps, verbose = FALSE)
  unlink(dir, recursive = TRUE)
  file
}
