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

# Surprisal probability figures (fig_sp0, fig_sp1, fig_sp2) with maths in KaTeX fonts
make_fig_sp_katex <- function(figs) {
  lapply(figs, function(p) {
    # Once y_i is shown, these figures redraw the vertical grid lines in white
    # (for a white background); drop them so there are no vertical grid lines
    white <- vapply(
      p$layers,
      \(l) identical(l$aes_params$colour, "white"),
      logical(1)
    )
    p$layers <- p$layers[!white]
    katex_fig_sp(p)
  })
}

# US weekly mortality rates by age group
make_us_stmf <- function(us_mortality) {
  us_mortality |>
    filter(Sex == "Total", Age != "Total") |>
    mutate(Week = yearweek(paste0(Year, "-W", Week))) |>
    select(Week, `Age group` = Age, Mortality) |>
    as_tsibble(index = Week, key = `Age group`)
}

# Surprisal probabilities of the standardised STL remainders
compute_stmf_scores <- function(us_stmf) {
  us_stmf |>
    model(stl = STL(Mortality)) |>
    augment() |>
    group_by(`Age group`) |>
    mutate(
      a = robustbase::s_Qn(.resid),
      e = .resid / a
    ) |>
    ungroup() |>
    mutate(prob = surprisals_prob(e, "gpd", distribution = dist_normal())) |>
    arrange(prob)
}

# Episodes of anomalous weeks (from the STL surprisals) and their causes
make_us_episodes <- function() {
  tibble::tribble(
    ~start, ~end, ~episode, ~cause,
    "2020 W03", "2020 W04", "2020 W03–W04",
    "artefact (75+): STL trend pulled up by the COVID-19 peak a few weeks later",
    "2020 W14", "2020 W18", "2020 W14–W18",
    "first wave (New York, April 2020): all adult age groups",
    "2020 W49", "2021 W03", "2020 W49–2021 W03",
    "winter 2020–21, the deadliest wave: 65+ only",
    "2021 W13", "2021 W15", "2021 W13–W15",
    "low mortality (75+): restricted movement, vaccination, mortality displacement",
    "2021 W33", "2021 W39", "2021 W33–W39",
    "Delta: only 15–64 and 65–74 (older people vaccinated)",
    "2022 W02", "2022 W05", "2022 W02–W05",
    "Omicron: mostly 65+"
  ) |>
    mutate(start = yearweek(start), end = yearweek(end))
}

# Weird series: first two principal components of the scaled PBS features,
# with LOO KDE surprisal probabilities
compute_pbs_pca <- function(pbs_features) {
  feat <- pbs_features |> na.omit()
  pca <- feat |>
    select(-Concession, -Type, -ATC1, -ATC2) |>
    prcomp(scale = TRUE, rank = 2) |>
    broom::augment(feat)
  pca |>
    mutate(
      prob = surprisals_prob(
        pca |> select(.fittedPC1, .fittedPC2),
        loo = TRUE,
        approximation = "gpd"
      )
    )
}

# PBS series matching the keys in `series`, one panel each
pbs_series_plot <- function(PBS, series, title) {
  PBS |>
    semi_join(series, by = c("Concession", "Type", "ATC2")) |>
    autoplot(Scripts) +
    facet_grid(vars(Concession, Type, ATC2), scales = "free_y") +
    labs(title = title) +
    guides(colour = "none")
}

# Surveillance: one-step ETS forecasts from the first `i` months, for series
# with at least 36 observations from their first non-zero value
pbs_forecast <- function(pbs, i = 36) {
  pbs |>
    filter(t <= i) |>
    group_by_key() |>
    filter(cumsum(Scripts) > 0, n() >= 36) |>
    ungroup() |>
    model(ets = ETS(Scripts)) |>
    forecast(h = 1)
}

# Surprisal probabilities of the observations at time i + 1 under the
# forecasts `pbs_fc` made at time i
pbs_forecast_probs <- function(pbs_fc, pbs, i = 36) {
  pbs_fc |>
    rename(fcast = Scripts) |>
    left_join(pbs |> filter(t == i + 1), by = c("Month", "ATC2")) |>
    mutate(prob = surprisals_prob(Scripts, distribution = fcast)) |>
    as_tibble() |>
    select(ATC2, Scripts, fcast, prob) |>
    arrange(prob)
}

# Forecast of one series at time i, with the observation at time i + 1
pbs_forecast_plot <- function(pbs_fc, pbs, series = "S01", i = 36) {
  obs <- pbs |> filter(ATC2 == series, t <= i + 1)
  pbs_fc |>
    filter(ATC2 == series) |>
    autoplot(obs) +
    geom_point(data = obs |> filter(t == i + 1), aes(x = Month, y = Scripts)) +
    labs(
      y = "Scripts (thousands)",
      title = paste0(
        "Forecast of ", series, " scripts: ", format(max(obs$Month), format = "%b %Y")
      )
    ) +
    theme(legend.position = "none")
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

# Months with a significant number of flagged series, formatted for a table
pbs_joint_table <- function(pbs_joint, threshold = 0.001) {
  pbs_joint |>
    filter(prob < threshold) |>
    mutate(
      Month = format(Month),
      pi = sprintf("%.4f", pi),
      prob = sprintf("%.1e", prob)
    ) |>
    rename(
      `$M_t$` = M,
      `$n_t$` = n,
      `$\\hat\\pi_t$` = pi,
      `$\\mathbb{P}(N_t \\ge n_t)$` = prob
    )
}
