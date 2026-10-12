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
