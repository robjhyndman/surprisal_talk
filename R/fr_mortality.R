library(weird)

# Replacement for weird::hampel_anomalies to allow approximations to be used.
hampel_anomalies <- function(
  y,
  bandwidth,
  alpha = 0.01,
  approximation = c("none", "gpd", "empirical")
) {
  approximation = match.arg(approximation)
  if (abs(bandwidth - round(bandwidth)) > 1e-8) {
    stop("Bandwidth must be an integer")
  }
  bandwidth <- as.integer(round(bandwidth))
  n <- length(y)
  # Running medians
  m <- stats::runmed(
    y,
    2 * bandwidth + 1,
    endrule = "keep",
    na.action = "na.omit"
  )
  # Set MAD to Inf so end points are not considered anomalies
  mad <- rep(Inf, n)
  # Running MADs
  for (i in (bandwidth + 1):(n - bandwidth)) {
    mad[i] <- stats::median(
      abs(y[(i - bandwidth):(i + bandwidth)] - m[i]),
      na.rm = TRUE
    )
  }
  # Find anomalies
  s <- -dnorm(abs((y - m) / mad) * qnorm(0.75), log = TRUE)
  p <- weird:::surprisal_prob(
    s,
    distribution = dist_normal(),
    approximation = approximation
  )
  return(p < alpha)
}

create_fr_mortality_plot <- function(
  fr_mortality,
  type = c("functional", "timeseries")
) {
  type <- match.arg(type)
  set_ggplot_options()
  if (type == "functional") {
    fr_mortality |>
      ggplot(aes(x = Year, y = Mortality, color = Age, group = Age)) +
      geom_line() +
      facet_grid(. ~ Sex) +
      scale_y_log10(labels = scales::comma) +
      scale_color_continuous(palette = rainbow(20)[1:17])
  } else {
    fr_mortality |>
      ggplot(aes(x = Age, y = Mortality, color = Year, group = Year)) +
      geom_line() +
      facet_grid(. ~ Sex) +
      scale_y_log10(labels = scales::comma) +
      scale_color_continuous(palette = rainbow(20)[1:17])
  }
}

find_fr_anomalies <- function(fr_mortality) {
  fr_anomalies <- fr_mortality |>
    group_by(Age, Sex) |>
    mutate(
      hampel = hampel_anomalies(
        Mortality,
        bandwidth = 7,
        alpha = 0.01,
        approximation = "gpd"
      )
    ) |>
    ungroup() |>
    filter(hampel) |>
    arrange(Year, Age)
  # Keep only those years where there is at least 2 age groups
  yrs <- fr_anomalies |>
    group_by(Year, Sex) |>
    mutate(n = n()) |>
    ungroup() |>
    filter(n >= 3) |>
    select(Year, Sex) |>
    distinct()
  fr_anomalies |> right_join(yrs, by = c("Year", "Sex"))
}

create_fr_anomaly_plot <- function(fr_anomalies) {
  set_ggplot_options()

  yrs <- fr_anomalies |>
    select(Year, Sex) |>
    distinct()

  fr_anomalies_plot_male2 <- fr_anomalies |>
    filter(Sex == "Male") |>
    ggplot(aes(x = Year, y = Age)) +
    facet_grid(. ~ Sex) +
    scale_x_continuous(
      breaks = seq(1820, 2000, by = 20),
      limits = range(yrs$Year)
    ) +
    geom_vline(
      xintercept = unique(yrs$Year[yrs$Sex == "Male"]),
      alpha = 0.5,
      color = "grey"
    ) +
    geom_point(col = "#478cb2") +
    ggrepel::geom_text_repel(
      data = yrs |>
        filter(Sex == "Male", !Year %in% 1915:1918),
      aes(y = 75, label = Year),
      col = "#478cb2",
      size = 3,
      seed = 1967
    ) +
    ylim(0, 86)
  fr_anomalies_plot_female2 <- fr_anomalies |>
    filter(Sex == "Female") |>
    ggplot(aes(x = Year, y = Age)) +
    facet_grid(. ~ Sex) +
    scale_x_continuous(
      breaks = seq(1820, 2000, by = 20),
      limits = range(yrs$Year)
    ) +
    geom_vline(
      xintercept = unique(yrs$Year[yrs$Sex == "Female"]),
      alpha = 0.5,
      color = "grey"
    ) +
    labs(title = "French mortality anomalies") +
    geom_point(col = "#c1653a") +
    ggrepel::geom_text_repel(
      data = yrs[yrs$Sex == "Female", ],
      aes(y = 75, label = Year),
      col = "#c1653a",
      size = 3,
      seed = 1967
    ) +
    ylim(0, 86)
  patchwork::wrap_plots(
    fr_anomalies_plot_female2,
    fr_anomalies_plot_male2,
    nrow = 1
  )
}
