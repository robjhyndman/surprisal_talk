library(weird)

create_fr_mortality_plot <- function(
  fr_mortality,
  type = c("functional", "timeseries"),
  fr_anomalies = NULL
) {
  type <- match.arg(type)
  alpha <- 0.4 + 0.6 * is.null(fr_anomalies)
  if (!is.null(fr_anomalies)) {
    fr_mortality <- fr_mortality |>
      left_join(
        fr_anomalies |> select(Year, Age, Sex, hampel),
        by = c("Year", "Age", "Sex")
      )
  }
  if (type == "functional") {
    p <- fr_mortality |>
      ggplot(aes(x = Year, y = Mortality, color = Age, group = Age))
  } else {
    p <- fr_mortality |>
      ggplot(aes(x = Age, y = Mortality, color = Year, group = Year))
  }
  p <- p +
    geom_line(alpha = alpha) +
    facet_grid(. ~ Sex) +
    scale_y_log10(labels = scales::comma) +
    scale_color_continuous(palette = rainbow(20)[1:17])
  if (!is.null(fr_anomalies)) {
    p <- p +
      geom_point(
        size = 0.3,
        shape = 1,
        col = "black",
        data = fr_mortality |> filter(hampel)
      )
  }
  p
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
