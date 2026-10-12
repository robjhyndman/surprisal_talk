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
  if (type == "timeseries") {
    p <- fr_mortality |>
      ggplot(aes(
        x = Year,
        y = Mortality,
        color = Age,
        group = factor(Age, levels = sort(unique(Age), decreasing = TRUE))
      ))
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
  # Keep only those year-sex combinations where there is at least 3 age groups
  yrs <- fr_anomalies |>
    count(Year, Sex) |>
    filter(n >= 3)
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
      data = yrs[yrs$Sex == "Male", ],
      #yrs |> filter(Sex == "Male", !Year %in% 1915:1918),
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

# Interactive version of the anomaly plot: hovering over a point shows its year
# and age, and highlights all anomalies from that year
create_fr_anomaly_girafe <- function(
  fr_anomalies,
  colours = c(Female = "#c14b14", Male = "#1d6fa5"),
  tooltip_css = paste(
    "background:#0f2a44; color:white; padding:6px 10px; border-radius:6px;",
    "font-family:'Fira Sans', sans-serif; font-size:20px;"
  ),
  width = 9,
  height = 4.25
) {
  yrs <- fr_anomalies |>
    distinct(Year, Sex)
  p <- fr_anomalies |>
    mutate(tooltip = paste0("Year: ", Year, "<br>Age: ", Age)) |>
    ggplot(aes(x = Year, y = Age)) +
    facet_grid(. ~ Sex) +
    ggiraph::geom_vline_interactive(
      data = yrs,
      aes(xintercept = Year, data_id = Year, tooltip = Year),
      colour = "grey70",
      alpha = 0.5
    ) +
    ggiraph::geom_point_interactive(
      aes(colour = Sex, data_id = Year, tooltip = tooltip),
      size = 2
    ) +
    scale_colour_manual(values = colours, guide = "none") +
    scale_x_continuous(breaks = seq(1820, 2000, by = 20)) +
    ylim(0, 86)
  ggiraph::girafe(
    ggobj = p,
    width_svg = width,
    height_svg = height,
    options = list(
      ggiraph::opts_tooltip(css = tooltip_css, use_fill = FALSE),
      ggiraph::opts_hover(css = "stroke:#0f2a44; stroke-width:1.5px;"),
      ggiraph::opts_hover_inv(css = "opacity:0.2;"),
      ggiraph::opts_sizing(rescale = TRUE, width = 1),
      ggiraph::opts_toolbar(saveaspng = FALSE, hidden = c("selection", "zoom", "misc"))
    )
  )
}

# Card that shows a label for the year under the mouse in an interactive
# (ggiraph) plot on the same slide. `labels` is a named vector keyed by year;
# labels equal to `unknown` are shown in the alert style.
year_card <- function(labels, unknown = NULL) {
  htmltools::tagList(
    htmltools::div(
      class = "year-card",
      `data-labels` = jsonlite::toJSON(as.list(labels), auto_unbox = TRUE),
      `data-unknown` = unknown
    ),
    htmltools::tags$script(htmltools::HTML(
      "
document.addEventListener('mouseover', function (e) {
  var item = e.target.closest('.girafe [data-id]');
  var section = e.target.closest('section');
  var card = section && section.querySelector('.year-card');
  if (!card) return;
  if (!item) { card.classList.remove('show'); return; }
  var year = item.getAttribute('data-id');
  var label = JSON.parse(card.dataset.labels)[year];
  if (!label) { card.classList.remove('show'); return; }
  card.innerHTML = '<strong>' + year + ':</strong> ' + label;
  card.classList.toggle('unknown', label === card.dataset.unknown);
  card.classList.add('show');
});"
    ))
  )
}
