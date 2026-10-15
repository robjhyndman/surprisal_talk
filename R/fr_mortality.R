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
    font_set = uconn_girafe_fonts(),
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


# Illustration of the Hampel identifier for one series, built up in stages:
#   "series":     the series alone
#   "window":     the window of 2w+1 observations centred on `centre`, and the
#                 moving median traced from the first full window up to `centre`
#   "difference": the observation y_t and its difference from m_t (a segment)
#   "normal":     the implied Normal distribution N(m_t, sigma_t^2) at time t
# All stages have the same axes, so they overlay exactly on the slides.
create_hampel_fig <- function(
  fr_mortality,
  stage = c("normal", "difference", "window", "series"),
  centre = t,
  age = 50,
  sex = "Female",
  t = 1849,
  w = 7,
  years = c(1816, 1872),
  colours = c(series = "#1e293b", median = "#1d6fa5", normal = "#c14b14")
) {
  stage <- match.arg(stage)
  level <- match(stage, c("series", "window", "difference", "normal"))
  df <- fr_mortality |>
    filter(Age == age, Sex == sex) |>
    arrange(Year) |>
    mutate(m = stats::runmed(Mortality, 2 * w + 1, endrule = "keep"))
  window <- df |> filter(Year >= t - w, Year <= t + w)
  m_t <- df$m[df$Year == t]
  y_t <- df$Mortality[df$Year == t]
  sigma_t <- stats::median(abs(window$Mortality - m_t)) / 0.6745
  # Normal density drawn sideways to the right of t, scaled to `width` years
  width <- 5
  yy <- seq(m_t - 4 * sigma_t, m_t + 4 * sigma_t, length.out = 201)
  normal <- tibble(
    y = yy,
    x = t + width * stats::dnorm(yy, m_t, sigma_t) / stats::dnorm(0, 0, sigma_t)
  )
  df <- df |> filter(Year >= years[1], Year <= years[2])
  # Moving median from the centre of the first full window up to the window centre
  m_at <- function(x) stats::approx(df$Year, df$m, x)$y
  start <- years[1] + w
  traced <- tibble(Year = c(df$Year[df$Year >= start & df$Year < centre], centre)) |>
    mutate(m = m_at(Year))
  # Visible x range (ggplot's default 5% expansion), to clip the window to
  xrange <- years + c(-1, 1) * 0.05 * diff(years)
  xmin <- max(centre - w - 0.5, xrange[1])
  xmax <- min(centre + w + 0.5, xrange[2])
  register_katex_font()
  # Maths labels in KaTeX fonts, to match the maths on the slides
  math_label <- function(label, ...) {
    annotate("text", label = label, parse = TRUE, family = "KaTeX", ...)
  }
  p <- ggplot(df, aes(x = Year, y = Mortality)) +
    # Extent of the final stage, so every stage has the same axes
    geom_blank(data = tibble(
      Year = range(df$Year, normal$x),
      Mortality = range(df$Mortality, df$m, yy)
    ))
  if (level >= 2) {
    p <- p +
      annotate(
        "rect",
        xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
        fill = colours[["median"]], alpha = 0.1
      )
  }
  p <- p + geom_line(colour = colours[["series"]])
  if (level >= 2) {
    p <- p +
      geom_line(data = traced, aes(y = m), colour = colours[["median"]],
        linetype = "dashed") +
      annotate("point", x = centre, y = m_at(centre), colour = colours[["median"]],
        size = 2) +
      # Label fixed at the start of the median line, so it stays still in the video
      math_label("italic(m)[italic(t)]", x = start, y = m_at(start),
        vjust = -0.5, colour = colours[["median"]], size = 7) +
      # Words in the slide font, maths in KaTeX
      ggtext::geom_richtext(
        data = data.frame(x = (xmin + xmax) / 2, y = Inf),
        aes(x = x, y = y),
        label = paste0(
          "window of ", katex_up("2"), katex_it("w"), katex_up(" + 1"), " years"
        ),
        vjust = 1.2, colour = colours[["median"]], size = 4.5,
        fill = NA, label.colour = NA, inherit.aes = FALSE
      )
  }
  if (level >= 4) {
    p <- p +
      geom_polygon(
        data = normal, aes(x = x, y = y),
        fill = colours[["normal"]], alpha = 0.3, colour = colours[["normal"]]
      ) +
      annotate("segment", x = t, xend = t, y = min(yy), yend = max(yy),
        colour = colours[["normal"]]) +
      math_label(
        "N(italic(m)[italic(t)]*','~italic('σ')[italic(t)]^2)",
        x = t + 0.6, y = m_t - 3.2 * sigma_t,
        hjust = 0, colour = colours[["normal"]], size = 6.5
      )
  }
  if (level >= 3) {
    p <- p +
      annotate("segment", x = t, xend = t, y = m_t, yend = y_t,
        colour = colours[["normal"]], linewidth = 1.2) +
      annotate("point", x = t, y = m_t, colour = colours[["median"]], size = 2) +
      annotate("point", x = t, y = y_t, colour = colours[["normal"]], size = 3) +
      math_label("italic(y)[italic(t)]", x = t - 0.4, y = y_t,
        hjust = 1, colour = colours[["normal"]], size = 7)
  }
  p +
    scale_y_continuous(expand = expansion(mult = c(0.08, 0.2))) +
    coord_cartesian(xlim = years, clip = "off") +
    labs(x = "Year", y = "Mortality", title = paste0(sex, ", age ", age)) +
    theme(plot.title = element_text(face = "plain"))
}

# MP4 of the "window" stage of create_hampel_fig(), with the window sliding
# from `from` to `to` and tracing out the moving median. The first and last
# frames match create_hampel_fig(stage = "window") with centre = from and to.
# The default `from` puts the first full window of 2w+1 years at the left edge.
hampel_window_video <- function(
  fr_mortality,
  file,
  from = 1823,
  to = 1849,
  seconds = 3,
  fps = 30,
  width = 9,
  height = 2.6,
  res = 200,
  background = "#fbf9f4"
) {
  n <- seconds * fps
  ease <- (1 - cos(pi * seq(0, 1, length.out = n))) / 2
  centres <- from + ease * (to - from)
  dir <- tempfile("frames")
  dir.create(dir)
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  frames <- file.path(dir, sprintf("frame%03d.png", seq_len(n)))
  for (i in seq_len(n)) {
    p <- create_hampel_fig(fr_mortality, stage = "window", centre = centres[i], t = to)
    ragg::agg_png(frames[i], width = width, height = height, units = "in",
      res = res, background = background)
    print(p)
    grDevices::dev.off()
  }
  av::av_encode_video(frames, file, framerate = fps, verbose = FALSE)
  unlink(dir, recursive = TRUE)
  file
}
