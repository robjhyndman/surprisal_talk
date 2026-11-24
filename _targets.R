library(targets)
library(tarchetypes)

# Targets for the current talk only. Past talks are frozen as git tags
# (`git tag -l`): check one out to rebuild it with its own pipeline.

tar_option_set(
  packages = c(
    "tibble",
    "weird",
    "dplyr",
    "ggplot2",
    "tsibble",
    "fable",
    "feasts"
  )
)

tar_source()

# Target saving figure target `fig` as a PNG for the slides, in figs/
tar_png <- function(fig, width = 9, height = 4.4) {
  fig <- deparse(substitute(fig))
  tar_target_raw(
    paste0(fig, "_png"),
    substitute(
      save_png(FIG, FILE, width = WIDTH, height = HEIGHT),
      list(
        FIG = as.symbol(fig),
        FILE = file.path("figs", paste0(fig, ".png")),
        WIDTH = width,
        HEIGHT = height
      )
    ),
    format = "file",
    packages = c(tar_option_get("packages"), "ggtext", "patchwork")
  )
}

list(
  # Startup message of library(weird), which shows package versions: rebuilt
  # whenever the package library changes (targets doesn't track packages)
  tar_target(uvr_lock, "uvr.lock", format = "file"),
  tar_target(weird_attach, weird_attach_message(uvr_lock)),

  # What is an anomaly? ----------------------------------
  tar_target(chisq_data1, {
    set.seed(1)
    tibble(x = c(rchisq(100, df = 5)))
  }),
  tar_target(chisq_data2, {
    set.seed(2)
    tibble(x = c(rchisq(100, df = 5)))
  }),
  tar_target(chisq_data3, {
    set.seed(3)
    tibble(x = c(rchisq(100, df = 5)))
  }),
  # Three chi-square samples, each as a strip
  tar_target(
    fig_chisq_pngs,
    unname(mapply(
      function(data, k) {
        save_png(
          chisq_strip(data),
          file.path("figs", paste0("fig_chisq", k, ".png")),
          width = 9,
          height = 1.15
        )
      },
      list(chisq_data3, chisq_data1, chisq_data2),
      1:3
    )),
    format = "file"
  ),
  tar_target(
    spot_anomaly_data,
    make_spot_anomaly_data()
  ),
  # Spot the anomaly: two versions, with the anomaly in each circled on the
  # slide by a hand-drawn (rough notation) circle round spot_boxes
  tar_target(
    fig_spot_pngs,
    vapply(
      c("a", "b"),
      function(version) {
        save_png(
          spot_anomaly_plot(spot_anomaly_data, version),
          file.path("figs", paste0("fig_spot_", version, ".png")),
          width = 4.6,
          height = 4.6
        )
      },
      character(1),
      USE.NAMES = FALSE
    ),
    format = "file"
  ),
  tar_target(
    spot_boxes,
    lapply(c(a = "a", b = "b"), \(version) spot_anomaly_box(spot_anomaly_data, version))
  ),

  # Surprisal probabilities ------------------------------
  tar_target(
    fig_sp0,
    create_fig_surprisal_prob(step = 0L)
  ),
  tar_target(
    fig_sp1,
    create_fig_surprisal_prob(step = 1L)
  ),
  tar_target(
    fig_sp2,
    create_fig_surprisal_prob(step = 2L)
  ),
  # The surprisal probability steps, with the same panel position in every
  # step so they overlay exactly
  tar_target(fig_sp_png,
    save_aligned_pngs(
      make_fig_sp_katex(list(fig_sp0, fig_sp1, fig_sp2), sub = "i"),
      file.path("figs", paste0("fig_sp", 1:3, ".png")),
      height = 3.6
    ),
    format = "file",
    packages = c(tar_option_get("packages"), "ggtext")
  ),
  tar_target(normal_eg_steps, normal_example_steps()),
  tar_target(fig_sp_normal_png,
    save_png(
      make_fig_sp_katex(
        list(create_fig_surprisal_prob(2L, den = dist_normal(), size = 98)),
        sub = "i"
      )[[1]],
      file.path("figs", "fig_sp_normal.png"),
      width = 6.2,
      height = 3.85
    ),
    format = "file",
    packages = c(tar_option_get("packages"), "ggtext")
  ),

  # Wine quality and prices ------------------------------
  tar_target(
    wine_reviews,
    weird::fetch_wine_reviews() |>
      filter(variety %in% c("Shiraz", "Syrah")) |>
      select(-variety, -region)
  ),
  tar_target(wine_p, wine_probs(wine_reviews)),
  tar_target(wine_p_gpd, wine_probs(wine_reviews, approximation = "gpd")),
  tar_target(fig_wine_scatter, wine_scatter(wine_reviews)),
  tar_target(fig_wine_scatter_log, wine_scatter(wine_reviews, log = TRUE)),
  tar_target(fig_wine_model_scatter, wine_model_scatter(wine_reviews)),
  tar_png(fig_wine_scatter),
  tar_png(fig_wine_scatter_log),
  tar_png(fig_wine_model_scatter),
  tar_target(
    wine_model_log_girafe,
    with_slide_style(wine_anomaly_girafe(wine_p))
  ),
  tar_target(
    wine_model_girafe,
    with_slide_style(wine_anomaly_girafe(wine_p, log = FALSE))
  ),
  tar_target(wine_anomaly_card_html, wine_anomaly_card(wine_p)),

  # Approximate extreme surprisal probabilities ----------
  # Stages of the GPD approximation to the surprisal probabilities, with a
  # video zooming into the tail between stages 2 and 3
  tar_target(
    fig_gpd_data,
    gpd_stage_data()
  ),
  tar_target(
    fig_gpd_files,
    gpd_stage_files(fig_gpd_data, "figs", sub = "i"),
    format = "file",
    packages = c(tar_option_get("packages"), "ggtext")
  ),

  # Old Faithful eruptions -------------------------------
  tar_target(
    oldfaithful,
    weird::oldfaithful
  ),
  tar_target(of_probs, old_faithful_probs(oldfaithful)),
  # The data after each step of the code reveal on the slide
  tar_target(of_probs_steps, list(of_probs, arrange(of_probs, p))),
  tar_target(fig_of_scatter, old_faithful_scatter(oldfaithful)),
  tar_target(fig_of_kde, old_faithful_kde(oldfaithful)),
  tar_target(
    of_kde_anomalies_girafe,
    with_slide_style(old_faithful_girafe(oldfaithful, of_probs))
  ),
  tar_target(of_anomaly_card_html, old_faithful_card(of_probs)),
  tar_target(fig_of_hdrboxplot, old_faithful_hdrboxplot(oldfaithful)),
  tar_png(fig_of_scatter),
  tar_png(fig_of_kde, width = 10, height = 3.95),
  tar_png(fig_of_hdrboxplot, width = 10, height = 3.95),

  # French mortality rates -------------------------------
  tar_target(
    fr_mortality,
    weird::fr_mortality
  ),
  tar_target(
    fig_fr_mortality2,
    create_fr_mortality_plot(fr_mortality, type = "timeseries")
  ),
  tar_png(fig_fr_mortality2),
  tar_target(
    fr_anomalies,
    find_fr_anomalies(fr_mortality)
  ),
  tar_target(
    fig_hampel_stages,
    {
      stages <- c("series", "window", "difference", "normal")
      figs <- setNames(
        lapply(stages, \(s) create_hampel_fig(fr_mortality, stage = s)),
        stages
      )
      # Window stage shows the first full window, where the video starts
      figs$window <- create_hampel_fig(
        fr_mortality,
        stage = "window",
        centre = 1823
      )
      figs
    }
  ),
  # Stages of the Hampel figure, in the order of fig_hampel_stages
  tar_target(
    fig_hampel_png,
    unname(mapply(
      save_png,
      fig_hampel_stages,
      file.path(
        "figs",
        paste0("fig_hampel_", names(fig_hampel_stages), ".png")
      ),
      MoreArgs = list(height = 2.6)
    )),
    format = "file",
    packages = c(tar_option_get("packages"), "ggtext")
  ),
  # Window sliding over the series, tracing out the moving median
  tar_target(
    hampel_video,
    with_slide_style(
      hampel_window_video(
        fr_mortality,
        file.path("figs", "hampel_window.mp4")
      )
    ),
    format = "file"
  ),
  # The data after each step of the Hampel code on the slide
  tar_target(
    fr_hampel_steps,
    fr_steps(fr_mortality)
  ),
  tar_target(
    fr_anomalies_girafe,
    with_slide_style(create_fr_anomaly_girafe(fr_anomalies))
  ),
  # Known causes of the anomalous years, shown in a card when hovering over the plot
  tar_target(
    fr_causes_card,
    {
      fr_causes <- c(
        "1832" = "Cholera outbreak",
        "1834" = "unknown cause",
        "1837" = "unknown cause",
        "1849" = "Cholera outbreak",
        "1870" = "Franco–Prussian war",
        "1871" = "Repression of the Commune de Paris",
        "1914" = "World War I",
        "1915" = "World War I",
        "1918" = "Spanish flu outbreak",
        "1940" = "World War II"
      )
      hover_card(
        fr_causes,
        alert = names(fr_causes)[fr_causes == "unknown cause"]
      )
    }
  ),

  # Pharmaceutical surveillance --------------------------
  tar_target(
    pbs,
    make_pbs(tsibbledata::PBS)
  ),
  tar_target(
    pbs_eg_steps,
    pbs_steps(tsibbledata::PBS)
  ),
  tar_target(
    pbs_fc_steps,
    pbs_forecast_steps(pbs, i = 36)
  ),
  tar_target(
    pbs_fc,
    pbs_fc_steps[[length(pbs_fc_steps)]]
  ),
  tar_target(
    pbs_fc_probs_steps,
    pbs_forecast_probs_steps(pbs_fc, pbs)
  ),
  # Distribution of all the surprisal probabilities, then with p < 0.01 coloured
  tar_target(fig_pbs_pvalues1, pbs_pvalue_hist(pbs_scores)),
  tar_target(fig_pbs_pvalues2, pbs_pvalue_hist(pbs_scores, highlight = TRUE)),
  tar_png(fig_pbs_pvalues1, width = 11, height = 3.5),
  tar_png(fig_pbs_pvalues2, width = 11, height = 3.5),
  # Staged figure: one-step forecasts of S01 from a rolling origin, with a
  # video rolling the origin forward
  tar_target(
    pbs_roll,
    pbs_roll_data(pbs, "S01", origins = 36:95)
  ),
  tar_target(
    fig_pbs_roll_files,
    pbs_roll_files(pbs_roll, "figs"),
    format = "file",
    packages = c(tar_option_get("packages"), "ggtext")
  ),
  # compute_pbs_scores(pbs) takes about an hour, so use the cached
  # result from the book (OTexts/weird/rds/pbs_scores.rds)
  tar_target(
    pbs_scores_file,
    "data/pbs_scores.rds",
    format = "file"
  ),
  tar_target(
    pbs_scores,
    readRDS(pbs_scores_file)
  ),
  # Heat map of all surprisal probabilities, filled in month by month
  tar_target(
    fig_pbs_heatmap_files,
    pbs_heatmap_files(pbs_scores, "figs"),
    format = "file",
    packages = c(tar_option_get("packages"), "ggtext")
  ),
  tar_target(
    pbs_anomalies,
    pbs_scores |> filter(prob < 0.001)
  ),
  tar_target(
    fig_pbs_anomalies,
    pbs_plot(pbs, pbs_anomalies, c(
      A12 = "Mineral supplements",
      N07 = "Other nervous system drugs",
      R05 = "Cough and cold preparations"
    ))
  ),
  tar_png(fig_pbs_anomalies, height = 4.6),
  tar_target(
    pbs_joint,
    compute_pbs_joint(pbs_scores)
  ),
  tar_target(
    pbs_joint_girafe_widget,
    with_slide_style(pbs_joint_girafe(pbs_joint))
  ),
  # Explanations of the significant months, shown in a card when hovering over the plot
  tar_target(
    pbs_joint_card_html,
    pbs_joint_card(pbs_joint, c(
      `1997 Dec` = "Unusually large end-of-year stockpiling spike",
      `1999 Mar` = "No obvious explanation",
      `2001 Dec` = "Unusually large end-of-year stockpiling spike",
      `2008 Feb` = "Usual February fall did not occur: a change in timing, not demand"
    ))
  ),

  # Slides ------------------------------------------------
  tar_quarto(
    slides_wombat,
    "wombat_2026.qmd",
    quiet = FALSE,
    extra_files = c(
      "_quarto.yml",
      "slim_html.py",
      "slides.css",
      "wombat_2026.css",
      "title-slide.html",
      "images/title_of.svg",
      "_extensions/quarto-monash/presentation/_images/logo/monash-stacked-reversed-white.png",
      "_extensions/EmilHvitfeldt/roughnotation/rough.lua",
      "_extensions/EmilHvitfeldt/roughnotation/rough.js",
      "_extensions/EmilHvitfeldt/roughnotation/rough.css",
      "katex/katex.min.css",
      "katex/katex.min.js",
      "katex/katex-local.html",
      "fonts/fonts.css"
    )
  )
)
