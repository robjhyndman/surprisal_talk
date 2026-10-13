library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c(
    "tibble", "weird", "lookout", "dplyr", "ggplot2", "ggtda",
    "tsibble", "fable", "feasts"
  )
)

tar_source()

# Target saving figure target `fig` as a PNG for uconn_2026.qmd
tar_png <- function(fig, width = 9, height = 4.4) {
  fig <- deparse(substitute(fig))
  tar_target_raw(
    paste0(fig, "_png"),
    substitute(
      save_png(FIG, FILE, width = WIDTH, height = HEIGHT),
      list(
        FIG = as.symbol(fig),
        FILE = file.path("figs", "uconn", paste0(fig, ".png")),
        WIDTH = width,
        HEIGHT = height
      )
    ),
    format = "file",
    packages = c(tar_option_get("packages"), "ggtext", "patchwork")
  )
}

# Replace the target list below with your own:
list(
  # Anomaly definition
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
  tar_target(
    figchisq1,
    make_figchisq1(chisq_data1)
  ),
  tar_target(
    figchisq2,
    make_figchisq1(chisq_data2)
  ),
  tar_target(
    figchisq3,
    make_figchisq1(chisq_data3)
  ),
  tar_target(
    figchisq5,
    create_fig_density(dist_chisq(5), y = 9, shading = FALSE)
  ),
  tar_target(
    spot_anomaly_data,
    make_spot_anomaly_data()
  ),
  tar_target(
    fig_spot_anomaly1,
    create_fig_spot_anomaly1(spot_anomaly_data)
  ),
  tar_target(
    fig_spot_anomaly2,
    create_fig_spot_anomaly2(spot_anomaly_data)
  ),
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
  tar_target(
    fig_sp_normal,
    create_fig_surprisal_prob(
      step = 2L,
      den = dist_normal(),
      clean = TRUE,
      size = 98
    )
  ),
  tar_target(
    fig61,
    create_fig_density(dist_chisq(5), y = 9)
  ),
  tar_target(
    fig_normal,
    create_fig_density(dist_normal(), y = 2.2)
  ),
  tar_target(
    fig_hdr,
    create_fig_density(
      dist_mixture(
        dist_normal(-2, 1),
        dist_normal(2, 1),
        weights = c(1 / 3, 2 / 3)
      ),
      y = -0.5
    )
  ),
  # KDE
  tar_map(
    list(h = seq(0.04, 0.25, l = 99), i = seq(99)),
    tar_target(
      kde_file,
      make_kde_plot(rips_data, h, i),
    )
  ),
  # RIPS animation
  tar_target(
    rips_data,
    d <- tibble(
      x = stats::rnorm(n = 20, mean = 0, sd = .2),
      y = x + stats::rnorm(n = 20, mean = 0, sd = .2)
    )
  ),
  tar_map(
    list(prox = seq(0.01, by = 0.005, l = 99), i = seq(99)),
    tar_target(
      rip_file,
      rips(rips_data, prox, i),
    )
  ),
  tar_target(
    barcode,
    make_barcode(rips_data)
  ),
  tar_target(
    rips_kde_bandwidth,
    lookout::find_tda_bw(rips_data, fast = FALSE, gamma = gamma)
  ),
  tar_target(
    rips_kde,
    dist_kde(rips_data, H = diag(rep(rips_kde_bandwidth^2, 2))) |>
      gg_density(show_points = TRUE)
  ),
  # Should scaling be used?
  tar_target(scale, TRUE),
  # Value of alpha
  tar_target(alpha, 0.01),
  # Value of beta
  tar_target(beta, 0.90),
  # Value of gamma
  tar_target(gamma, 0.97),
  # Old faithful example ------------------------------
  tar_target(
    oldfaithful,
    weird::oldfaithful
  ),
  tar_target(
    old_faithful_results,
    analyse_old_faithful(oldfaithful, scale, alpha, beta, gamma)
  ),
  tar_target(
    fig_old_faithful,
    create_old_faithful_figure(old_faithful_results)
  ),
  tar_target(
    fig_old_faithful2,
    create_old_faithful_figure(
      old_faithful_results,
      scale = TRUE
    )
  ),
  tar_target(
    fig_old_faithful3,
    create_old_faithful_figure(
      old_faithful_results,
      scale = TRUE,
      contour = TRUE
    )
  ),
  tar_target(
    fig_old_faithful4,
    create_old_faithful_figure(
      old_faithful_results,
      scale = TRUE,
      contour = TRUE,
      show_anomalies = TRUE
    )
  ),
  tar_target(
    fig_old_faithful5,
    create_old_faithful_figure(
      old_faithful_results,
      scale = FALSE,
      contour = FALSE,
      show_anomalies = TRUE
    )
  ),
  tar_target(
    of_lookout_prob,
    get_of_lookout_prob(oldfaithful, alpha)
  ),
  # Wine reviews example -------------------------------
  tar_target(
    wine_reviews,
    weird::fetch_wine_reviews() |>
      filter(variety %in% c("Shiraz", "Syrah")) |>
      select(-variety, -region)
  ),
  tar_target(
    wine_aug,
    augment_wine(wine_reviews)
  ),
  tar_target(
    wine_loo_surprisals,
    plot_wine_loo_surprisals(wine_aug)
  ),
  tar_target(
    wine_loo_anomalies,
    plot_wine_loo_surprisals(wine_aug, show_anomalies = TRUE)
  ),
  tar_target(
    wine_model_plot,
    create_wine_model_plot(wine_aug)
  ),
  #tar_target(
  #  wine_results,
  #  analyse_wine_data(wine_reviews, scale, alpha, beta, gamma)
  #),
  tar_target(
    fig_wine,
    create_wine_figure(wine_aug, show_anomalies = FALSE)
  ),
  tar_target(
    fig_wine1,
    create_wine_figure(wine_aug, show_anomalies = FALSE, log = TRUE)
  ),
  tar_target(
    fig_wine2,
    create_wine_figure(wine_aug, show_anomalies = TRUE)
  ),
  tar_target(
    wine_anomalies,
    wine_aug |>
      filter(prob < 0.001) |>
      arrange(prob) |>
      select(
        Area = state,
        Winery = winery,
        Year = year,
        Points = points,
        Price = price
      )
  ),
  # French mortality example ---------------------------
  tar_target(
    fr_mortality,
    weird::fr_mortality
  ),
  tar_target(
    fig_fr_mortality,
    create_fr_mortality_plot(fr_mortality, type = "functional")
  ),
  tar_target(
    fig_fr_mortality2,
    create_fr_mortality_plot(fr_mortality, type = "timeseries")
  ),
  tar_target(
    fig_fr_mortality3,
    create_fr_mortality_plot(fr_mortality, type = "functional", fr_anomalies)
  ),
  tar_target(
    fig_fr_mortality4,
    create_fr_mortality_plot(fr_mortality, type = "timeseries", fr_anomalies)
  ),
  tar_target(
    fr_anomalies,
    find_fr_anomalies(fr_mortality)
  ),
  tar_target(
    fig_fr_anomalies,
    create_fr_anomaly_plot(fr_anomalies)
  ),
  # Cricket not-outs ------------------------------------
  tar_target(
    cricket_batting,
    weird::cricket_batting
  ),
  tar_target(
    not_outs,
    cricket_batting |>
      filter(Innings > 0) |>
      mutate(prop_no = NotOuts / Innings)
  ),
  tar_target(
    fit_no,
    fit_notouts(not_outs)
  ),
  tar_target(
    notouts_aug,
    augment_notouts(fit_no, not_outs)
  ),
  tar_target(
    not_outs_plot,
    create_notouts_plot(notouts_aug, show_smooth = FALSE, show_jma = FALSE)
  ),
  tar_target(
    not_outs_plot_smooth,
    create_notouts_plot(notouts_aug, show_jma = FALSE)
  ),
  tar_target(
    not_outs_plot_jma,
    create_notouts_plot(notouts_aug)
  ),

  # Experiment 1 - theory
  tar_target(
    norm_data,
    rnorm(1000)
  ),
  tar_target(
    t4_data,
    rt(1000, df = 4)
  ),
  tar_target(
    fig_expt1,
    create_fig_expt1(norm_data, t4_data)
  ),
  tar_target(
    fig_expt1b,
    create_fig_expt1(norm_data, t4_data, empirical = FALSE)
  ),
  # Experiment 2 - theory
  tar_target(
    expt2_prob,
    get_expt2_prob()
  ),
  tar_target(
    fig_expt2,
    create_fig_expt2(expt2_prob)
  ),
  tar_target(
    fig_expt2b,
    create_fig_expt2(expt2_prob, empirical = FALSE)
  ),
  tar_target(
    gpd_shape_estimates,
    get_gpd_shape()
  ),
  tar_target(
    fig_gpd_shape,
    create_gpd_shape_fig(gpd_shape_estimates)
  ),

  # ------------------------------------------------------
  # Experiment 1: Gamma distribution
  # ------------------------------------------------------

  tar_target(
    exp1_results,
    run_synthetic_exp1(scale, alpha, beta, gamma)
  ),
  tar_target(
    fig_exp1,
    create_figure_exp1(exp1_results)
  ),

  # ------------------------------------------------------
  # Experiment 2: Normal distribution
  # ------------------------------------------------------

  tar_target(
    exp2_results,
    run_synthetic_exp2(scale, alpha, beta, gamma)
  ),
  tar_target(
    fig_exp2,
    create_figure_exp2(exp2_results)
  ),

  # ------------------------------------------------------
  # Experiment 3: Increasing N with normal distribution
  # ------------------------------------------------------

  tar_target(
    exp3_results,
    run_synthetic_exp3(scale, alpha, beta, gamma)
  ),
  tar_target(
    fig_exp3,
    create_figure_exp3(exp3_results)
  ),

  # ------------------------------------------------------
  # Experiment 4: Increasing N with gamma distribution
  # ------------------------------------------------------

  tar_target(
    exp4_results,
    run_synthetic_exp4(scale, alpha, beta, gamma)
  ),
  tar_target(
    fig_exp4,
    create_figure_exp4(exp4_results)
  ),

  # ------------------------------------------------------
  # SHOWCASE EXAMPLES
  # ------------------------------------------------------

  tar_target(
    showcase_results,
    generate_showcase_examples(scale, alpha, beta, gamma)
  ),
  tar_target(
    fig_showcase,
    create_showcase_figure(showcase_results)
  ),
  # Time series examples --------------------------------
  tar_target(
    fig_tsparadigms,
    create_fig_tsparadigms(fr_mortality)
  ),
  tar_target(
    fig_sp_katex,
    make_fig_sp_katex(list(fig_sp0, fig_sp1, fig_sp2))
  ),
  # Weird times: French mortality
  tar_target(
    fig_hampel_stages,
    {
      stages <- c("series", "window", "difference", "normal")
      setNames(lapply(stages, \(s) create_hampel_fig(fr_mortality, stage = s)), stages)
    }
  ),
  # Window sliding over the series, tracing out the moving median
  tar_target(
    hampel_video,
    with_uconn_style(
      hampel_window_video(fr_mortality, file.path("figs", "uconn", "hampel_window.mp4"))
    ),
    format = "file"
  ),
  tar_target(
    fr_anomaly_years,
    fr_anomalies |>
      distinct(Year, Sex, n) |>
      arrange(Year, Sex) |>
      as.data.frame()
  ),
  tar_target(
    fr_anomalies_girafe,
    with_uconn_style(create_fr_anomaly_girafe(fr_anomalies))
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
      hover_card(fr_causes, alert = names(fr_causes)[fr_causes == "unknown cause"])
    }
  ),
  # Weird times: US weekly mortality
  tar_target(
    us_stmf,
    make_us_stmf(weird::us_mortality)
  ),
  tar_target(
    fig_stmf,
    us_stmf |>
      autoplot(Mortality) +
      facet_grid(`Age group` ~ ., scales = "free_y") +
      guides(colour = "none")
  ),
  tar_target(
    stmf_scores,
    compute_stmf_scores(us_stmf)
  ),
  # Output as it is computed on the slide: columns after augment(), a, e and
  # prob, in time order; then sorted by prob
  tar_target(
    stmf_scores_steps,
    {
      steps <- lapply(
        list(character(), "a", c("a", "e"), c("a", "e", "prob")),
        \(extra) stmf_scores |>
          arrange(Week, `Age group`) |>
          select(Week, `Age group`, Mortality, .fitted, all_of(extra))
      )
      c(steps, list(arrange(steps[[4]], prob)))
    }
  ),
  tar_target(
    fig_stl_anomalies,
    us_mortality_plot(us_stmf, stmf_scores)
  ),
  tar_target(
    us_episodes,
    make_us_episodes()
  ),
  tar_target(
    us_zoom,
    list(from = yearweek("2019 W40"), to = yearweek("2022 W26"))
  ),
  tar_target(
    us_covid_girafe,
    with_uconn_style(us_mortality_girafe(
      us_stmf,
      stmf_scores,
      us_episodes,
      from = us_zoom$from,
      to = us_zoom$to,
      band_alpha = 0.08
    ))
  ),
  # Animation zooming in from the full period to the interactive plot
  tar_target(
    us_zoom_video,
    with_uconn_style(us_mortality_zoom_video(
      us_stmf,
      stmf_scores,
      us_episodes,
      from = us_zoom$from,
      to = us_zoom$to,
      file = "figs/us_zoom.mp4"
    )),
    format = "file"
  ),
  tar_target(
    us_causes_card,
    hover_card(
      setNames(us_episodes$cause, us_episodes$episode),
      alert = c("2020 W03–W04", "2021 W13–W15")
    )
  ),
  # Weird series: PBS
  tar_target(
    pbs_raw,
    tsibbledata::PBS
  ),
  tar_target(
    pbs_features,
    compute_pbs_features(pbs_raw)
  ),
  tar_target(
    pbs_features_table,
    pbs_features |>
      select(-Concession, -Type, -ATC1)
  ),
  tar_target(
    pbs_pca,
    compute_pbs_pca(pbs_features)
  ),
  tar_target(
    pbs_pca_table,
    pbs_pca |>
      select(Concession, Type, ATC2, .fittedPC1, .fittedPC2, prob) |>
      arrange(prob)
  ),
  tar_target(
    fig_pbs_pca,
    pbs_pca |>
      ggplot(aes(x = .fittedPC1, y = .fittedPC2)) +
      geom_point() +
      labs(x = "Principal component 1", y = "Principal component 2")
  ),
  tar_target(
    fig_pbs_outliers,
    pbs_series_plot(
      pbs_raw,
      pbs_pca |> filter(prob < 0.001),
      "Outlying time series in PC space"
    )
  ),
  tar_target(
    pbs_missing,
    pbs_features |>
      anti_join(pbs_pca, by = c("Concession", "Type", "ATC1", "ATC2"))
  ),
  tar_target(
    fig_pbs_missing,
    pbs_series_plot(pbs_raw, pbs_missing, "Time series with missing features")
  ),
  # Surveillance: PBS by ATC2 group
  tar_target(
    pbs,
    make_pbs(pbs_raw)
  ),
  tar_target(
    fig_a12,
    pbs |>
      filter(ATC2 == "A12") |>
      autoplot(Scripts) +
      labs(
        y = "Scripts (thousands)",
        title = "Scripts for ATC group A12 (Mineral supplements)"
      )
  ),
  tar_target(
    fig_tscv,
    tscv_plot(.init = 8, .step = 1, h = 1) +
      annotate("text", x = 9, y = 0, label = "h = 1", colour = "#c14b14")
  ),
  tar_target(
    pbs_fc,
    pbs_forecast(pbs, i = 36)
  ),
  tar_target(
    pbs_fc_probs,
    pbs_forecast_probs(pbs_fc, pbs, i = 36)
  ),
  tar_target(
    fig_s01_forecast,
    pbs_forecast_plot(pbs_fc, pbs, "S01", i = 36)
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
  tar_target(
    pbs_anomalies,
    pbs_scores |> filter(prob < 0.001)
  ),
  tar_target(
    fig_pbs_a12,
    pbs_plot(pbs, pbs_anomalies, "A12")
  ),
  tar_target(
    fig_pbs_n07,
    pbs_plot(pbs, pbs_anomalies, "N07")
  ),
  tar_target(
    fig_pbs_r05,
    pbs_plot(pbs, pbs_anomalies, "R05")
  ),
  tar_target(
    pbs_joint,
    compute_pbs_joint(pbs_scores)
  ),
  tar_target(
    fig_pbs_joint,
    pbs_joint_plot(pbs_joint)
  ),
  tar_target(
    pbs_joint_anomalies,
    pbs_joint_table(pbs_joint)
  ),
  tar_target(
    pbs_feb08,
    pbs_joint |> filter(Month == yearmonth("2008 Feb"))
  ),
  # PNGs of the figures in uconn_2026.qmd -----------------
  tar_png(fig_tsparadigms, width = 7, height = 5.6),
  tar_png(fig_expt1b),
  tar_png(fig_fr_mortality2),
  # Stages of the Hampel figure, in the order of fig_hampel_stages
  tar_target(
    fig_hampel_png,
    unname(mapply(
      save_png,
      fig_hampel_stages,
      file.path("figs", "uconn", paste0("fig_hampel_", names(fig_hampel_stages), ".png")),
      MoreArgs = list(height = 2.6)
    )),
    format = "file",
    packages = c(tar_option_get("packages"), "ggtext")
  ),
  tar_png(fig_stmf),
  tar_png(fig_stl_anomalies, height = 4.25),
  # Feature-space build-up: stages of each figure, overlaid as fragments
  tar_target(pbs_scaled, pbs_rescale(pbs_raw)),
  tar_target(
    fig_pbs_build_png,
    {
      stages <- list(
        spaghetti = pbs_spaghetti_stages(pbs_scaled),
        trend = pbs_feature_stages(pbs_features, pbs_scaled, "trend_strength", "Strength of trend"),
        season = pbs_feature_stages(pbs_features, pbs_scaled, "seasonal_strength_year", "Strength of seasonality"),
        # Grobs are built with a PNG device open, so text is measured as drawn
        space = {
          tmp <- tempfile(fileext = ".png")
          ragg::agg_png(tmp, width = 9, height = 3.6, units = "in", res = 400)
          grobs <- with_uconn_style(pbs_feature_space_stages(pbs_scaled, pbs_pca))
          grDevices::dev.off()
          unlink(tmp)
          grobs
        }
      )
      heights <- c(spaghetti = 4.4, trend = 4.4, season = 4.4, space = 3.6)
      unlist(lapply(names(stages), function(s) {
        files <- file.path("figs", "uconn", paste0("fig_pbs_", s, seq_along(stages[[s]]), ".png"))
        unname(mapply(save_png, stages[[s]], files, MoreArgs = list(height = heights[[s]])))
      }))
    },
    format = "file",
    packages = c(tar_option_get("packages"), "ggtext", "patchwork", "GGally")
  ),
  tar_png(fig_pbs_pca),
  tar_png(fig_pbs_outliers),
  tar_png(fig_pbs_missing),
  tar_png(fig_a12, height = 3.2),
  tar_png(fig_tscv),
  tar_png(fig_s01_forecast),
  tar_png(fig_pbs_a12),
  tar_png(fig_pbs_n07),
  tar_png(fig_pbs_r05),
  tar_png(fig_pbs_joint),
  # The surprisal probability steps, with the same panel position in every
  # step so they overlay exactly
  tar_target(
    fig_sp_png,
    save_aligned_pngs(
      fig_sp_katex,
      file.path("figs", "uconn", paste0("fig_sp", seq_along(fig_sp_katex), ".png")),
      height = 3.6
    ),
    format = "file",
    packages = c(tar_option_get("packages"), "ggtext")
  ),
  # Slides ------------------------------------------------
  tar_quarto(
    slides,
    "auckland_2026.qmd", #"surprisals.qmd",  "surprisals_fsi2025.qmd",
    quiet = FALSE,
    extra_files = c("header.tex", "before-title.tex")
  ),
  tar_quarto(
    slides_uconn,
    "uconn_2026.qmd",
    quiet = FALSE,
    extra_files = c(
      "uconn_2026.css", "title-slide.html", "figs/title_ts.png", "katex/katex.min.css", "katex/katex.min.js",
      "katex/katex-local.html", "fonts/fonts.css"
    )
  )
)
