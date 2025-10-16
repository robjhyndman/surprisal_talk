library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("tibble", "weird", "lookout", "dplyr", "ggplot2", "ggtda")
)

tar_source()

# Replace the target list below with your own:
list(
  # Anomaly definition
  tar_target(
    figchisq5,
    create_fig_density(dist_chisq(5), y = 9, shading = FALSE)
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
      filter(variety %in% c("Shiraz", "Syrah"))
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

  # Slides ------------------------------------------------
  tar_quarto(
    slides,
    "surprisals.qmd",
    quiet = FALSE,
    extra_files = c("header.tex")
  )
)
