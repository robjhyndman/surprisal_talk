library(targets)
library(tarchetypes)

# Set target options:
tar_option_set(
  packages = c("tibble", "weird", "lookout")
)

tar_source()

# Replace the target list below with your own:
list(
  # Should scaling be used?
  tar_target(scale, TRUE),
  # Value of alpha
  tar_target(alpha, 0.01),
  # Value of gamma
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
    create_old_faithful_figure(old_faithful_results, show_anomalies = FALSE)
  ),
  # Wine reviews example -------------------------------
  tar_target(
    wine_reviews,
    weird::fetch_wine_reviews() |>
      filter(variety %in% c("Shiraz", "Syrah")) |>
      select(points, price)
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
  tar_target(
    wine_results,
    analyse_wine_data(wine_reviews, scale, alpha, beta, gamma)
  ),
  tar_target(
    fig_wine,
    create_wine_figure(wine_aug, show_anomalies = FALSE)
  ),
  tar_target(
    fig_wine2,
    create_wine_figure(wine_aug, show_anomalies = TRUE)
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
  # Slides ------------------------------------------------
  tar_quarto(
    slides,
    "surprisals.qmd",
    quiet = FALSE
  )
)
