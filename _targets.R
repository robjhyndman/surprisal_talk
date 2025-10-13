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
    weird::fetch_wine_reviews()
  ),
  tar_target(
    wine_results,
    analyse_wine_data(wine_reviews, scale, alpha, beta, gamma)
  ),
  tar_target(
    fig_wine,
    create_wine_figure(wine_results, show_anomalies = FALSE)
  ),
  # Slides ------------------------------------------------
  tar_quarto(
    slides,
    "surprisals.qmd",
    quiet = FALSE
  )
)
