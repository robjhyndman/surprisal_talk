library(weird)

get_expt1_prob <- function(y) {
  tibble(
    y = y,
    p = surprisals(y, distribution = dist_normal()),
    p_emp = surprisals(
      y,
      distribution = dist_normal(),
      approximation = "empirical"
    ),
    p_gpd = surprisals(y, distribution = dist_normal(), approximation = "gpd"),
    pt = surprisals(y, distribution = dist_student_t(4)),
    pt_emp = surprisals(
      y,
      distribution = dist_student_t(4),
      approximation = "empirical"
    ),
    pt_gpd = surprisals(
      y,
      distribution = dist_student_t(4),
      approximation = "gpd"
    )
  )
}

create_fig_expt1 <- function(norm, t4) {
  bind_rows(
    norm |> get_expt1_prob() |> mutate(Model = "Data: Normal"),
    t4 |> get_expt1_prob() |> mutate(Model = "Data: t(4)")
  ) |>
    filter(y > 2.5) |>
    tidyr::pivot_longer(
      p:pt_gpd,
      names_to = "Estimate",
      values_to = "Probability"
    ) |>
    mutate(
      Distribution = if_else(
        Estimate %in% c("p", "p_emp", "p_gdp"),
        "Normal",
        "t(4)"
      ),
      Estimate = factor(
        Estimate,
        levels = c("p", "pt", "p_emp", "pt_emp", "p_gpd", "pt_gpd"),
        labels = c(
          "Assumed distribution",
          "Assumed distribution",
          "Empirical distribution",
          "Empirical distribution",
          "GPD",
          "GPD"
        )
      )
    ) |>
    ggplot(aes(x = y, y = Probability, col = Estimate, lty = Distribution)) +
    geom_line() +
    facet_wrap(~Model, scales = "free_x") +
    theme(legend.position = "bottom")
}
