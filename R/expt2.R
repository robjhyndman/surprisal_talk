get_expt2_prob_single <- function(n) {
  data <- matrix(rgamma(2 * n, 2, 2), n, 2)
  df <- 4
  tibble(
    s1 = -dgamma(data[, 1], 2, 2, log = TRUE) -
      dgamma(data[, 2], 2, 2, log = TRUE),
    s2 = -dnorm(data[, 1], 1, 1 / sqrt(2), log = TRUE) -
      dnorm(data[, 2], 1, 1 / sqrt(2), log = TRUE),
    s3 = -dt(data[, 1], df = df, ncp = 1 - 3 / (4 * df - 1), log = TRUE) -
      dt(data[, 2], df = df, ncp = 1 - 3 / (4 * df - 1), log = TRUE),
    s4 = -dgamma(data[, 1], 1, 1, log = TRUE) -
      dgamma(data[, 2], 1, 1, log = TRUE),
    Empirical1 = rank(-s1) / n,
    GPD1 = weird:::surprisal_gpd_prob(s1, 0.10),
    Empirical2 = rank(-s2) / n,
    GPD2 = weird:::surprisal_gpd_prob(s2, 0.10),
    Empirical3 = rank(-s3) / n,
    GPD3 = weird:::surprisal_gpd_prob(s3, 0.10),
    Empirical4 = rank(-s4) / n,
    GPD4 = weird:::surprisal_gpd_prob(s4, 0.10),
  ) |>
    select(-s1, -s2, -s3, -s4) |>
    tidyr::pivot_longer(
      everything(),
      names_to = "Method",
      values_to = "prob"
    ) |>
    filter(prob <= 0.01) |>
    count(Method) |>
    rename(count = n)
}

get_expt2_prob <- function(n = 10^seq(2, 5, by = 0.1), nrep = 500) {
  purrr::pmap_dfr(
    tidyr::expand_grid(n = n, rep = seq(nrep)),
    function(n, rep) {
      get_expt2_prob_single(n) |> mutate(rep = rep, n = n)
    }
  )
}

create_fig_expt2 <- function(expt2_prob, empirical = TRUE) {
  df <- expt2_prob |>
    mutate(anomaly_rate = count / n) |>
    group_by(Method, n) |>
    mutate(
      Method = recode(
        Method,
        Empirical1 = "Empirical (Gamma22)",
        GPD1 = "GPD (Gamma22)",
        Empirical2 = "Empirical (Normal)",
        GPD2 = "GPD (Normal)",
        Empirical3 = "Empirical (t)",
        GPD3 = "GPD (t)",
        Empirical4 = "Empirical (Gamma11)",
        GPD4 = "GPD (Gamma11)"
      )
    ) |>
    ungroup() 
  if(!empirical) {
    df <- df |>
      filter(!grepl("Empirical", Method))
  }
  df |>
    ggplot(aes(x = log10(n), y = anomaly_rate, col = Method)) +
    geom_smooth(method = "loess", span = 0.6) +
    geom_hline(aes(yintercept = 0.01)) +
    scale_x_continuous(breaks = 2:5, labels = sprintf("%d", 10^(2:5))) +
    labs(x = "Sample size n", y = "Anomaly rate")
}
