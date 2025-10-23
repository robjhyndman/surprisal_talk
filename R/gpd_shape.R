gpd_shape <- function(s, threshold_p = 0.10) {
  n <- length(s)
  threshold_q <- stats::quantile(
    s,
    prob = 1 - threshold_p,
    type = 8,
    na.rm = TRUE
  )
  if (!any(s > threshold_q, na.rm = TRUE)) {
    warning("No surprisals above threshold")
    return(rep(1, n))
  }
  finite <- s < Inf
  if (any(!finite, na.rm = TRUE)) {
    warning("Infinite surprisals will be ignored in GPD")
  }
  gpd <- evd::fpot(s[finite], threshold = threshold_q, std.err = FALSE)$estimate
  return(gpd["shape"])
}
get_gpd_shape_single <- function(n) {
  data <- matrix(rgamma(2 * n, 2, 2), n, 2)
  df <- 4
  s1 <- -dgamma(data[, 1], 2, 2, log = TRUE) -
    dgamma(data[, 2], 2, 2, log = TRUE)
  s2 <- -dnorm(data[, 1], 1, 1 / sqrt(2), log = TRUE) -
    dnorm(data[, 2], 1, 1 / sqrt(2), log = TRUE)
  s3 <- -dt(data[, 1], df = df, ncp = 1 - 3 / (4 * df - 1), log = TRUE) -
    dt(data[, 2], df = df, ncp = 1 - 3 / (4 * df - 1), log = TRUE)
  s4 <- -dgamma(data[, 1], 1, 1, log = TRUE) -
    dgamma(data[, 2], 1, 1, log = TRUE)
  tibble(
    GPD1 = gpd_shape(s1, 0.10),
    GPD2 = gpd_shape(s2, 0.10),
    GPD3 = gpd_shape(s3, 0.10),
    GPD4 = gpd_shape(s4, 0.10),
  )
}

get_gpd_shape <- function(n = 10^seq(2, 5, by = 0.1), nrep = 500) {
  purrr::pmap_dfr(
    tidyr::expand_grid(n = n, rep = seq(nrep)),
    function(n, rep) {
      get_gpd_shape_single(n) |> mutate(rep = rep, n = n)
    }
  )
}

create_gpd_shape_fig <- function(gpd_shape_estimates) {
  gpd_shape_estimates |>
    tidyr::pivot_longer(GPD1:GPD4, names_to = "Method") |>
    dplyr::group_by(n, Method) |>
    dplyr::summarise(mean = mean(value)) |>
    mutate(
      Method = recode(
        Method,
        GPD1 = "GPD (Gamma22)",
        GPD2 = "GPD (Normal)",
        GPD3 = "GPD (t)",
        GPD4 = "GPD (Gamma11)"
      )
    ) |>
    ggplot(aes(x = n, y = mean, col = Method)) +
    geom_line() +
    scale_x_log10() +
    labs(y = "Average shape parameter")
}
