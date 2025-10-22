generate_showcase_examples <- function(scale, alpha, beta, gamma) {
  # Example 1
  X1 <- bind_rows(
    tibble(
      x = rnorm(1000),
      y = rnorm(1000)
    ),
    tibble(
      x = rnorm(5, mean = 10, sd = 0.2),
      y = rnorm(5, mean = 10, sd = 0.2)
    )
  )

  ex1_lookout_new <- lookout::lookout(
    X1,
    scale = scale,
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    old_version = FALSE
  )

  # Example 2
  X2 <- bind_rows(
    tibble(
      x = rnorm(1000, mean = -10),
      y = rnorm(1000),
    ),
    tibble(
      x = rnorm(1000, mean = 10),
      y = rnorm(1000)
    ),
    tibble(
      x = rnorm(5, sd = 0.2),
      y = rnorm(5, sd = 0.2)
    )
  )
  ex2_lookout_new <- lookout::lookout(
    X2,
    scale = scale,
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    old_version = FALSE
  )

  # Example 3
  X3 <- bind_rows(
    tibble(
      x = rnorm(500),
      y = rnorm(500)
    ),
    tibble(
      x = rnorm(100, sd = 0.7),
      y = rnorm(100, mean = 8, sd = 0.7)
    ),
    tibble(
      x = rnorm(100, mean = 8, sd = 0.7),
      y = rnorm(100, sd = 0.7)
    ),
    tibble(
      x = rnorm(3, mean = 6, sd = 0.2),
      y = rnorm(3, mean = 6, sd = 0.2)
    )
  )

  ex3_lookout_new <- lookout::lookout(
    X3,
    scale = scale,
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    old_version = FALSE
  )

  # Example 4
  X4 <- bind_rows(
    tibble(
      x = rnorm(500),
      y = rnorm(500)
    ),
    tibble(
      x = rnorm(100, sd = 0.7),
      y = rnorm(100, mean = 8, sd = 0.7)
    ),
    tibble(
      x = rnorm(100, mean = 8, sd = 0.7),
      y = rnorm(100, sd = 0.7)
    ),
    tibble(
      x = c(5, 8, 12),
      y = c(5, 7.5, 4)
    )
  )

  ex4_lookout_new <- lookout::lookout(
    X4,
    scale = scale,
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    old_version = FALSE
  )

  # Example 5
  X5 <- bind_rows(
    tibble(
      x = rnorm(1000, sd = 0.2),
      y = x^2 + rnorm(1000, sd = 0.001)
    ),
    tibble(
      x = c(0, -0.4, 0.4),
      y = c(0.15, 0.3, 0.3)
    )
  )
  ex5_lookout_new <- lookout::lookout(
    X5,
    scale = scale,
    alpha = alpha,
    beta = beta,
    gamma = gamma,
    old_version = FALSE
  )

  bind_rows(
    as.data.frame(ex1_lookout_new) |>
      mutate(Example = "Example 1", Version = "New Lookout"),
    as.data.frame(ex2_lookout_new) |>
      mutate(Example = "Example 2", Version = "New Lookout"),
    as.data.frame(ex3_lookout_new) |>
      mutate(Example = "Example 3", Version = "New Lookout"),
    as.data.frame(ex4_lookout_new) |>
      mutate(Example = "Example 4", Version = "New Lookout"),
    as.data.frame(ex5_lookout_new) |>
      mutate(Example = "Example 5", Version = "New Lookout"),
  ) |>
    as_tibble() |>
    group_by(Example) |>
    mutate(x = (x - min(x)) / (max(x) - min(x))) |>
    ungroup()
}

create_showcase_figure <- function(results) {
  df <- results |>
    mutate(alpha = 0.4 + 0.6 * outliers)
  # Create figure
  df |>
    ggplot(aes(x = x, y = y, color = outliers)) +
    geom_point(alpha = df$alpha) +
    facet_wrap(Example ~ ., scales = "free") +
    theme(
      axis.title.x = element_blank(), # Hide x-axis title
      axis.text.x = element_blank(), # Hide x-axis text labels
      axis.ticks.x = element_blank(), # Hide x-axis tick marks
      axis.title.y = element_blank(), # Hide y-axis title
      axis.text.y = element_blank(), # Hide y-axis text labels
      axis.ticks.y = element_blank() # Hide y-axis tick marks
    ) +
    scale_color_manual(values = c(`FALSE` = "#999999", `TRUE` = "red")) +
    guides(color = "none") # Hide legend
}
