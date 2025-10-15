fit_notouts <- function(not_outs) {
  fit <- mgcv::gam(
    prop_no ~ s(Innings),
    data = not_outs,
    family = binomial(link = logit),
    weights = Innings
  )
}

augment_notouts <- function(fit, not_outs) {
  broom::augment(fit, data = not_outs, type.predict = "response")
}

create_notouts_plot <- function(
  notouts_aug,
  show_smooth = TRUE,
  show_jma = TRUE
) {
  p <- notouts_aug |>
    ggplot(aes(x = Innings, y = prop_no)) +
    geom_point(alpha = 0.2)
  if (show_smooth) {
    p <- p +
      geom_line(aes(y = .fitted), color = "#0072B2") +
      geom_ribbon(
        aes(
          ymin = .fitted - 2 * .se.fit,
          ymax = .fitted + 2 * .se.fit
        ),
        fill = "#0072B2",
        alpha = 0.2
      )
  }
  if (show_jma) {
    p <- p +
      ggrepel::geom_text_repel(
        data = notouts_aug |> filter(Player == "JM Anderson"),
        aes(label = "JM Anderson"),
        col = "red"
      )
  }
  p +
    labs(
      title = "Career batting data for all test cricketers (M+W): 1834-2025",
      y = "Proportion of not outs"
    )
}

noutouts_anomalies <- function(fit_notouts, notouts_aug) {
  fit_notouts |>
    mutate(
      surprisals = surprisals(fit_notouts, probability = FALSE),
      prob = surprisals(fit_notouts, approximation = "gpd")
    ) |>
    select(Player:Country, Innings:NotOuts, prop_no:.fitted, surprisals:prob) |>
    arrange(desc(surprisals))
}
