# Generate title page
library(weird)
p <- oldfaithful |>
  ggplot(aes(x = duration, y = waiting)) +
  geom_point(colour = "#65775c") +
  theme_void()
cairo_pdf("figs/title.pdf", width = 8, height = 4.5)
print(p)
dev.off()

# Title page for time series talk
library(tsibble)
p <- us_mortality |>
  filter(Sex == "Total", Age == "85+") |>
  mutate(Week = yearweek(paste0(Year, "-W", Week))) |>
  ggplot(aes(x = Week, y = Mortality)) +
  geom_line(colour = "#65775c", linewidth = 0.8) +
  theme_void()
cairo_pdf("figs/title_ts.pdf", width = 8, height = 4.5)
print(p)
dev.off()
# PNG version for revealjs title slide: light line in the lower half of a
# transparent image, to sit on a dark background below the title text
p <- p +
  aes(y = Mortality) +
  scale_y_continuous(expand = expansion(mult = c(0.03, 1.1)))
p$layers[[1]]$aes_params$colour <- "#4f86b5"
ragg::agg_png(
  "figs/title_ts.png", width = 8, height = 4.5, units = "in", res = 240,
  background = "transparent"
)
print(p)
dev.off()
