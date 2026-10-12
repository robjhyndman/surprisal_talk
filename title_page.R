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
