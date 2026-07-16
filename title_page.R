# Generate title page
library(weird)
p <- oldfaithful |>
  ggplot(aes(x = duration, y = waiting)) +
  geom_point(colour = "#65775c") +
  theme_void()
cairo_pdf("figs/title.pdf", width = 8, height = 4.5)
print(p)
dev.off()
