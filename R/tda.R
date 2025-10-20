rips <- function(d, prox, i) {
  p_d <- ggplot(d, aes(x = x, y = y)) +
    coord_fixed() +
    geom_disk(radius = prox / 2, fill = "#0072B2") +
    geom_point()
  p_sc <- ggplot(d, aes(x = x, y = y)) +
    coord_fixed() +
    stat_simplicial_complex(diameter = prox, fill = "#c14b14") +
    theme_bw() +
    theme(legend.position = "none")
  # combine the plots
  p <- patchwork::wrap_plots(p_d, p_sc, nrow = 1)
  file <- paste0("figs/rips_", i, ".pdf")
  Cairo::CairoPDF(file = file, width = 8, height = 4)
  print(p)
  crop::dev.off.crop(file)
  return(file)
}

make_barcode <- function(data) {
  ph1 <- TDAstats::calculate_homology(data, dim = 0)
  bcd <- TDAstats::plot_barcode(ph1)
  #pst <- TDAstats::plot_persist(ph1) + xlab("Birth") + ylab("Death")
  #patchwork::wrap_plots(bcd, pst, nrow = 1)
  bcd
}
