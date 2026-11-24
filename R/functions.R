# General helpers

set_ggplot_options <- function() {
  colours <- c(
    "#F39B00",
    "#65775c",
    "#009E73",
    "#CC79A7",
    "#E69F00",
    "#56B4E9",
    "#C14b14"
  )
  options(
    ggplot2.discrete.colour = colours,
    ggplot2.discrete.fill = colours
  )
}

# Convert ggplots to grobs with identical widths and heights of their non-panel
# parts (axis labels, titles, legends), so their panels line up exactly when
# the plots are overlaid or shown in sequence
align_ggplots <- function(plots) {
  grobs <- lapply(plots, ggplotGrob)
  widths <- do.call(grid::unit.pmax, lapply(grobs, `[[`, "widths"))
  heights <- do.call(grid::unit.pmax, lapply(grobs, `[[`, "heights"))
  lapply(grobs, function(g) {
    g$widths <- widths
    g$heights <- heights
    g
  })
}

# Card that shows a label for the item under the mouse in an interactive
# (ggiraph) plot on the same slide. `labels` is a named vector keyed by the
# plot's data_id; the card shows "key: label" (labels may contain HTML, and
# `sep` replaces the ":"). Keys in `alert` are shown in the alert style.
# `class` adds classes to the card, e.g. to position it. With `connect`, a line
# joins the hovered item to the nearest point of the card.
hover_card <- function(labels, alert = NULL, sep = ":", class = NULL, connect = FALSE) {
  htmltools::tagList(
    htmltools::div(
      class = paste(c("hover-card", class), collapse = " "),
      `data-labels` = jsonlite::toJSON(as.list(labels), auto_unbox = TRUE),
      `data-alert` = jsonlite::toJSON(as.character(alert)),
      `data-sep` = sep,
      `data-connect` = if (connect) "true"
    ),
    htmltools::tags$script(htmltools::HTML(
      "
document.addEventListener('mouseover', function (e) {
  var item = e.target.closest('.girafe [data-id]');
  var section = e.target.closest('section');
  var card = section && section.querySelector('.hover-card');
  if (!card) return;
  var key = item && item.getAttribute('data-id');
  var label = key && JSON.parse(card.dataset.labels)[key];
  var line = section.querySelector('.hover-connector');
  if (!label) {
    card.classList.remove('show');
    if (line) line.classList.remove('show');
    return;
  }
  card.innerHTML = '<strong>' + key + card.dataset.sep + '</strong> ' + label;
  card.classList.toggle('is-alert', JSON.parse(card.dataset.alert).includes(key));
  card.classList.add('show');
  if (!card.dataset.connect) return;
  if (!line) {
    line = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    line.setAttribute('class', 'hover-connector');
    line.innerHTML = '<line/><circle/>';
    section.appendChild(line);
  }
  // Slide coordinates: client coordinates relative to the section, unscaled
  var s = section.getBoundingClientRect();
  var scale = s.width / section.offsetWidth;
  var p = item.getBoundingClientRect();
  var c = card.getBoundingClientRect();
  var x = (p.left + p.width / 2 - s.left) / scale;
  var y = (p.top + p.height / 2 - s.top) / scale;
  var clamp = function (v, lo, hi) { return Math.min(Math.max(v, lo), hi); };
  var x2 = clamp(x, (c.left - s.left) / scale + 12, (c.right - s.left) / scale - 12);
  var y2 = clamp(y, (c.top - s.top) / scale, (c.bottom - s.top) / scale);
  var l = line.querySelector('line');
  l.setAttribute('x1', x); l.setAttribute('y1', y);
  l.setAttribute('x2', x2); l.setAttribute('y2', y2);
  var o = line.querySelector('circle');
  o.setAttribute('cx', x); o.setAttribute('cy', y); o.setAttribute('r', 9);
  line.classList.add('show');
});"
    ))
  )
}

# Output of library(weird), as printed when the package is attached.
# `lockfile` is unused: it makes the target depend on the package library.
weird_attach_message <- function(lockfile = NULL) {
  callr::r(function() {
    options(cli.num_colors = 1, cli.width = 72, width = 72)
    msgs <- character()
    withCallingHandlers(
      library(weird),
      packageStartupMessage = function(m) {
        msgs <<- c(msgs, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    sub("\n$", "", paste(msgs, collapse = ""))
  })
}
