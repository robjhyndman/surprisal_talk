# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

set_ggplot_options <- function() {
  colours <- c(
    "#D55E00",
    "#0072B2",
    "#009E73",
    "#CC79A7",
    "#E69F00",
    "#56B4E9",
    "#F0E442"
  )
  options(
    ggplot2.discrete.colour = colours,
    ggplot2.discrete.fill = colours
  )
}

# Convert lookout object to a data frame with outlier labels
as.data.frame.lookoutliers <- function(object, ...) {
  varnames <- colnames(object$data)
  if (is.null(varnames)) {
    varnames <- paste0("V", seq(NCOL(object$data)))
  }
  X <- as.data.frame(object$data)
  colnames(X) <- varnames
  X$outliers <- which_outliers(object)
  return(X)
}

# Take lookout object and return logical vector of which points are outliers
which_outliers <- function(object) {
  preds <- rep(FALSE, length(object$outlier_probability))
  preds[object$outliers[, 1]] <- TRUE
  preds
}

# Function to calculate difference metrics
diff_metrics <- function(act, pred) {
  # positives to be denoted by 1 and negatives with 0
  stopifnot(length(act) == length(pred))

  n <- length(act)
  tp <- sum((act == 1) & (pred == 1))
  tn <- sum((act == 0) & (pred == 0))
  fp <- sum((act == 0) & (pred == 1))
  fn <- sum((act == 1) & (pred == 0))
  prec <- (tp + tn) / n
  sn <- tp / (tp + fn)
  sp <- tn / (tn + fp)

  tpr <- tp / (tp + fn)
  tnr <- tn / (tn + fp)
  fpr <- fp / (fp + tn)
  fnr <- fn / (fn + tp)

  precision <- if_else(
    (tp + fp) == 0,
    0,
    tp / (tp + fp)
  )
  recall <- tp / (tp + fn)
  fmeasure <- if_else(
    (precision == 0) & (recall == 0),
    0,
    2 * precision * recall / (precision + recall)
  )

  if (tp == 0) {
    tpr <- 0
  }
  if (fn == 0) {
    fnr <- 0
  }
  if (tn == 0) {
    tnr <- 0
  }
  if (fp == 0) {
    fpr <- 0
  }

  out <- data.frame(
    N = n,
    true_pos = tp,
    true_neg = tn,
    false_pos = fp,
    false_neg = fn,
    true_positive_rate = tpr,
    true_negative_rate = tnr,
    false_positive_rate = fpr,
    false_negative_rate = fnr,
    accuracy = prec,
    sensitivity = sn,
    specificity = sp,
    gmean = sqrt(sn * sp),
    precision = precision,
    recall = recall,
    fmeasure = fmeasure
  )

  return(out)
}

# Set up a tibble to store difference metrics
set_up_diff_metrics <- function(nrows) {
  # Create a tibble to store the results
  df <- diff_metrics(0, 0)
  as_tibble(df[rep(1, nrows), ])
}


# Convert lookout object to a data frame with outlier labels
as.data.frame.lookoutliers <- function(object, ...) {
  varnames <- colnames(object$data)
  if (is.null(varnames)) {
    varnames <- paste0("V", seq(NCOL(object$data)))
  }
  X <- as.data.frame(object$data)
  colnames(X) <- varnames
  X$outliers <- which_outliers(object)
  return(X)
}

# Apply dobin to data frame with labelled outliers
as_dobin <- function(object) {
  labels <- object$Points
  object$Points <- NULL
  dobout <- dobin::dobin(object)
  dobX <- dobout$coords
  colnames(dobX) <- paste0("D", seq(NCOL(dobX)))
  as_tibble(dobX) |>
    mutate(labels = labels)
}

get_legend <- function(plot) {
  tmp <- ggplot_gtable(ggplot_build(plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if (length(leg) > 0) {
    legend <- tmp$grobs[[leg]]
    return(patchwork::wrap_elements(panel = legend))
  } else {
    return(patchwork::plot_spacer())
  }
}


# Function to plot data on left, anomaly rates on right with legend in bottom left quadrant
experiment_plot <- function(g1, g2, filename, height = 5, width = 8, ...) {
  #legend1 <- get_legend(g1)
  #legend2 <- get_legend(g2)
  #combined_legends <- patchwork::wrap_plots(legend1, legend2, ncol = 2)

  left <- g1 + theme(legend.position = "bottom") + guides(color = guide_legend(nrow = 2, byrow = TRUE))
  right <- g2 + theme(legend.position = "none")

  # Create figure
  patchwork::wrap_plots(left, right, ncol = 2, widths = c(1, 1))
}

# Function to fit smooth loess curve and get predictions with results truncated to (0,1)
truncated_smooth <- function(x, y) {
  fit <- loess(y ~ x, span = 0.75, data = data.frame(x = x, y = y))

  x_range <- range(x, na.rm = TRUE)
  pred_data <- data.frame(x = seq(x_range[1], x_range[2], length.out = 100))
  pred <- predict(fit, pred_data, se = TRUE)

  # Calculate confidence intervals
  pred_data$fit <- pred$fit
  pred_data$se <- pred$se.fit
  pred_data$lower <- pred$fit - 1.96 * pred$se.fit
  pred_data$upper <- pred$fit + 1.96 * pred$se.fit

  # Truncate fits and confidence intervals to (0,1)
  pred_data$fit <- pmax(pmin(pred$fit, 1), 0)
  pred_data$lower <- pmax(0, pred_data$lower)
  pred_data$upper <- pmin(1, pred_data$upper)

  return(pred_data)
}
