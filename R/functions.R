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
