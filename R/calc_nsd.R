#' Calculate Net Squared Displacement
#'
#' @param x A vector of the UTM Easting coordinate
#' @param y A vector of the UTM Northing coordinate
#' @param normalize A logical value that indicates whether to normalize the result. The default is FALSE
#'
#' @return A vector of the NSD values for each (x, y) vector pair.

calc_nsd <- function (x, y, normalize = FALSE) {
  nsd <- (x - x[1])^2 + (y - y[1])^2

  if (normalize) {
    nsd <- (nsd - min(nsd)) / (max(nsd) - min(nsd))
  }

  return(nsd)
}
