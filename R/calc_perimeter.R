#' Calculate the perimeter of a polygon
#'
#' \code{calc_perimeter} returns the perimeter of simple polygons. This function
#' is used internally to calculate the perimeter of the local convex hulls.
#'
#' @param shape The XY coordinates of a polygon perimeter
#'
#' @return The perimeter of the given polygon

calc_perimeter <- function(shape) {
  d <- as.matrix(dist(shape))
  dg <- c()

  for (i in seq_along(1:(nrow(d) - 1))) {
    j <- ifelse(i == nrow(d), 1, i + 1)
    dg <- c(dg, d[j, i])
  }
  return(Reduce(`+`, dg))
}
