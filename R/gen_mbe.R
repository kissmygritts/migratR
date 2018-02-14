#' Generates a minimum bounding ellipse for a point set
#'
#' \code{gen_mbe} generates a minimum bounding ellipse for the given point set.
#' A minimum bounding ellipse is the smallest possible ellipse that contains the entire point set.
#'
#' @param x A vector of the UTM Easting coordinates of a point set
#' @param y A vector of the UTM Northing coordinates of a point set
#'
#' @return An ellipse object (see \code{?ellipse}) with the added values of the
#'   eccentricity of the ellippse, and the semi-major and semi-minor axes.
#'
gen_mbe <- function(x, y) {
  ellipse <- cluster::ellipsoidhull(
    matrix(c(x, y), ncol = 2)
  )

  eigen_vals <- eigen(ellipse$cov)$values
  a <- eigen_vals[1]
  b <- eigen_vals[2]

  ellipse$eccentricity <- sqrt(1 - ((b^2) / (a^2)))
  ellipse$a <- a
  ellipse$b <- b

  return(ellipse)
}
