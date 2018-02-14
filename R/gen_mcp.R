#' Generates a Minimum Convex Polygon
#'
#' \code{gen_mcp} generates the minimum convex polygon given x and y vectors
#' of a point set. The percentage of points to include can be controlled with
#' the \code{pct} parameter.
#'
#' @param x A vector of the UTM Easting coordinates of a point set
#' @param y A vector of the UTM Northing coordinates of a point set
#' @param pct A number between 0 and 1 that determines the percent of points
#' used to generate the MCP
#'
#' @return A list that contains the MCP border, the centroid of the MCP,
#' the area of the MCP, and the perimeter of the MCP
#'
gen_mcp <- function (x, y, pct = 1) {
  centroid <- c('x' = mean(x), 'y' = mean(y))
  distance_to_centroid <- sqrt((x - centroid[1]) ^ 2 + (y - centroid[2]) ^ 2)
  indx <- 1:length(distance_to_centroid)

  ## get points withing 99th quantile
  pct <- indx[distance_to_centroid <= quantile(distance_to_centroid, pct)]
  mcp_x <- x[pct]
  mcp_y <- y[pct]

  ## calculate border
  mcp_border <- chull(mcp_x, mcp_y)
  mcp_border_x <- mcp_x[mcp_border]
  mcp_border_y <- mcp_y[mcp_border]

  ## concat first point to complete ring
  df <- data.frame(
    'x' = c(mcp_border_x, mcp_border_x[1]),
    'y' = c(mcp_border_y, mcp_border_y[1])
  )

  ## spatial polygon for area
  p_area <- sp::Polygon(df)@area
  p_perim <- calc_perimeter(df)

  return(list(
    mcp = df,
    centroid = data.frame('x' = centroid[1], 'y' = centroid[2]),
    area = p_area,
    perimeter = p_perim
  ))
}
