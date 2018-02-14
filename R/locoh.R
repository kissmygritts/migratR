#' Creates a set of Local Convex Hulls
#'
#' \code{locoh} creates a set of Local Convex Hulls (LoCoH). LoCoHs are
#' minimum are MCPs around a selected point set from the dataset. The number
#' of points to include in a LoCoH defaults to 25 and can be controlled by
#' the \code{n_points} parameter (in dev). You will be able to select the
#' method by which LoCoH are created (k, r, a). \code{locoh} also generates a
#' minimum bounding ellipse for each LoCoH The summary details about each LoCoH
#' are added to the summary data.frame.
#'
#'
#' @param dat A \code{data.frame} that contains the XY, ID, and timestamp of
#'   the dataset
#' @param cols A vector of column names for the following fields in the \code{data.frame}
#'   x, y, timestamp, id. The default is \code{c('x', 'y', 'timestamp', 'id')}.
#'   The entire vector must always be given if any default value is changed.
#'
#' @return A list of two \code{data.frames}. The first is a \code{data.frame} that
#'   contains the boundaries for all the LoCoHs. The second \code{data.frame}
#'   contains summary details for each LoCoH. These two \code{data.frames} share
#'   an id field, \code{id}.
#'
locoh <- function(dat, cols = c('x', 'y', 'timestamp', 'ndowid'), knn = 20) {
  df <- data.frame(
    dat[, cols[1]],
    dat[, cols[2]],
    dat[, cols[3]],
    dat[, cols[4]]
  )
  colnames(df) <- c('x', 'y', 'date', 'id')

  ## calculate distance matrix
  dist_matrix <- as.matrix(dist(df[, 1:2]))

  ## initializing empty dataframes
  mcp_df <- data.frame()
  summary_df <- data.frame()

  ## the magic happens here
  for (i in seq_along(1:nrow(dist_matrix))) {
    ## mcp of the local hull
    parent_point <- df[i, ]
    nn <- df[order(dist_matrix[i, ])[1:26], ]
    mcp <- gen_mcp(nn$x, nn$y)
    nn_mcp_df <- mcp$mcp
    nn_mcp_df$id <- rep(i, nrow(nn_mcp_df))
    mcp_df <- rbind(mcp_df, nn_mcp_df)

    ## ellipse of the local hull
    elps <- gen_mbe(nn$x, nn$y)

    smry_row <- data.frame(
      'id' = i,
      'start_date' = parent_point[, 'date'],
      'centroid_x' = mcp$centroid$x,
      'centroid_y' = mcp$centroid$y,
      'area' = mcp$area,
      'perimeter' = mcp$perimeter,
      'par' = mcp$perimeter / mcp$area,
      'eccentricity' = elps$eccentricity,
      'frac' = (2 * log(mcp$perimeter) / log(mcp$area)),
      'shape' = mcp$perimeter / min(mcp$perimeter)
    )
    summary_df <- rbind(summary_df, smry_row)
  }
  return(list(
    mcp_df = mcp_df,
    summary_df = summary_df
  ))
}
