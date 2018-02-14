#' Recursively union a dataframe of polygons
#'
r_union <- function (poly_df, id_idx, rm_holes = TRUE) {
  ## create list of data.frames subset by id
  polydfs <- split(poly_df, f = poly_df[, id_idx])

  ## create list of polygons
  p <- lapply(seq_along(polydfs), function(i) {
    sp::SpatialPolygons(list(sp::Polygons(list(
      sp::Polygon(polydfs[[i]][, c('x', 'y')])
    ),
    ID = unique(polydfs[[i]][, id_idx]))))
  })

  ## union to create the boundary
  boundary <- Reduce(rgeos::gUnion, p)

  ## remove holes, if requested
  if (rm_holes) {
    boundary <- sp::SpatialPolygons(list(
      sp::Polygons(
        Filter(function (f) {
          f@ringDir == 1
        }, boundary@polygons[[1]]@Polygons),
        ID = 1
      )
    ))
  }

  return(boundary)
}

# plot(r_union(seasonal$mcp_df, 3, rm_holes = T))
