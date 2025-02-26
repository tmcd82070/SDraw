#' @title spreadPts - Spread points for faster sampling
#' 
#' @description
#' Spread out the points of a 'sf' POINTS object so that 
#' distances between them are uniform-ish. This speeds 
#' BAS sampling of point resources. 
#' 
#' @param x A 'sf' object containing POINT geometries
#' 
#' @return A 'sf' object containing the same number 
#' of POINT geometries as the input, but with the points
#' spread out. 
#' 
#' @export
#' 
spreadPts <- function(x){
  
  # all points must be in a single MULTIPOINT object
  xx <- x |>
    dplyr::summarise()
  
  vPolys <- sf::st_voronoi(xx) |>
    sf::st_collection_extract() # One polygon per point
  
  vPolyCentroids <- vPolys |>
    sf::st_centroid()
  
  # Merge back in attributes of x b/c st_voroni drops them
  attrs.x <- x |>
    sf::st_drop_geometry()
  ind <- sf::st_intersects(x, vPolys)
  ind <- do.call(c, ind)
  out <- attrs.x |>
    dplyr::slice(ind) |>
    dplyr::bind_cols( vPolyCentroids ) |>
    sf::st_set_geometry(attr(x,"sf_column"))
  
  out
}