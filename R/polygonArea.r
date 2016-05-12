#' @export polygonArea
#' 
#' @title Polygon area
#' 
#' @description An all-R routine that computes area of all 
#' polygons in a \code{SpatialPolygons*} object, 
#' taking account of holes.
#' 
#' @param x A spatial object inheriting from \code{SpatialPolygons}
#' 
#' @details Provides the same answer as \code{rgeos::gArea}, but is 
#' all-R (does not require rgeos Java library) and does not 
#' fire a warning if \code{x} is un-projected (i.e., lat-long).
#' 
#' @return Area of all polygons in \code{x}, taking account of holes. 
#' Units of area are squared units of coordinates in \code{x}.  E.g., 
#' square meters if coordinates in \code{x} are UTM meters, square
#' decimal degrees if coordinates in \code{x} are lat-long decimial 
#' degrees.
#' 
#' @author Trent McDonald
#' 
#' @seealso \code{sp::SpatialPolygons-class}
#' 
#' @examples 
#' 
#' # Area of Washington state, in hectares
#' a <- polygonArea( WA ) / (100*100)
#' 

polygonArea <- function(x){

  print(class(x))
  
  if(inherits(x, "SpatialPolygons")){
    holes <- sapply(x@polygons, function(xx){sapply(xx@Polygons,slot,"hole")})
    areas <- sapply(x@polygons, function(xx){sapply(xx@Polygons,slot,"area")})
  } else if( inherits( x, "Polygons") ){ # a single polygons list
    holes <- sapply(x@Polygons,slot,"hole")
    areas <- sapply(x@Polygons,slot,"area")
  } else {  # assume we have a list of polygons, an @polygons list
    holes <- sapply(x,function(xx){sapply(xx@Polygons,slot,"hole")})
    areas <- sapply(x,function(xx){sapply(xx@Polygons,slot,"area")})
  }

  sum((-2*unlist(holes)+1) * unlist(areas))
  
}