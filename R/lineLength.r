#' @export lineLength
#' 
#' @title Line length 
#' 
#' @description An all-R routine that computes total length of all 
#' lines in a \code{SpatialLines*} object.
#' 
#' @param x A spatial object inheriting from \code{SpatialLines}, \code{SpatialPolygons}, 
#' or \code{SpatialPoints}.
#' 
#' @param byid Whether to return lengths of individual spatial objects (TRUE) 
#' or the sum of all length (FALSE). 
#' 
#' @details Provides the same answer as \code{rgeos::gLength}, but is 
#' all-R (does not require rgeos Java library) and does not 
#' fire a warning if \code{x} is un-projected (i.e., lat-long).
#' 
#' @return 
#' If \code{byid==TRUE}, a vector containing the lengths of individual 
#' spatial objects 
#' (the points, lines, or polygons) is returned.  If \code{byid=FALSE}, 
#' the total length of all spatial objects is returned (a single number). 
#' 
#' If \code{x} inherits from \code{SpatialPoints}, returned 
#' value is 0.  If \code{x} inherits from \code{SpatialLines}, returned 
#' value contains line lengths or the sum of line lenghs in \code{x}. 
#' If \code{x} inherits from \code{SpatialPolygons}, returned
#' value contains lengths of the perimeter of all polygons, or 
#' the sum of perimeters, in \code{x}. When \code{x} contains polygons with 
#' holes, the perimeter of the holes is included (i.e., perimeter of holes
#' is postive, not negative).    
#' 
#' 
#' Units of the returned value are same as units of coordinates 
#' in \code{x}.  E.g., 
#' meters if coordinates in \code{x} are UTM meters, 
#' decimal degrees if coordinates in \code{x} are lat-long decimial 
#' degrees.
#' 
#' @author Trent McDonald
#' 
#' @seealso \code{sp::SpatialLines-class}
#' 
#' @examples 
#' 
#' # Length of Hawaii coastline, in kilometers
#' l <- lineLength( HI.coast ) / 1000
#' 

lineLength <- function(x,byid=FALSE){
  
  if(inherits(x,"SpatialPoints")) return(0)
  
  # Extract all coordinates.    
  # Keep in mind that x@lines[[i]] and x@polygons[[1]] can contain several objects
  if(inherits(x,"SpatialPolygons")){
    tmp <- lapply( unlist(x@polygons), slot, "Polygons")
  } else {
    tmp <- lapply( unlist(x@lines), slot, "Lines")
  }
  tmp <- lapply( unlist(tmp), slot, "coords")
  
  SDraw.LineLength <- function(cc){
    # internal function to compute length of a bunch of coordinates
    x <- cc[,1]
    y <- cc[,2]
    sum( sqrt(diff(x)^2 + diff(y)^2) )
  }
  
  tmp <- sapply(tmp, SDraw.LineLength)
  
  if( !byid ) tmp <- sum(tmp)
  
  tmp
}