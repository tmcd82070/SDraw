#' @export hal.polygon
#' 
#' @title  Draws a Halton Lattice sample from an area (polygon) resource.
#' 
#' @description  Draws a Halton Lattice sample from a \code{SpatialPolygons} object. 
#' 
#' @details  \emph{Brief description of Halton Lattice sampling for polygons:} 
#' Given a set of Halton Lattice parameters \code{J}, \code{bases}, \code{eta}, 
#' and  \code{triangular}, the union of all polygons is discretized by placing 
#' a Halton lattice of \code{prod(bases^J)} boxes, each containing  \code{prod(eta)}
#' points, over the bounding box of all polygons. Points outside any polygon are
#' dropped, and the resulting Halton lattice frame is passed to \code{hal.point} 
#' for sampling.  
#' 
#' @note The number of points in the Halton lattice becomes large very quickly. 
#' The number of points in the Halton lattice is \code{prod(bases^J)*prod(eta)}.
#' 
#' @param n Sample size.  Number of locations to draw from the set of points
#' contained in \code{shp}.
#' @param shp A SpatialPointss or SpatialPointssDataFrame object. This object must
#' contain at least 1 point.  
#' @param J A 2X1 vector of base powers.  \code{J[1]} is for horizontal,
#' \code{J[2]} for vertical dimension. \code{J} determines the size and shape
#' of the lowest level of Halton boxes. If \code{J=NULL} (the default),
#' \code{J} is choosen so that Halton boxes are as square as possible.
#' @param eta A 2X1 vector specifying the number of points to add in the 
#' horizontal and vertical dimensions of each Halton box.  e.g., if 
#' \code{eta} = c(3,2), a grid of 3 (horizontal) by 2 (vertical) points is 
#' added inside each Halton box. 
#' @param triangular A boolean scalar. If TRUE, odd horizontal rows of the 
#' Halton lattice are moved one-quarter a Halton box width to the right, while 
#' even rows of the Halton lattic are moved one-quarter of a Halton box width 
#' to the left. This creates a triangular Halton lattice over the 
#' bounding box of the polygons. 
#' If FALSE, a rectangluar Halton lattice is constructed. 
#' @param bases 2X1 vector of Halton bases.  These must be co-prime.

#' @return A \code{SpatialPointsDataFrame} containing locations in the HAL sample, in
#' the order they are to be visited.  A 'siteID' attribute is attached to each
#' point (in the embedded data frame) and gives the HAL ordering of the sample
#' (i.e., sort on 'siteID' to get proper HAL order).  In addition, if the input
#' object has an attached data frame (i.e., is a \code{SpatialPolygonsDataFrame}), the
#' attrributes of the polygon in which each HAL point fell is attached in the
#' associated data frame. The ID  of the polygon in \code{shp} in which each
#' point fell is an attribute of the output points.
#' @author Trent McDonald
#' @seealso \code{\link{hal.line}}, \code{\link{hal.point}}, \code{\link{spsample}}
#' @keywords design survey
#' @examples
#' 
#'   #   Draw sample of Hawaii coastline
#'   #   This takes approximately 30 seconds to run
#'   data(WA)
#'   samp <- hal.polygon( 100, WA )
#'   plot(WA)
#'   points( samp, pch=16, col="red" )
#'   
#'   #   Different lattice topology
#'   samp <- hal.polygon( 100, WA, J=c(8,2), eta=c(2,1), triangular=TRUE)
#'   points( samp, pch=15, col="blue")
#'   
#' 

hal.polygon <- function( n, shp, J=NULL, eta=c(1,1), triangular=FALSE, bases=c(2,3)){
  #
  #   Draw a HAL sample from a polygon in a shapefile.
  #
  #   input:
  #   n = desired sample size,
  #   shp = a SpatialPolygons* object, according to package sp.
  #   J = 2X1 vector of base powers.  J[1] is for horizontal, J[2] for vertical dimension
  #     J determines the size and shape of the lowest level of Halton boxes. If J=NULL (the default), 
  #     J is choosen so that Halton boxes are as square as possible. 
  #   eta = 2X1 vector of number of points to add inside each Halton box.  e.g., if 
  #     eta = c(3,2), a small grid of 3 by 2 points is added inside each Halton box. eta[1] is for 
  #     horizontal dimension, eta[2] is for vertical dimension. 
  #   triangular = boolean, if TRUE, construct a triangular grid. If FALSE, construct rectangluar grid.
  #   bases = 2X1 vector of Halton bases.  These must be co-prime. 
  
  #   Output:
  #   A SpatialPOints* object containing n HAL sample points, in order. 

  #   Details
  #   If J is missing, the approximate frame size or approximate lattice size is set to 100 times 
  #     the number of samples requested. If J is NULL, the algorithm attempts to place 100*n points in the bounding box 
  #     using Halton boxes that are as close to square as possible.  The lattice size is not exact, but is a target. 
  
  
  #   Check n
  if( n < 1 ){
    n <- 1
    warning("Sample size less than one has been reset to 1")
  }

  # If lattice is not specified, set frame size to some multiple of n
  if(is.null(J)){
    N <- 100*n
  } else {
    N <- NULL
  }
  
  # Construct Halton lattice  
  hl.points <- halton.lattice.polygon( shp, N, J, eta, triangular, bases )
  
  # Now that we have points, we can draw a HAL point sample. 
  samp <- hal.point( n, hl.points, attr(hl.points, "J"), attr(hl.points, "bases") )
  
  samp
  
  
}


# ----- Some examples

# samp<- hal.polygon(1000, WA.utm)
# plot(WA.utm)
# points(samp)
