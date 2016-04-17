#' @export halton.lattice.polygon
#' 
#' @title Halton lattice inside a \code{SpatialPolygon*} object.
#' 
#' @description Constructs a lattice of Halton boxes (a Halton lattice) inside a 
#' \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} object. This is a wrapper for
#' \code{halton.lattice}, which does all the hard work. 
#' 
#' @param x A \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} object. 
#' 
#' @param J A 2X1 vector of base powers which determines the size and shape 
#' of the Halton boxes. See additional description in help for 
#' \code{\link{hal.polygon}} function.  
#' 
#' @param N Approximate number of points to place in the lattice.  If \code{J} 
#' is specified, it takes precedence.  If \code{J} is NULL, the 
#' algorithm attempts to place \code{N} points in the bounding box 
#' using Halton boxes that are as close to square as possible.  
#' This \code{N} is not exact, but is a target. 
#' 
#' @param eta A 2X1 vector of the number of points to add inside each Halton box.  
#' e.g., if \code{eta} = \code{c(3,2)}, a small grid of 3 by 2 points is 
#' added inside each Halton box. \code{eta[1]} is for the
#' horizontal dimension, \code{eta[2]} is for the vertical dimension. 
#' 
#' @param triangular = boolean, if TRUE, construct a triangular grid. 
#' If FALSE, construct rectangluar grid.  See help for \code{\link{hal.polygon}}.
#' 
#' @param bases A 2X1 vector of Halton bases.  These must be co-prime. 
#' 
#' @return A \code{SpatialPointsDataFrame} containing locations in the Halton lattice
#' 
#'  Attributes of the points are: 
#' \itemize{
#'   \item \code{latticeID}: A unique identifier for every point.  ID's are integers
#'   numbering points in row-major order from the south.  
#'
#'   \item \code{geometryID}: The ID of the polygon in \code{x} containing each 
#'   point.  The 
#'   ID of polygons in \code{x} are \code{row.names(geometry(x))}. 
#'   \item Any attributes of the original polygons (in \code{x}). 
#' }
#'
#' Additional attributes of the output object, beyond those which 
#' make it a \code{SpatialPointsDataFrame}, are:
#' \itemize{
#'    \item \code{J}: the \code{J} vector used to construct the lattice. 
#'      This is either the input \code{J} or the computed \code{J} when 
#'      only \code{N} is specified. 
#'    \item \code{eta}: the \code{eta} vector used in the lattice.
#'    \item \code{bases}: the bases of the van der Corput sequences used in the lattice, 
#'      one per dimension.
#'    \item \code{triangular}: Whether the lattice is triangular or square.
#'    \item \code{hl.bbox}: the bounding box surrounding the input \code{x} object. 
#'      This is saved because bounding box of the return object is not the 
#'      same as the bounding box of \code{x} (i.e., \code{bbox(return)} \code{!=} 
#'      \code{bbox(x)}).
#' }
#' 
#' @details This routine is called internally by \code{hal.polygon}, and is not 
#' normally called by the user. 
#' 
#' @author Trent McDonald
#' @seealso \code{\link{hal.polygon}}, \code{\link{halton.lattice}}
#' @keywords design survey
#' @examples
#'
#' # Take and plot Halton lattice to illustrate
#' WA.hgrid <- halton.lattice.polygon( WA, J=c(3,2), eta=c(3,2), triangular=T )
#' plot(WA)
#' points(WA.hgrid, pch=16, cex=.5, col="red" )
#' 
#' # Plot the Halton boxes
#' tmp.J <- attr(tmp,"J")
#' tmp.b <- attr(tmp,"bases")
#' tmp.bb <- attr(tmp,"hl.bbox")
#' 
#' for(d in 1:2){
#'   tmp2 <- tmp.bb[d,1] + (0:(tmp.b[d]^tmp.J[d]))*(diff(tmp.bb[d,]))/(tmp.b[d]^tmp.J[d])
#'   if( d == 1){
#'       abline(v=tmp2, col="blue")
#'   } else{
#'       abline(h=tmp2, col="blue")
#'   }
#' }
#' 
#' # To explore, re-run the above changing J, eta, and triangular, 

halton.lattice.polygon <- function(x, N=10000, J=NULL, eta=c(1,1), triangular=FALSE, bases=c(2,3)){

  if( is.null(J)){
    # Get area of object, subtracting holes.  This requires rgeos package.
    sp.area <- SDraw:::polygonArea( x )
    
    # Bump up the N requested to account for area outside polygons, but within bbox.
    bb.area <- prod(apply(bbox(x),1,diff))
    N <- N * bb.area / sp.area
  }

  # Get the halton lattice inside the bounding box, this does all the hard work
  hl <- halton.lattice( bbox(x), N, J, eta, triangular, bases)

  
  # Convert lattice to SpatialPoints object
  hl.points <- SpatialPoints(hl, proj4string=CRS(proj4string(x)))
  
  # Do point-in-polygon selection to get just those inside a polygon.
  # Make sue x has a data frame attached with attributes we want 
  # so over() works
  if( inherits(x, "SpatialPolygonsDataFrame") ){
    #   x has a data frame
    df <- data.frame( sampleID=1:length(x), geometryID=row.names(x), data.frame(x) )
  } else {
    df <- data.frame( sampleID=1:length(x), geometryID=row.names(x),  row.names=row.names(x) )
  }
  
  x <- SpatialPolygonsDataFrame( x, data=df )

  pip <- over( hl.points, x )

  hl.points <- hl.points[!is.na(pip$sampleID),]
  pip <- pip[!is.na(pip$sampleID),]
  hl.points <- SpatialPointsDataFrame(hl.points, data=pip, proj4string = CRS(proj4string(x)))

  # Add attributes
  attr(hl.points,"J") <- attr(hl, "J")
  attr(hl.points,"eta") <- attr(hl, "eta")
  attr(hl.points,"bases") <- attr(hl, "bases")
  attr(hl.points,"triangular") <- attr(hl, "triangular")
  attr(hl.points,"hl.bbox") <- attr(hl, "hl.bbox")  # save original bbox, because bbox(hl.points) != bbox(x)
  
  hl.points
  
}


# ---------------------------------------------
# # Example of this fuctions use and plotting
#plot(WA)
#plot(WA.ll)
# 
# tmp <- halton.lattice.polygon( WA.nodf, N=200, eta=c(3,2), triangular=F )
# tmp <- halton.lattice.polygon( WA, N=200, eta=c(3,2), triangular=F )
#tmp <- halton.lattice.polygon( WA.ll, J=c(3,2), eta=c(3,2) )
# # 
#points(tmp, pch=16, cex=.5, col="red" )
#
# tmp.J <- attr(tmp,"J")
# tmp.b <- attr(tmp,"bases")
# tmp.bb <- attr(tmp,"hl.bbox")
# 
# for(d in 1:2){
#   tmp2 <- tmp.bb[d,1] + (0:(tmp.b[d]^tmp.J[d]))*(diff(tmp.bb[d,]))/(tmp.b[d]^tmp.J[d])
#   if( d == 1){
#       abline(v=tmp2, col="blue")
#   } else{
#       abline(h=tmp2, col="blue")
#   }
# }
# 
## Compute Voronoi polygons and variance of sizes
# library(dismo)
# tmp.v <- voronoi(tmp)
# plot(WA.utm)
# plot(tmp.v, add=T, col=rainbow(length(tmp.v)))
# plot(WA.utm, add=T)
# plot(tmp, pch=16, add=T)
# tmp.v.sizes <- unlist(lapply( tmp.v@polygons, function(x){x@area} ))
# cat("Standard deviation of tesselation sizes (m)\n")
# print(sd(tmp.v.sizes))
