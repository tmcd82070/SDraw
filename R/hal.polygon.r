#' @export hal.polygon
#' 
#' @title  Draws a Halton Lattice sample from an area (polygon) resource.
#' 
#' @description  Draws a Halton Lattice sample from a \code{SpatialPolygons*} object. 
#' 
#' @details  \emph{A brief description of Halton Lattice sampling for polygons:} 
#' Given a set of Halton Lattice parameters \code{J}, \code{bases}, \code{eta}, 
#' and  \code{triangular}, the bounding box around all polygons is partitioned using  
#' a Halton lattice of \code{prod(bases^J)} boxes, each containing  \code{prod(eta)}
#' points. Points not inside at 
#' least one polygon are
#' dropped. See \code{\link{halton.lattice.polygon}}.  
#' The resulting points (the Halton lattice frame) are passed to \code{hal.point} 
#' and sampled using the HAL method for points.  
#' 
#' @note The number of points in the Halton lattice becomes large very quickly. 
#' The number of points in the Halton lattice is \code{prod(bases^J)*prod(eta)}.
#' 
#' @param n Sample size.  Number of locations to draw from the union of all 
#' polygons contained in \code{x}.
#' 
#' @param x A \code{SpatialPoints} or \code{SpatialPointsDataFrame} object. 
#' This object must contain at least 1 polygon.  
#' 
#' @param J A 2X1 vector of base powers.  \code{J[1]} is for horizontal,
#' \code{J[2]} for vertical dimension. \code{J} determines the size and shape
#' of the smallest Halton boxes. There are \code{bases[1]^J[1]} vertical columns 
#' of Halton boxes over \code{x}'s bounding box, and \code{bases[2]^J[2]} 
#' horizontal rows of Halton boxes over the bounding box, for a total 
#' of \code{prod(bases^J)} total boxes.  The dimension of each box is 
#' \code{c(dx,dy)/(bases^J)}, where \code{c(dx,dy)} are the horizontal and 
#' vertical extents of \code{x}'s bounding box.  If \code{J=NULL} (the default),
#' \code{J} is choosen so that approximately \code{1000*n} Halton boxes are placed 
#' in the bounding box of polygons, each as 
#' square as possible and each containing one point, 
#' 
#' @param eta A 2X1 vector specifying the number of points to add in the 
#' horizontal and vertical dimensions of each Halton box.  e.g., if 
#' \code{eta} = c(3,2), a grid of 3 (horizontal) by 2 (vertical) points is 
#' added inside each Halton box. 
#' 
#' @param triangular A boolean scalar. If TRUE, odd horizontal rows of the 
#' Halton lattice are moved one-quarter a Halton box width to the right, while 
#' even rows of the Halton lattic are moved one-quarter of a Halton box width 
#' to the left. This creates a triangular Halton lattice over the 
#' bounding box of the polygons. 
#' If FALSE, a rectangluar Halton lattice is constructed. 
#' 
#' @param bases 2X1 vector of Halton bases.  These must be co-prime.
#' 
#' @param init.n.factor A
#' scalar factor controling the approximate number of points 
#' in the Halton lattice to place inside the polygons before sampling. 
#' If \code{J} is not specified, approximately \code{init.n.factor*n} 
#' points are placed in the Halton lattice overlaid on the polygons 
#' of \code{x}.  Points in the Halton lattice are then sampled 
#' using the HAL method for points.  \code{init.n.factor*n} is the 
#' approximate frame size.  
#' 
#' @return A \code{SpatialPointsDataFrame} containing locations in the HAL sample, 
#' in HAL order.
#'  Attributes of the sample points are: 
#' \itemize{
#'   \item \code{sampleID}: A unique identifier for every sample point.  This 
#'   encodes the HAL order.  \code{return[order(return$sampleID),]} will sort the 
#'   returned object in HAL order.
#'   
#'   \item \code{geometryID}: The ID of the polygon in \code{x} containing each 
#'   sample point.  The 
#'   ID of polygons in \code{x} are \code{row.names(geometry(x))}. 
#'   \item Any attributes of the original polygons (in \code{x}). 
#' }
#'
#' Additional attributes of the output object, beyond those which 
#' make it a \code{SpatialPointsDataFrame}, are:
#' \itemize{
#'    \item \code{frame}: Name of the input sampling frame.
#'    \item \code{frame.type}: Type of resource in sampling frame. (i.e., "polygon").
#'    \item \code{sample.type}: Type of sample drawn. (i.e., "HAL").
#'    \item \code{random.start}: The random seed of the Halton lattice sample.  
#'    See \code{\link{hal.point}}. 
#' }
#' 
#' 
#' @author Trent McDonald
#' @seealso \code{\link{hal.line}}, \code{\link{hal.point}}, \code{\link{spsample}}
#' @keywords design survey
#' @examples
#' 
#'samp <- hal.polygon( WA, 100 )
#'plot(WA)
#'points( samp, pch=16, col="red" )
#'   
#'#   Different lattice topology
#'samp <- hal.polygon( WA, 100, J=c(8,2), eta=c(2,1), triangular=TRUE)
#'points( samp, pch=15, col="blue")
#'   
#' 

hal.polygon <- function( x, n, J=NULL, eta=c(1,1), triangular=FALSE, bases=c(2,3), 
                         init.n.factor=1000){

  
  #   Check n
  if( n < 1 ){
    n <- 1
    warning("Sample size less than one has been reset to 1")
  }

  # If lattice is not specified, set frame size to some multiple of n
  if(is.null(J)){
    N <- init.n.factor*n
  } else {
    N <- NULL
  }
  
  # Construct Halton lattice  
  hl.points <- halton.lattice.polygon( x, N, J, eta, triangular, bases )
  
  # Renames some attributes
  names(hl.points)[names(hl.points) == "geometryID"] <- "polygonID"
  
  # Now that we have points, we can draw a HAL point sample. 
  samp <- hal.point( hl.points, n, attr(hl.points, "J"), attr(hl.points, "bases") )
  
  # Drop the point geometry ID
  samp <- samp[,which(names(samp) != "geometryID")]
  
  # Renames polygonID to geometryID 
  names(samp)[names(samp) == "polygonID"] <- "geometryID"

  # Erase row.names because they are the point id's and not useful
  row.names(samp) <- 1:length(samp)
  
  samp
  
  
}


# ----- Some examples

# samp<- hal.polygon(1000, WA.utm)
# plot(WA.utm)
# points(samp)
