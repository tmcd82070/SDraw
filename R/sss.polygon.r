#' @export sss.polygon
#' 
#' @title  Draws a Simple Systematic Sample (SSS) from an area resource (polygons).
#' 
#' @description Draws a systematic, or grid, sample from a \code{SpatialPolygons} or 
#' \code{SpatialPolygonsDataFrame} object.  Optional parameters control
#' control the relative spacing in horizontal and vertical directions, whether
#' a square or triangular grid is produced, and whether the grid baseline has 
#' random orientation.
#' 
#' @details The projection system of the input shape object (\code{x}) is 
#' not considered. But, a projected coordinate system is necessary to 
#' obtain correct spacing on the ground.  
#' The author STRONGLY recommends converting \code{x} to a UTM coordinate
#' system prior to calling this function.
#'
#' Spacing (size and shape of grid cells) is determined by \code{n} and 
#' \code{spacing}. If \code{spacing} is not given,
#' grid spacing is equal in X and Y directions, which produces square grid
#' cells.  In this case, grid spacing is \code{delta} 
#' (= \code{sqrt(A/n)}, where
#' \code{A} = area of union of all polygons in \code{x}.
#' 
#' Relative shape of grid cells is controled by the \code{spacing} vector.  If
#' \code{spacing = c(rx, ry)}, spacing in X and Y directions is
#' \code{spacing*delta/rev(spacing)}, where \code{delta} 
#' = \code{sqrt(A/n)}. Conceptually, a square cell of size \code{delta^2} 
#' is "stretched" multiplicatively by \code{rx} in the X direction and \code{ry} in the 
#' Y direction. After stretching, the area of each cell remains
#' \code{delta^2} while the relative lengths of the (rectangular) cell 
#' sides is 1 to \code{(ry/rx)^2}.  That is, vertical dimension of each cell 
#' is \code{(ry/rx)^2} times the horizontal dimension.  Vice versa, the horizontal 
#' dimension is \code{(rx/ry)^2} times the vertical.
#' 
#' In general, realized sample size is not fixed.  Across multiple calls,
#' realized sample size will not always equal \code{n}.  Across an infinite number
#' of calls, the average sample size will be \code{n}
#' 
# One view of triangular grids (i.e., \code{triangular=TRUE}) is that 
# the resulting cells are triangles (hence the name).  Another view 
# is that every other point (diagonally) in a triangular grid is the center of a 
# hexagonal cell. When \code{triangular=TRUE} and 
# \code{!all(spacing==1)}, the hexagons are "squashed" in the vertical or 
# horizontal direction, but remain hexagons. Under the hexagon view, the 
# actual hexagonal cells are formed by connecting the six 
# points surrouding a center. 
# In this way, specifying 
# \code{triangular=TRUE} can be viewed as selecting hexagons. When 
# sampling geographic space, 
# sample designs that select of hexagons can have desirable properties.
#' 
#' In all cases, the grid is randomly shifted in the X and Y directions, 
#' before rotation (if called for).  The amount of the random shift is 
#' less than the X and Y extent of cells, and is returned as an attribute 
#' of the sample. 
#' 
#' 
#' @param n Sample size.  Number of locations to draw from the union of all
#' polygons contained in \code{x}.
#' 
#' @param x A \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} 
#' object. This object
#' must contain at least 1 polygon.  If it contains more than 1 polygon, the
#' SSS sample is drawn from the union of all polygons.  Holes are respected.
#' 
#' @param spacing A vector of length 2 containing the RELATIVE spacing of grid
#' points in the horizontal (X) and vertical (Y) directions.  See details.
#' 
#' @param triangular Boolean scaler specifying whether to produce a rectangular
#' (\code{triangular==FALSE}) or triangular (\code{triangular==TRUE}) grid. 
#' See Details.
#' 
#' @param rand.dir Either a boolean scaler specifying whether to randomly 
#' orient the
#' grid's horizontal axis (\code{rand.dir==TRUE}) or not 
#' (\code{rand.dir==FALSE}), or
#' a user-specified fixed direction for the horizontal axis.  If
#' FALSE, orientation of the grid is parallel to the X and Y axes.  If TRUE,
#' the X axis of the grid is randomly rotated by an angle between -pi/4 
#' (-45 degrees) and pi/4 (45 degrees).  
#' If \code{rand.dir} is a number, the 
#' grid is rotated by that many radians. No range check is performed 
#' on user-specified \code{rand.dir}, so for example, rotation by \code{pi/8} is 
#' equivalent to rotation by \code{pi/8 + 2*pi}. User-specified, but random, 
#' direction of the grid can be specified by \code{rand.dir = runif(1,0,pi)}.
#' Note, relative spacing 
#' of the grid cells is computed prior to rotation. 
#' 
#' 
#' @return A \code{SpatialPointsDataFrame} containing locations in the SSS sample, 
#' in row-wise order starting in the south (see \code{sampleID}, \code{row}, \code{col} 
#' in returned data frame).  Attributes of the sample points (in the 
#' embedded data frame) are 
#' as follows: 
#' \itemize{
#'   \item \code{sampleID}: A unique identifier for every sample point.  For 
#'   rectangular grids, \code{sampleID} is incremented west to east by row from 
#'   the south.  For triangular grids, \code{sampleID} is assigned west to east 
#'   to points in every other row from the south. Then, 
#'   starts over in the southwest and assigns ID's to previously-skipped 
#'   rows.
#'   \item \code{row}: Row number of the sampled point in the grid.  Row numbers 
#'   are the vertical indices of the grid in a direction perpendicular to 
#'   the (potentially rotated) main horizontal axis. Cell (1,1) is in 
#'   the lower left (southwest) corner of the shape's bounding box.
#'   Thus, row 1 is defined 
#'   along the lower (southern) boundary of the shape's bounding box.  
#'   Points in row 1 may not be inside the shape and therefore 
#'   may not appear in the sample.  Consequently, the lowest row appearing 
#'   in the sample may not be 1.  Visualize row i with 
#'   \code{points(samp[samp$row==i,])}. 
#'   \item \code{col}: Column number of the sampled point in the grid. Column 
#'   numbers are the horizontal indicies of the grid in a direction parallel to
#'   the (potentially rotated) main horizontal axis. Cell (1,1) is in 
#'   the lower left (southwest) corner of the shape's bounding box.
#'   Thus, column 1 is defined 
#'   along the left (western) boundary of the shape's bounding box.  
#'   Points in column 1 may not be inside the shape and therefore 
#'   may not appear in the sample.  Consequently, the lowest column appearing 
#'   in the sample may not be 1. Visualize column i with 
#'   \code{points(samp[samp$col==i,])}.   
#'   \item \code{geometryID}: The ID of the polygon in \code{x} which each 
#'   sample point falls.  The 
#'   ID of polygons in \code{x} are \code{row.names(geometry(x))}. 
#'   \item Any attributes of the original polygons (in \code{x}). 
#' }
#'
#' Additional attributes of the output object, beyond those which 
#' make it a \code{SpatialPointsDataFrame}, are:
#' \itemize{
#'    \item \code{frame}: Name of the input sampling frame.
#'    \item \code{frame.type}: Type of resource in sampling frame. (i.e., "polygon").
#'    \item \code{sample.type}: Type of sample drawn. (i.e., "SSS").
#' 
#'    \item \code{spacing.m}: A vector of length 2 giving the dimensions of
#'    cells in units of the coordinates of \code{x}. (e.g., meters).  This
#'    is the final \code{delta} computed above.  Each cell has size 
#'    \code{prod(spacing.m)} = Area / \code{n}.
#'    
#'    \item \code{rand.dir}: The (potentially randomly chosen) direction for the grid's 
#'    horizontal axis. This is in radians between -pi/4 and pi/4. 
#'    \code{rand.dir} = 0 corresponds to no rotation 
#'    (i.e., \code{rand.dir = FALSE}).  
#'    
#'    \item \code{rand.shift}: The random shift of the grid.  This is a vector 
#'    of length 2 containing the  random shifts in the horizontal and vertical 
#'    directions before rotation.  The random shift in both directions 
#'    is chosen between 0 and the corresponding element of the \code{spacing.m}
#'    attribute (described above).
#'    
#'    
#'    \item \code{triangular}: TRUE or FALSE depending on whether the output 
#'    grid is triangular or rectangular, respectively.
#' }
#'     
#' @author Trent McDonald
#' @seealso \code{\link{bas.polygon}}, \code{\link{sdraw}}
#' @keywords design survey
#' @examples
#' 
#' # A square grid oriented east-west
#' WA.samp <- sss.polygon( WA, 100 )   
#' plot( WA )
#' points( WA.samp )
#' 
#' # A rectangular grid oriented east-west, with relative spacing c(0.667, 1.5),
#' # or 1 to 2.25.
#' WA.samp <- sss.polygon( WA, 100, spacing=c(2,3) )   
#' plot( WA )
#' points( WA.samp )
#' 
#' # A rectangular grid oriented east-west, with x spacing = 2*(y spacing). 
#' WA.samp <- sss.polygon( WA, 100, spacing=c(sqrt(2),1) )   
#' 
#' # A rectangular grid, random orientation, with y spacing = 3*(x spacing)
#' WA.samp <- sss.polygon( WA, 100, spacing=c(1,sqrt(3)), rand.dir=TRUE )   
#' 
#' # A triangular grid oriented east-west
#' WA.samp <- sss.polygon( WA, 100, triangular=TRUE )   
#' 
#' # A triangular grid oriented east-west, with relative spacing c(.667,1.5)
#' WA.samp <- sss.polygon( WA, 100, spacing=c(2,3), triangular=TRUE )   
#' 
#' 
sss.polygon <- function( x, n, spacing=c(1,1), triangular=FALSE, rand.dir=FALSE ){


#   Bounding box of shapefile
bb <- bbox( x )

# area of shapefile.  Does not care whether x is projected 
A <- polygonArea(x)  


#   Compute spacing assuming equal x and y spacing
delta <- sqrt( A / n )   

#   Adjust for relative spacing
delta <- spacing * delta / rev(spacing)


#   Make the grid, which depends on whether it's rectangular or triangular
if( triangular ){
    #   Spacing of 1/2 the grid
    delta <- sqrt(2) * delta

    #   The random start
    m.x <- runif( 1, 0, delta[1] )
    m.y <- runif( 1, 0, delta[2] )

    #   Grid extent.  Do this so that under rotation, we don't loose any rows.
    dx <- diff(bb[1,])
    dy <- diff(bb[2,])
    d <- max(dx,dy)
    
    #   The first grid on corners.  Much bigger than we need, but it accounts for rotation.
    seq.x <- seq( bb[1,"min"]-d/2, bb[1,"min"] + 2*d, by=delta[1] ) + m.x
    seq.y <- seq( bb[2,"min"]-d/2, bb[2,"min"] + 2*d, by=delta[2] ) + m.y

    grd1 <- expand.grid( x=seq.x, y=seq.y )
    df1 <- data.frame( row=rep(2*(1:length(seq.y))-1, each=length(seq.x)), col=rep(2*(1:length(seq.x))-1, length(seq.y)) )
    
    #   The second grid on the centers
    grd2 <- grd1
    grd2$x <- grd1$x + delta[1]/2
    grd2$y <- grd1$y + delta[2]/2
    df2 <- data.frame( row=rep(2*(1:length(seq.y)), each=length(seq.x)), col=rep(2*(1:length(seq.x)), length(seq.y)) )

    grd <- rbind(grd1, grd2)
    df <- rbind(df1, df2)

    df <- data.frame( sampleID=1:nrow(df), df )    
    
} else {
    
    #   The random start
    m.x <- runif( 1, 0, delta[1] )
    m.y <- runif( 1, 0, delta[2] )
    
    #   Grid extent.  Do this so that under rotation, we don't loose any rows.
    dx <- diff(bb[1,])
    dy <- diff(bb[2,])
    d <- max(dx,dy)
    
    #   The grid.  Much bigger than we need to account for rotation. 
    seq.x <- seq( bb[1,"min"]-d/2, bb[1,"min"] + 2*d, by=delta[1] ) + m.x
    seq.y <- seq( bb[2,"min"]-d/2, bb[2,"min"] + 2*d, by=delta[2] ) + m.y
    
    grd <- expand.grid( x=seq.x, y=seq.y )
    df <- data.frame( sampleID=1:(length(seq.x)*length(seq.y)), row=rep(1:length(seq.y), each=length(seq.x)), col=rep(1:length(seq.x), length(seq.y)) )
}


#   Randomly spin the grid, if called for
if( rand.dir != FALSE ){

    # Determine the angle
    if( rand.dir == TRUE ){
      theta <- runif( 1, -pi/4, pi/4 )
    } else {
      # this allows the user to specify ANY rotation.
      theta <- -rand.dir
    }
  
    #   rotate the grid, after translating centroid of bounding box to (0,0)        
    A <- matrix( c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2,2)

    mean.xy <- c( mean(bb[1,]), mean(bb[2,]) )
    pts <- cbind(grd$x, grd$y) - matrix(mean.xy, nrow=nrow(grd), ncol=2, byrow=TRUE)
    rot.pts <- pts %*% A

    #   Translate back to proper location    
    grd$x <- rot.pts[,1] + mean.xy[1]
    grd$y <- rot.pts[,2] + mean.xy[2]
} else {
    theta <- 0   # for the record, nothing to do here
}

#   Make grid into a SpatialPoints object
grd <- SpatialPointsDataFrame( grd, proj4string=CRS(proj4string(x)), data=df )

#   Clip to x
x@data <- data.frame( geometryID=row.names(geometry(x)), data.frame(x),  zzz=1 )   #  make sure data frame has at least one numeric column
tmp <- over( grd, x )
keep <- !is.na(tmp$zzz)
tmp <- tmp[,!(names(tmp) %in% c("zzz"))] 
tmp2 <- data.frame(grd)
tmp2 <- tmp2[,!(names(tmp2) %in% c("x","y","optional"))] 
grd@data <- data.frame( tmp2, tmp )
grd <- grd[ keep, ]


#   Add spacing as attribute
attr(grd, "frame") <- deparse(substitute(x))
attr(grd, "frame.type") <- "polygon"
attr(grd, "sample.type") <- "SSS"
attr(grd, "spacing.m") <- delta
attr(grd, "rand.dir") <- -theta
attr(grd, "rand.shift") <- c(m.x, m.y)
attr(grd, "triangular") <- triangular

grd

}
