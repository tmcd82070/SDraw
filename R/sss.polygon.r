#' @export sss.polygon
#' 
#' @title  Draws a Simple Systematic Sample (SSS) from an area resource (polygons).
#' 
#' @description Draws a systematic, or grid, sample from a SpatialPolygons object, where
#' SpatialPolygons are defined in library 'sp'. Options are available to
#' control the relative spacing in horizontal and vertical directions, whether
#' a square or triangular grid is produced, and whether the grid is randomly
#' rotated.
#' 
#' @details The projection system of the input shape object (\code{shp}) is checked.  If
#' the projection system is anything other than UTM (e.g., "longlat"),
#' \code{shp} is converted to UTM prior to constructing the systematic grid.
#' UTM coordinate systems are necessary to obtain correct spacings (in UTM
#' space).  At the end, if the orginal projection system was not UTM, the
#' sample grid is projected back to the original.  This will make the output
#' grid lines non-parrallel in the orginal system.  To get a proper grid, with
#' consistent spacing, in the orginal projection, you must start with UTM's.
#' The author STRONGLY recommends converting \code{shp} to a UTM coordinate
#' system prior to calling this function.
#' 
#' Absolute spacing (size of grid cells) is determined by \code{n}. By default,
#' grid spacing is equal in X and Y directions, which produces square grid
#' cells.  By default, grid spacing is \code{delta} (= \code{sqrt(A/n)}, where
#' \code{A} = area of union of all polygons in \code{shp}.
#' 
#' Relative spacing is controled by the input \code{spacing} vector.  If
#' \code{spacing = c(rx, ry)}, spacing in X and Y directions is
#' \code{spacing*delta/rev(spacing)}. This assures area of each cell is
#' \code{delta^2} while maintaining relative size specified.
#' 
#' In general, realized sample size is not fixed.  Across multiple calls,
#' realized sample size will not always equal \code{n}.
#' 
#' @param n Sample size.  Number of locations to draw from the set of all
#' polygons contained in \code{shp}.
#' @param shp A SpatialPolygons or SpatialPolygonsDataFrame object. This object
#' must contain at least 1 polygon.  If it contains more than 1 polygon, the
#' BAS sample is drawn from the union of all polygons.
#' @param spacing A vector of length 2 containing the RELATIVE spacing of grid
#' points in the horizontal (X) and vertical (Y) directions.  See details.
#' @param triangular Boolean scaler specifying whether to produce a rectangular
#' (\code{triangular==FALSE}) or triangular (\code{triangular==TRUE}) grid.
#' @param rand.dir Boolean scaler specifying whether to randomly orient the
#' grid direction (\code{rand.dir==TRUE}) or not (\code{rand.dir==FALSE}).  If
#' FALSE, orientation of the grid is parallel to the X and Y axes.  If TRUE,
#' the X axis of the grid is randomly rotated by an angle between -45 and 45
#' degrees.  Note, relative spacing of the grid cells is computed prior to
#' rotation. After rotation, the X axis of the grid could be close to 90
#' degress, thereby nearly swapping the relative spacings.
#' @return A SpatialPointsDataFrame containing locations in the SSS sample, in
#' arbitrary order.  If the input object has an attached data frame (i.e., is a
#' SpatialPolygonsDataFrame), attrributes of the polygon in which each point
#' fell is attached in the associated data frame.
#' @author Trent McDonald
#' @seealso \code{\link{bas.polygon}}
#' @keywords design survey
#' @examples
#' 
#' # A square grid oriented east-west
#' WA.samp <- sss.polygon( 100, WA )   
#' plot( WA )
#' points( WA.samp )
#' 
#' # A rectangular grid oriented east-west, with relative spacing c(0.667, 1.5)
#' WA.samp <- sss.polygon( 100, WA, spacing=c(2,3) )   
#' plot( WA )
#' points( WA.samp )
#' 
#' # A rectangular grid oriented east-west, with x spacing = 2**(y spacing). 
#' WA.samp <- sss.polygon( 100, WA, spacing=c(sqrt(2),1) )   
#' plot( WA )
#' points( WA.samp )
#' 
#' # A rectangular grid, random orientation, with y spacing = 3*(x spacing)
#' WA.samp <- sss.polygon( 100, WA, spacing=c(1,sqrt(3)), rand.dir=TRUE )   
#' plot( WA )
#' points( WA.samp )
#' 
#' # A triangular grid oriented east-west
#' WA.samp <- sss.polygon( 100, WA, triangular=TRUE )   
#' plot( WA )
#' points( WA.samp )
#' 
#' # A triangular grid oriented east-west, with relative spacing c(.667,1.5)
#' WA.samp <- sss.polygon( 100, WA, spacing=c(2,3), triangular=TRUE )   
#' plot( WA )
#' points( WA.samp )
#' 
#' 
sss.polygon <- function( n, shp, spacing=c(1,1), triangular=FALSE, rand.dir=FALSE ){



#   First, convert to UTM, if shp is not already
ps <- proj4string(shp)
if( regexpr("proj=longlat", ps) > 0 ){
    #   We have a lat-long system  - convert
    #   All other systems leave alone
    #   Compute an "okay" zone = zone of center of shp.
    warning( paste("SDRAW: Converting from Lat-Long to UTM for sampling, then back.  This may cause non-parallel grid lines\n", 
    "when plotted in Lat-Long coordinate systems.  Recommend conversion of original spatial frame to UTM and re-drawing sample.\n"))
    
    mean.x <- mean( bbox(shp)["x",] )
    utm.zone <- floor((mean.x + 180)/6) + 1;  # screw exceptions near Svalbaard.  Don't matter here.
    
    cat( paste("UTM Zone used for conversion =", utm.zone, "\n" ))
    
    #   shp tranformed to UTM
    shp <- spTransform( shp, CRS(paste("+proj=utm +zone=", utm.zone, " +datum=WGS84", sep="")) )
} else {
    utm.zone <- NA
}


#   Bounding box of shapefile
bb <- bbox( shp )

# area of shapefile.  Because we made sure we have UTM, this is square meters. 
A <- sum(unlist(lapply( shp@polygons, function(x){ x@area})))  


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
    dx <- diff(bb["x",])
    dy <- diff(bb["y",])
    d <- max(dx,dy)
    
    #   The first grid on corners.  Much bigger than we need, but it accounts for rotation.
    seq.x <- seq( bb["x","min"]-d/2, bb["x","min"] + 2*d, by=delta[1] ) + m.x
    seq.y <- seq( bb["y","min"]-d/2, bb["y","min"] + 2*d, by=delta[2] ) + m.y

    grd1 <- expand.grid( x=seq.x, y=seq.y )
    df1 <- data.frame( row=rep(2*(1:length(seq.y))-1, each=length(seq.x)), col=rep(2*(1:length(seq.x))-1, length(seq.y)), pointType=rep("Sample", length(seq.x)*length(seq.y)) )
    
    #   The second grid on the centers
    grd2 <- grd1
    grd2$x <- grd1$x + delta[1]/2
    grd2$y <- grd1$y + delta[2]/2
    df2 <- data.frame( row=rep(2*(1:length(seq.y)), each=length(seq.x)), col=rep(2*(1:length(seq.x)), length(seq.y)), pointType=rep("Sample", length(seq.x)*length(seq.y)) )

    grd <- rbind(grd1, grd2)
    df <- rbind(df1, df2)
    
} else {
    
    #   The random start
    m.x <- runif( 1, 0, delta[1] )
    m.y <- runif( 1, 0, delta[2] )
    
    #   Grid extent.  Do this so that under rotation, we don't loose any rows.
    dx <- diff(bb["x",])
    dy <- diff(bb["y",])
    d <- max(dx,dy)
    
    #   The grid.  Much bigger than we need to account for rotation. 
    seq.x <- seq( bb["x","min"]-d/2, bb["x","min"] + 2*d, by=delta[1] ) + m.x
    seq.y <- seq( bb["y","min"]-d/2, bb["y","min"] + 2*d, by=delta[2] ) + m.y
    
    grd <- expand.grid( x=seq.x, y=seq.y )
    df <- data.frame( row=rep(1:length(seq.y), each=length(seq.x)), col=rep(1:length(seq.x), length(seq.y)), pointType=rep("Sample", length(seq.x)*length(seq.y)) )
}


#   Randomly spin the grid, if called for
if( rand.dir ){

    #   rotate the grid, after translating to (0,0)        
    theta <- runif( 1, -pi/4, pi/4 )
    A <- matrix( c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2,2)

    mean.xy <- c( mean(bb["x",]), mean(bb["y",]) )
    pts <- cbind(grd$x, grd$y) - matrix(mean.xy, nrow=nrow(grd), ncol=2, byrow=TRUE)
    rot.pts <- pts %*% A

    #   Translate back to proper location    
    grd$x <- rot.pts[,1] + mean.xy[1]
    grd$y <- rot.pts[,2] + mean.xy[2]
} else {
    theta <- 0   # for the record, nothing to do here
}

#   Make grid into a SpatialX object
grd <- SpatialPointsDataFrame( grd, proj4string=CRS(proj4string(shp)), data=df )

#   Clip to shp
shp@data <- data.frame( data.frame(shp), zzz=1 )   #  make sure data frame has at least one numeric column
tmp <- over( grd, shp )
keep <- !is.na(tmp$zzz)
tmp <- tmp[,!(names(tmp) %in% c("zzz"))] 
tmp2 <- data.frame(grd)
tmp2 <- tmp2[,!(names(tmp2) %in% c("x","y"))] 
grd@data <- data.frame( tmp2, tmp )
grd <- grd[ keep, ]

#   Translate back to original projection, if necessary. 
if( !is.na(utm.zone) ){
    # the original projection was not UTM - convert back
    grd <- spTransform( grd, CRS(ps) )
}


#   Add spacing as attribute
attr(grd, "spacing.m") <- delta
attr(grd, "theta") <- theta
attr(grd, "triangular") <- triangular

grd

}
