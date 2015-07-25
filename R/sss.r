#'  Simple Systematic Sample from a spatial object.  
#'  
#'  Draws a Simple Systematic Sample (SSS) sample from a SpatialLines or 
#'  SpatialPolygons object.  
#'  
#'  This is a wrapper for two funtions. 
#'  If \code{shp} is a SpatialLinesX object, \code{sss.line} is called.
#'  If \code{shp} is a SpatialPolygonsX object, \code{sss.polygon} is called.
#'  
#'  @param n Sample size.  The number of locations to draw from the spatial object. 
#'  @param shp  Name of the spatial object. This must be an existing SpatialLines or 
#'    SpatialPolygons object. The versions 
#'    with attached data frames are also accepted (i.e., SpatialLinesDataFrame or 
#'    SpatialPolygonsDataFrame).
#'  @param spacing  If \code{shp} is a SpatialPolygonsX object, this is vector of 
#'    length 2 containing the RELATIVE spacing of grid points in the horizontal 
#'    (X) and vertical (Y) directions.  See \code{help(sss.polygon)} for details. 
#'  @param triangular  If \code{shp} is a SpatialPolygonsX object, this is a 
#'    boolean scaler specifying whether to produce a rectangular 
#'    (\code{triangular==FALSE}) or triangular (\code{triangular==TRUE}) grid.
#'    See \code{help(sss.polygon)} for details.
#'  @param rand.dir If \code{shp} is a SpatialPolygonsX object, this is a 
#'    boolean scaler specifying whether to randomly orient the grid.  If FALSE, 
#'    orientation of the grid is parallel to the X and Y axes.  If TRUE, the 
#'    X axis of the grid is randomly rotated by an angle between -45 and 45
#'    degrees.  Note, relative spacing of the grid cells is computed prior to rotation. 
#'    See \code{help(sss.polygon)} for details.
#'    
#'  @return In all cases, a SpatialPointsDataFrame containing locations in the SSS 
#'    sample is returned.  A 'siteID' attribute is attached to each point in the 
#'    embedded data frame.  In addition, if the input object has an attached data
#'    frame (i.e., is a SpatialXDataFrame), the data frame of the output object 
#'    contains all the attributes of the original data frame at the location of 
#'    the sample point. 
#'    
#'  @author Trent McDonald
#'  @seealso \code{\link{sss.line}}, \code{\link{sss.polygon}}
#'  
#'  @examples
#'    
#'  #   Polygons: Draw points from the polygons that make up Washington.  
#'  data(WA)
#'  WA.sample <- sss( 100, WA )
#'  plot( WA )
#'  points( WA.sample, pch=16 )
#'    
#'  #   Above code issued a warning because coordinate reference
#'  #   system (CRS) of the WA object was lat-long. Compare the above to:
#'  utm.zone <- floor((mean( bbox(WA)["x",] ) + 180)/6) + 1  # one way to figure zone
#'  WA.utm <- spTransform( WA, CRS(paste("+proj=utm +zone=", utm.zone, 
#'        " +datum=WGS84", sep="")) )
#'  WA.sample2 <- sss( 100, WA.utm )
#'  plot( WA.utm )
#'  points( WA.sample2, pch=16 )
#'    
#'  #   Lines: Draw points along coastline of Hawaii
#'  data(HI.coast)
#'  HI.coast.samp <- sss( 100, HI.coast )
#'  plot(HI.coast)
#'  points(HI.coast.samp, col="red", pch=16 )
#'    
sss <- function( n, shp, spacing=c(1,1), triangular=FALSE, rand.dir=FALSE ){
  
  #   Determine the sample type and call the appropriate function
    if( length(grep("SpatialPoints", class(shp))) > 0 ){

        stop( "SSS samples of points not implemented in SDraw")
        #samp <- sss.finite( n, shp )

    } else if (length(grep("SpatialLines", class(shp))) > 0 ){

        samp <- sss.line( n, shp )

    } else if (length(grep("SpatialPolygons", class(shp))) > 0 ){

        samp <- sss.polygon( n, shp, spacing, triangular, rand.dir )

    } else {
        stop( "Unknown spatial object type" )
    }
    
    samp
}

