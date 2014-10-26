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

