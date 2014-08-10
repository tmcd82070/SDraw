plotSample <- function(button, dat){

#    print(ls(envir=environment()))
#    cat("*****\n")
#    print(ls(envir=parent.env(environment())))
#    cat("*****\n")
#    
#    SDrawPackageSpace <- as.environment( "package:SDraw" )
#    print( ls(envir=as.environment("equi.GUI")) )
        
    fn <- dat$shape.in.entry$getText()
    outobj <- dat$out.r.entry$getText()
    
    input.dir <- get(".INPUT.DIR", envir=.GlobalEnv ) 

    if( nchar(fn) == 0 ){
        error.message("A shapefile name must be specified.")
        return()
    }
    
    existing.fn <- paste( ".", fn, sep="" )
    if( exists(existing.fn) ){
        #   The shapefile has already been read, and it is laying around.  No need to re-read.
        shp <- get(existing.fn)
    } else {
        #   The shapefile is not laying around.  Read it.
        shp <- readShape(input.dir, fn)  # a wrapper for readOGR
        
        assign(existing.fn, shp, pos=.GlobalEnv)
    }
    
    #   plot shape file
    if( "SpatialPolygonsDataFrame" %in% class(shp) ){
        plot(shp, col=rainbow(length(shp@polygons)))
    } else {
        plot(shp)
    }

    #   If the sample object exists, plot points on the map
    if( exists( outobj )){
        samp <- get( outobj, pos=.GlobalEnv )
        n <- attr(samp, "n")
        stype <- attr(samp, "sample.type")
        
        if( nrow(samp) == n ){
            #   No oversample
            points( samp, pch=16 )
            legend("bottomleft", legend=paste(stype, "sample points"), pch=c(16))
        } else {
            #   There is some oversample
            points( samp[1:n,], pch=16 )
            points( samp[ (n+1):nrow(samp),], pch=1 )
            legend("bottomleft", legend=paste(stype, c("sample", "over sample")), pch=c(16,1))
        }

    }

}
