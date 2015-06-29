plotSample <- function(button, dat){

#    print(ls(envir=environment()))
#    cat("*****\n")
#    print(ls(envir=parent.env(environment())))
#    cat("*****\n")
#    
#    SDrawPackageSpace <- as.environment( "package:SDraw" )
#    print( ls(envir=as.environment("equi.GUI")) )
        

    fn <- dat$shape.in.entry$getText()
    in.dir <- dat$shape.in.dir$getText()
    outobj <- dat$out.r.entry$getText()
    
  
    if( nchar(fn) == 0 ){
        error.message("A shapefile name must be specified.")
        return()
    }
    
    #   Check whether the frame has been read already, and the sp object is laying around. If not, read it.
    shp <- getSpFrame(fn,in.dir)
    
  
    #   plot shape file
    if( regexpr("^SpatialPolygons", class(shp)[1]) > 0 ){
        plot(shp, col=rainbow(length(shp@polygons)))
    } else if (regexpr("^SpatialLines", class(shp)[1]) > 0){
        plot(shp, col=rainbow(length(shp)), lwd=3)
    } else if (regexpr("^SpatialPoints", class(shp)[1]) > 0 ){
        plot(shp, col=rainbow(length(shp)), pch=16)
    }

    #   If the sample object exists, plot points on the map
    if( exists( outobj )){
        samp <- get( outobj, pos=.GlobalEnv )
        stype <- attr(samp, "sample.type")
        
        # Is this a stratified sample -> different legend
        strat.var <- attr(samp, "strata.var")
        
        # Determine if this sample has an oversample
        has.oversamp <- "pointType" %in% names(data.frame(samp))
        if( has.oversamp )  has.oversamp <- length(unique(data.frame(samp)[,"pointType"])) > 1
        
        if( !is.null( strat.var )){
          # We have stratified sample
          strat.ind <- data.frame(samp)[,strat.var]
          strat.vals <- levels(factor(strat.ind))
          strat.cols <- terrain.colors(length(strat.vals))
          for(h in strat.vals){
            points( samp[strat.ind == h,], pch=which(h==strat.vals)+14, col=strat.cols[which(h==strat.vals)] )
          }
          legend("bottomleft", legend=strat.vals, pch=1:length(strat.vals)+14, col=strat.cols, title="Strata:")
          # Note. oversample points in stratified samples, if they exist, are not plotted.
        } else if( has.oversamp ){
          #   There is some oversample
          samp.ind <- data.frame(samp)[,"pointType"]
          points( samp[samp.ind=="Sample",], pch=16 )
          points( samp[samp.ind=="OverSample",], pch=1 )
          legend("bottomleft", legend=paste(stype, c("sample", "over sample")), pch=c(16,1))
        } else {
          #   No oversample
          points( samp, pch=16 )
          legend("bottomleft", legend=paste(stype, "sample points"), pch=c(16))
        }

    }

}
