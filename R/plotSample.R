plotSample <- function(button, dat){

#    print(ls(envir=environment()))
#    cat("*****\n")
#    print(ls(envir=parent.env(environment())))
#    cat("*****\n")
#    
#    SDrawPackageSpace <- as.environment( "package:SDraw" )
#    print( ls(envir=as.environment("equi.GUI")) )
        

    fn <- dat$shape.in.entry$getText()
    in.dir <- dat$shape.in.dir$getText()       # in.dir <- '//LAR-FILE-SRV/Data/NPS/GitHub/2015.06.11/inst/doc/Shapefiles'
    outobj <- dat$out.r.entry$getText()        # outobj <- samp  'sdraw.2015.06.15.161404'

  
    if( nchar(fn) == 0 ){
        error.message("A shapefile name must be specified.")
        return()
    }
    
    #   Check whether the frame has been read already, and the sp object is laying around. If not, read it.
    shp <- getSpFrame(fn,in.dir)
    
  
    #   plot shape file
    if( regexpr("^SpatialPolygons", class(shp)[1]) > 0 ){
        plot(shp, col="gray",border="white")#rainbow(length(shp@polygons),start=3/6,end=4/6,alpha=0.5),border="white")                   # traditional R plot
    } else if (regexpr("^SpatialLines", class(shp)[1]) > 0){
        plot(shp, col="gray", lwd=3)#rainbow(length(shp),start=3/6,end=4/6,alpha=0.5), lwd=3)                     # traditional R plot
    } else if (regexpr("^SpatialPoints", class(shp)[1]) > 0 ){
        plot(shp, col="gray", pch=16)#rainbow(length(shp),start=3/6,end=4/6,alpha=0.5), pch=16)                    # traditional R plot
    }

    #   If the sample object exists, plot points on the map
    if( outobj != ""){   # letting the R sample name field be a blank      --- s/be an if here to determine which button was pressed   something read.b?
    if( exists( outobj )){
        samp <- get( outobj, pos=.GlobalEnv )     
        stype <- attr(samp, "sample.type")        
        
        # Is this a stratified sample -> different legend
        strat.var <- attr(samp, "strata.var")
        
        # Is this an unequal prob sample -> different legend
        unequal.var <- attr(samp, "unequal.var")
        
        # Determine if this sample has an oversample
        has.oversamp <- "pointType" %in% names(data.frame(samp))
        if( has.oversamp )  has.oversamp <- length(unique(data.frame(samp)[,"pointType"])) > 1   # ?
        
        if( !is.null( strat.var )){
          
          # We have stratified sample
          strat.ind <- data.frame(samp)[,strat.var]
          strat.vals <- levels(factor(strat.ind))
          strat.cols <- rainbow(length(strat.vals))
          for(h in strat.vals){
            points( samp[strat.ind == h,], pch=which(h==strat.vals)+14,cex=1.5,col=strat.cols[which(h==strat.vals)] )
          }
          legend("bottomleft", legend=strat.vals, pch=1:length(strat.vals)+14, col=strat.cols, title="Strata:")
          # Note. oversample points in stratified samples, if they exist, are not plotted.
        } else if( !is.null( unequal.var )){
          
          if( attr(samp, "alloc.type") != "Continuous" ){    # so, unequal prob or constant
            
            # We have categorical sample
            unequal.ind <- data.frame(samp)[,c("mdcaty")] 
            unequal.vals <- levels(factor(unequal.ind))
            unequal.cols <- rainbow(length(unequal.vals))
            for(h in unequal.vals){
              points( samp[unequal.ind == h,], pch=which(h==unequal.vals)+14,cex=1.5,col=unequal.cols[which(h==unequal.vals)] )
            }
            legend("bottomleft", legend=unequal.vals, pch=1:length(unequal.vals)+14, col=unequal.cols, title="Categories:")

          } else if( attr(samp, "alloc.type") == "Continuous" ){     # continuous sample
            samp <- samp[order(samp$mdcaty),]                        # make sure legend plots in range order
            NS <- nrow(samp@data)                                    # get NS - the N sampled
            if(NS <= 5){                                             # get one range.
              ranges <- paste0("(",round(min(samp@data$mdcaty),0),",",round(max(samp@data$mdcaty),0),"]")
              points(samp,pch=15,cex=1.5,col='blue')
              legend("bottomleft",ranges,pch=19,col='blue')
            } else if(NS >= 6 & NS <= 25){                           # get three ranges.    
              ranges <- cut(samp@data$mdcaty,3,dig.lab=0)
              unequal.ind <- as.numeric(ranges)
              unequal.cols <- rainbow(3)
              unequal.pch <- c(15,16,17)
              for(h in unique(unequal.ind)){
                points(samp[unequal.ind == h,],pch=h+14,cex=1.5,col=unequal.cols[h])
              }
              legend("bottomleft",as.character(droplevels(unique(ranges))),pch=unequal.pch,col=unequal.cols, title="Ranges:")                            
            } else {                                                 # get five ranges.  
              ranges <- cut(samp@data$mdcaty,5,dig.lab=0)
              unequal.ind <- as.numeric(ranges)
              unequal.cols <- rainbow(5)
              unequal.pch <- c(15,16,17,18,19)
              for(h in unique(unequal.ind)){
                points(samp[unequal.ind == h,],pch=h+14,cex=1.5,col=unequal.cols[h])
              }
              legend("bottomleft",as.character(droplevels(unique(ranges))),pch=unequal.pch,col=unequal.cols, title="Ranges:")  
            }
          }
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

}
