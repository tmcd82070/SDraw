draw.strat.grts <- function(n, over.n, strat.var, alloc.type, fn, dir, outobj){   
#
#   draw a GRTS sample using spsurvey.  Spsurvey should already be loaded.
#

    cat("Drawing GRTS sample...This can take a while ...\n")

#   Check whether the frame has been read already, and the sp object is laying around. 
    shp <- getSpFrame( fn, dir )

    print(head(data.frame(shp)))

    if(!(strat.var %in% names(shp))){
      stop(paste("Variable", strat.var, "not found in frame"))
    }    
    n.strata <- length(unique(data.frame(shp)[,strat.var]))
    
#   Find number of strata
    if( regexpr("SpatialPoints", class(shp)[1]) > 0 ){
      sframe.type = "points"
    } else if( regexpr("SpatialLines", class(shp)[1]) > 0 ){
      sframe.type = "lines"
    } else if( regexpr("SpatialPolygons", class(shp)[1]) > 0 ){
      sframe.type = "polygons"
    }

  # Set sample sizes based on allocation scheme
    if(alloc.type=="proportional"){
      if( sframe.type == "polygons"){
        # get area in each strata
    		strata.memb <- data.frame(shp)[,strat.var]
        feature.sizes <- gArea(shp, TRUE)
        strata.sizes <- tapply( feature.sizes, strata.memb, sum)
      } else if( sframe.type == "lines" ){
        # get total length in each strata
        strata.memb <- data.frame(shp)[,strat.var]
        feature.sizes <- gLength(shp, TRUE)
        strata.sizes <- tapply( feature.sizes, strata.memb, sum)	
      } else {
        strata.sizes <- table(data.frame(shp)[,strat.var])
      }
      n <- round(as.numeric(n[1]) * strata.sizes / sum(strata.sizes))
    } else if( alloc.type=="constant"){
        n <- rep(as.numeric(n[1]),n.strata)
    } else if( alloc.type=="user"){
        n <- as.numeric(unlist(strsplit(n,",")))
        if( length(n) != n.strata ){
          stop(paste( length(n), "samples sizes specified, but", n.strata, "strata found."))
        }
    }

#   Call the user visible routine that takes a SpatialX object
    ans <- grts.strat(n, over.n, strat.var, shp, fn, dir, outobj )
    ans
}
