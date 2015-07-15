grts.equi <- function( n, over.n, shp, fn, dir, outobj ){

  Equaldsgn <- list(None = list(panel = c(Main = n),
                                seltype = "Equal",
                                over = over.n))

  
# ------------- PRINT TO CONSOLE ----------------------------------------------------------------    
  cat("# Prepare the design of the sampling for use in the grts function.\n
        Equaldsgn <- list(None=list(panel=c(Main=(",sum(get("n")),")),
        seltype='Equal',over=",get("over.n"),"))\n\n", sep="")
# ------------- PRINT TO CONSOLE ----------------------------------------------------------------    

  
  
  
  
  if( regexpr("SpatialPoints", class(shp)[1]) > 0 ){
      sframe.type = "finite"
  } else if( regexpr("SpatialLines", class(shp)[1]) > 0 ){
      sframe.type = "linear"
  } else if( regexpr("SpatialPolygons", class(shp)[1]) > 0 ){
      sframe.type = "area"
  }

# ------------- PRINT TO CONSOLE ----------------------------------------------------------------    
  cat("# Draw the sample via the grts function in package spsurvey.\n
       Equalsites <- grts(design=Equaldsgn,
                     DesignID='Site',
      type.frame=",dQuote(get("sframe.type")),",
      src.frame='sp.object',
      sp.object=shp,
      shapefile=FALSE)\n\n", sep="")
# ------------- PRINT TO CONSOLE ----------------------------------------------------------------    
  
  Equalsites <- grts(design=Equaldsgn,
          DesignID='Site',
          type.frame=sframe.type,
          src.frame="sp.object",
          sp.object=shp,
          shapefile=FALSE)

  cat("Success.\n")

  #   Toss some variables that are not important for equal probability designs
  #Equalsites <- Equalsites[,!(names(Equalsites) %in% c("mdcaty","wgt","stratum","panel"))]

  #   Add a column of sample/oversample for convieneince
  # Equalsites$pointType <- c(rep("Sample",n), rep("OverSample",over.n))

  #   Copy over the projection from the input spatial object
  proj4string(Equalsites) <- CRS(proj4string(shp))

  #   Store some attributes
  attr(Equalsites, "sample.type") <- "GRTS"
  attr(Equalsites, "n") <- n
  attr(Equalsites, "over.n") <- over.n
  attr(Equalsites, "sp.object") <- deparse(substitute(shp))
  attr(Equalsites, "frame.type") <- sframe.type
  
  makeLog(strat.var=NULL,strata.levels=NULL,unequal.var=NULL,alloc.type=NULL,category.levels=NULL,n,over.n,shp,fn,dir,outobj,sframe.type=sframe.type,selType=NULL)
  
  Equalsites
}
