draw.unequal.grts <- function(n, over.n, unequal.var, alloc.type, fn, dir, outobj){   
  #
  #   draw a GRTS sample using spsurvey.  Spsurvey should already be loaded.
  #
  
  # jason note - run unequal.GUI first -- then all necessary vars are already read in.
  
  cat("Drawing GRTS sample...This can take a while ...\n")
  
  #   Check whether the frame has been read already, and the sp object is laying around. 
  shp <- getSpFrame( fn, dir )
  
  print(head(data.frame(shp)))
  
  if(!(unequal.var %in% names(shp))){
    stop(paste("Variable", unequal.var, "not found in frame"))
  }   
  n.categories <- length(unique(data.frame(shp)[,unequal.var]))
  
  #   Find number of groups -- jason:  this just ids the sd type?
  if( regexpr("SpatialPoints", class(shp)[1]) > 0 ){
    sframe.type = "points"
  } else if( regexpr("SpatialLines", class(shp)[1]) > 0 ){
    sframe.type = "lines"
  } else if( regexpr("SpatialPolygons", class(shp)[1]) > 0 ){
    sframe.type = "polygons"
  }
  
  # Set sample sizes based on allocation scheme
  if(alloc.type=="continuous"){
#     if( sframe.type == "polygons"){
#       # get area in each category
#       unequal.memb <- data.frame(shp)[,unequal.var]
#       feature.sizes <- gArea(shp, TRUE)
#       unequal.sizes <- tapply( feature.sizes, unequal.memb, sum)
#     } else if( sframe.type == "lines" ){
#       # get total length in each category
#       unequal.memb <- data.frame(shp)[,unequal.var]
#       feature.sizes <- gLength(shp, TRUE)
#       unequal.sizes <- tapply( feature.sizes, unequal.memb, sum)	
#     } else {
#       unequal.sizes <- table(data.frame(shp)[,unequal.var])
#     }
    n <- round(as.numeric(n[1]))
  } else if( alloc.type=="constant"){
    n <- c(rep(round(as.numeric(n[1])/n.categories),(n.categories - 1)),as.numeric(n[1]) - sum(rep(round(as.numeric(n[1])/n.categories),(n.categories - 1))))   # give the last cat the leftovers
  } else if( alloc.type=="uneqproportion"){
    n <- as.numeric(unlist(strsplit(n,",")))
    if( length(n) != n.categories ){
      stop(paste( length(n), "samples sizes specified, but", n.categories, "categories found."))
    }
  }
  
  #   Call the user visible routine that takes a SpatialX object
  ans <- grts.unequal(n, over.n, unequal.var, shp, alloc.type, fn, dir, outobj ) 
  ans
}
