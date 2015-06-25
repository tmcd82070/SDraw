grts.unequal <- function( n, over.n, unequal.var, shp, alloc.type ){
  
  # Inputs: 
  # n = vector of sample sizes, one element per category
  # over.n = scalar (vector length 1) of number of units to add per category.  Constant across category
  # unequal.var = string nameing category variable IF shape contains points or lines
  # shp = the SpatialXDataFrame object (the frame)
  
  # Get category level names from shape file
  category.levels <- names(table(data.frame(shp)[,unequal.var]))
  # For debuggin
  cat("---- n -----\n")
  print(n)
  cat("---- over.n -----\n")
  print(over.n)
  cat("---- unequal.var -----\n")
  print(unequal.var)
  cat("---- category.levels -----\n")
  print(category.levels) 
  cat("---- head(shp) -----\n")
  print(head(data.frame(shp)))
  
  if(alloc.type == "constant"){
    
    #make caty.n
    the.caty.n <- n
    names(the.caty.n) <- category.levels
  
    #this makes a list of elements to be passed to the grts function
    selType="Unequal"
    Unequaldsgn <- list(None=list(panel=c(PanelOne=sum(n)),seltype=selType,caty.n=the.caty.n,over=over.n))
    IDHelper <- "Site"   
    
  } else if(alloc.type == "continuous"){

    #this makes a list of elements to be passed to the grts function
    selType="Continuous"
    IDHelper <- "Site" 
    Unequaldsgn <- list(None=list(panel=c(PanelOne=sum(n)),
                               seltype=selType,
                               over=over.n))
 
  } else if(alloc.type == "uneqproportion"){
    
    #make caty.n
    the.caty.n <- n
    names(the.caty.n) <- category.levels
    
    #this makes a list of elements to be passed to the grts function
    selType="Unequal"
    IDHelper <- "Site" 
    Unequaldsgn <- list(None=list(panel=c(PanelOne=sum(n)),
                                  seltype=selType,
                                  caty.n=the.caty.n,
                                  over=over.n)) 
  }
  
  if( regexpr("SpatialPoints", class(shp)[1]) > 0 ){
    sframe.type = "finite"
  } else if( regexpr("SpatialLines", class(shp)[1]) > 0 ){
    sframe.type = "linear"
  } else if( regexpr("SpatialPolygons", class(shp)[1]) > 0 ){
    sframe.type = "area"
  }
  
  Unequalsites <- grts(design=Unequaldsgn,
                     DesignID=IDHelper,
                     type.frame=sframe.type,
                     att.frame=data.frame(shp),
                     src.frame="sp.object",
                     sp.object=shp,
                     mdcaty=unequal.var,   #need to use category/continuous variable name as taken from GUI
                     shapefile=FALSE)
  
  
  cat("Success.\n")
  
  #   Toss some variables that are not important for equal probability designs
  #Equalsites <- Equalsites[,!(names(Equalsites) %in% c("mdcaty","wgt","stratum","panel"))]
  
  #   Add a column of sample/oversample for convieneince
  # Equalsites$pointType <- c(rep("Sample",n), rep("OverSample",over.n))
  
  #   Copy over the projection from the input spatial object
  proj4string(Unequalsites) <- CRS(proj4string(shp))
  
  #   Store some attributes
  attr(Unequalsites, "sample.type") <- "GRTS"
  attr(Unequalsites, "n") <- n
  attr(Unequalsites, "over.n") <- over.n
  attr(Unequalsites, "sp.object") <- deparse(substitute(shp))
  attr(Unequalsites, "frame.type") <- sframe.type
  attr(Unequalsites, "unequal.var") <- unequal.var
  attr(Unequalsites, "alloc.type") <- selType
  
  Unequalsites
}
