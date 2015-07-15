grts.unequal <- function( n, over.n, unequal.var, shp, alloc.type, fn, dir, outobj ){
  
  # Inputs: 
  # n = vector of sample sizes, one element per category
  # over.n = scalar (vector length 1) of number of units to add per category.  Constant across category
  # unequal.var = string nameing category variable IF shape contains points or lines
  # shp = the SpatialXDataFrame object (the frame)
    
  # Get category level names from shape file
  category.levels <- names(table(data.frame(shp)[,unequal.var]))
  # For debuggin
#   cat("---- n -----\n")
#   print(n)
#   cat("---- over.n -----\n")
#   print(over.n)
#   cat("---- unequal.var -----\n")
#   print(unequal.var)
#   cat("---- category.levels -----\n")
#   print(category.levels) 
#   cat("---- head(shp) -----\n")
#   print(head(data.frame(shp)))
  
  if(alloc.type == "constant"){
    
    #make caty.n
    the.caty.n <- n
    names(the.caty.n) <- category.levels
  
    #this makes a list of elements to be passed to the grts function
    selType="Unequal"
    IDHelper <- "Site" 
    
    
# ------------- PRINT TO CONSOLE ----------------------------------------------------------------     
    # prepare category string for printing
    for(i in 1:length(the.caty.n)){
      if(i == 1){
        string <- paste("c(",dQuote(names(the.caty.n[1])),"=",the.caty.n[1],sep="")
      } else {
        string <- paste(string,",",dQuote(names(the.caty.n[i])),"=",the.caty.n[i],sep="")
      }
    }
    string <- paste(string,")",sep="")
    
    cat("# Prepare the design of the sampling for use in the grts function.\n
        Unequaldsgn <- list(None=list(panel=c(PanelOne=(",sum(get("n")),")),
        seltype=",dQuote(get("selType")),",
        caty.n=",string,",
        over=",get("over.n"),"))\n\n", sep="")
# ------------- PRINT TO CONSOLE ----------------------------------------------------------------     
    
    
    
    Unequaldsgn <- list(None=list(panel=c(PanelOne=sum(n)),seltype=selType,caty.n=the.caty.n,over=over.n))
      
  } else if(alloc.type == "continuous"){

    #this makes a list of elements to be passed to the grts function
    selType="Continuous"
    IDHelper <- "Site" 
    
    
# ------------- PRINT TO CONSOLE ----------------------------------------------------------------    
    cat("# Prepare the design of the sampling for use in the grts function.\n
      Unequaldsgn <- list(None=list(panel=c(PanelOne=(",sum(get("n")),")),
      seltype=",dQuote(get("selType")),",
      over=",get("over.n"),"))\n\n", sep="")
# ------------- PRINT TO CONSOLE ----------------------------------------------------------------        
    

    Unequaldsgn <- list(None=list(panel=c(PanelOne=sum(n)),
                               seltype=selType,
                               over=over.n))
    
  } else if(alloc.type == "uneqproportion"){
    
    #make caty.n
    the.caty.n <- n
    names(the.caty.n) <- category.levels
    
    
    
# ------------- PRINT TO CONSOLE ----------------------------------------------------------------    
    # prepare category string for printing
    for(i in 1:length(the.caty.n)){
      if(i == 1){
        string <- paste("c(",dQuote(names(the.caty.n[1])),"=",the.caty.n[1],sep="")
      } else {
        string <- paste(string,",",dQuote(names(the.caty.n[i])),"=",the.caty.n[i],sep="")
      }
    }
    string <- paste(string,")",sep="")
    
    cat("# Prepare the design of the sampling for use in the grts function.\n
      Unequaldsgn <- list(None=list(panel=c(PanelOne=(",sum(get("n")),")),
      seltype=",dQuote(get("selType")),",
        caty.n=",string,",
        over=",get("over.n"),"))\n\n", sep="")
# ------------- PRINT TO CONSOLE ----------------------------------------------------------------    
    
    
    
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



# ------------- PRINT TO CONSOLE ----------------------------------------------------------------
cat("# Draw the sample via the grts function in package spsurvey.\n
    Unequalsites <- grts(design=Unequaldsgn,
    DesignID=",dQuote(get("IDHelper")),",
    type.frame=",dQuote(get("sframe.type")),",
    att.frame=data.frame(shp),
    src.frame='sp.object',
    sp.object=shp,
    mdcaty=",dQuote(get("unequal.var")),",   
    shapefile=FALSE)\n\n", sep="")
# ------------- PRINT TO CONSOLE ----------------------------------------------------------------



  Unequalsites <- grts(design=Unequaldsgn,
                     DesignID=IDHelper,
                     type.frame=sframe.type,    # added to file
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

  makeLog(strat.var=NULL,strata.levels=NULL,unequal.var=unequal.var,alloc.type=alloc.type,category.levels=NULL,n,over.n,shp,fn,dir,outobj,sframe.type=sframe.type,selType=selType)

  Unequalsites
}