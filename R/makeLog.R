makeLog <- function(strat.var=NULL,strata.levels=NULL,unequal.var=NULL,alloc.type,category.levels=NULL,n,over.n,shp,fn,dir,outobj=outobj,sframe.type=NULL,selType=NULL){
  #                 strat vars                       ,unequal vars                                    ,ubiquitous vars           , in unequal and strat
  
  
  # if file currently exists, delete it out.
  if (file.exists(paste0(dir,"/",outobj,".log"))) file.remove(paste0(dir,"/",outobj,".log"))
  
  # file now doesn't exist, so make it.
 
# make grts.strat log file
if(!is.null(strata.levels)){
    
  options(useFancyQuotes = FALSE)
  log_con <- file(paste0(dir,"/",outobj,".log"),open="a")
  close(log_con)
  
  # prepare stratum string for printing
  for(i in 1:length(strata.levels)){
    if(i == 1){
      string <- paste("c(",dQuote(strata.levels[1]),sep="")
    } else {
      string <- paste(string,",",dQuote(strata.levels[i]),sep="")
    }
  }
  string <- paste(string,")",sep="")

  # prepare n-string for printing
  for(i in 1:length(n)){
    if(i == 1){
      nstring <- paste("c(",n[1],sep="")
    } else {
      nstring <- paste(nstring,",",n[i],sep="")
    }
  }
  nstring <- paste(nstring,")",sep="")
  
  log_con <- file(paste0(dir,"/",outobj,".log"),open="a")     
  cat("# Utilization of this code without first installing R packages rgdal and spsurvey will result in error.\n",sep="",file=log_con)
  
  cat("# This output results from the grts.strat.r function of the SDraw package, WEST Inc., 2015, Version 1.04.\n
  library(rgdal)
  library(spsurvey)\n\n",sep="",file=log_con)
  
  cat("# Read in the shapefile of interest from which sampling occurs.\n
  shp <- readOGR( ",dQuote(dir),", ",dQuote(fn)," ) \n\n",sep="",file=log_con)

  cat("# Prepare the design of the sampling for use in the grts function.\n
  n <- ",nstring,"\n
  Stratdsgn <- lapply(1:length(",string,"), function(x, nn, st, o.n){
        list(panel=c(Main=n[x]),seltype=",dQuote(get("selType")),",over=",get("over.n"),")
      }, nn=",nstring,", st=",dQuote(get("selType")),", o.n=",get("over.n"),")\n
  names(Stratdsgn) <- ",string,"\n\n", sep="", append = TRUE, file = log_con)

  cat("# Draw the sample via the grts function in package spsurvey.\n
      Stratsites <- grts(design=Stratdsgn,
      DesignID='STRAT',
      type.frame=",dQuote(get("sframe.type")),",
      att.frame=data.frame(shp),
      src.frame='sp.object',
      sp.object=shp,
      stratum=",dQuote(get("strat.var")),",
      shapefile=FALSE)
      \n\n", sep="", append = TRUE, file = log_con)
  
  cat("# Plot the original shapefile, along with the sample.\n
      plot(shp)
      plot(Stratsites,col='red',pch=19,add=TRUE)\n\n", sep="", append = TRUE, file = log_con)
  
  close(log_con)
  options(useFancyQuotes = TRUE)
  
# make grts.unequal log file
} else if(!is.null(category.levels)){
  
  options(useFancyQuotes = FALSE)
  log_con <- file(paste0(dir,"/",outobj,".log"),open="a")    
  cat("# Utilization of this code without first installing R packages rgdal and spsurvey will result in error.\n",sep="",file=log_con)
  
  cat("# This output results from the grts.unequal.r function of the SDraw package, WEST Inc., 2015, Version 1.04.\n
library(rgdal)
library(spsurvey)\n\n",sep="",file=log_con)
  
  cat("# Read in the shapefile of interest from which sampling occurs.\n
shp <- readOGR( ",dQuote(dir),", ",dQuote(fn)," ) \n\n",sep="",file=log_con)
  close(log_con)
  
  if(alloc.type == "constant"){
    
    #make caty.n
    the.caty.n <- n
    names(the.caty.n) <- category.levels
    
    #this makes a list of elements to be passed to the grts function
    selType="Unequal"
    IDHelper <- "Site" 
    
    # prepare category string for printing
    for(i in 1:length(the.caty.n)){
      if(i == 1){
        string <- paste("c(",dQuote(names(the.caty.n[1])),"=",the.caty.n[1],sep="")
      } else {
        string <- paste(string,",",dQuote(names(the.caty.n[i])),"=",the.caty.n[i],sep="")
      }
    }
    string <- paste(string,")",sep="")
    
    log_con <- file(paste0(dir,"/",outobj,".log"),open="a")
    cat("# Prepare the design of the sampling for use in the grts function.\n
        Unequaldsgn <- list(None=list(panel=c(PanelOne=(",sum(get("n")),")),
        seltype=",dQuote(get("selType")),",
        caty.n=",string,",
        over=",get("over.n"),"))\n\n", sep="", append = TRUE, file = log_con)
    close(log_con)
    
  } else if(alloc.type == "continuous"){
    
    #this makes a list of elements to be passed to the grts function
    selType="Continuous"
    IDHelper <- "Site" 
    
    log_con <- file(paste0(dir,"/",outobj,".log"),open="a")
    cat("# Prepare the design of the sampling for use in the grts function.\n
        Unequaldsgn <- list(None=list(panel=c(PanelOne=(",sum(get("n")),")),
        seltype=",dQuote(get("selType")),",
        over=",get("over.n"),"))\n\n", sep="", append = TRUE, file = log_con)
    close(log_con)
    
  } else if(alloc.type == "uneqproportion"){
    
    #make caty.n
    the.caty.n <- n
    names(the.caty.n) <- category.levels
    
    #this makes a list of elements to be passed to the grts function
    selType="Unequal"
    IDHelper <- "Site" 
    
    # prepare category string for printing
    for(i in 1:length(the.caty.n)){
      if(i == 1){
        string <- paste("c(",dQuote(names(the.caty.n[1])),"=",the.caty.n[1],sep="")
      } else {
        string <- paste(string,",",dQuote(names(the.caty.n[i])),"=",the.caty.n[i],sep="")
      }
    }
    string <- paste(string,")",sep="")
    
    log_con <- file(paste0(dir,"/",outobj,".log"),open="a")
    cat("# Prepare the design of the sampling for use in the grts function.\n
        Unequaldsgn <- list(None=list(panel=c(PanelOne=(",sum(get("n")),")),
        seltype=",dQuote(get("selType")),",
        caty.n=",string,",
        over=",get("over.n"),"))\n\n", sep="", append = TRUE, file = log_con)
    close(log_con)
  }
  
  log_con <- file(paste0(dir,"/",outobj,".log"),open="a")
  cat("# Draw the sample via the grts function in package spsurvey.\n
      Unequalsites <- grts(design=Unequaldsgn,
      DesignID=",dQuote(get("IDHelper")),",
      type.frame=",dQuote(get("sframe.type")),",
      att.frame=data.frame(shp),
      src.frame='sp.object',
      sp.object=shp,
      mdcaty=",dQuote(get("unequal.var")),",   
      shapefile=FALSE)\n\n", sep="", append = TRUE, file = log_con)
  
  cat("# Plot the original shapefile, along with the sample.\n
      plot(shp)
      plot(Unequalsites,col='red',pch=19,add=TRUE)\n\n", sep="", append = TRUE, file = log_con)
  
  close(log_con)
  options(useFancyQuotes = TRUE)
} else {
  
  options(useFancyQuotes = FALSE)
  log_con <- file(paste0(dir,"/",outobj,".log"),open="a")     
  
  cat("# Utilization of this code without first installing R packages rgdal and spsurvey will result in error.\n",sep="",file=log_con)
  
  cat("# This output results from the grts.equi.r function of the SDraw package, WEST Inc., 2015, Version 1.04.\n
      library(rgdal)
      library(spsurvey)\n\n",sep="",file=log_con)
  
  cat("# Read in the shapefile of interest from which sampling occurs.\n
      shp <- readOGR( ",dQuote(dir),", ",dQuote(fn)," ) \n\n",sep="",file=log_con)

  cat("# Prepare the design of the sampling for use in the grts function.\n
        Equaldsgn <- list(None=list(panel=c(Main=(",sum(get("n")),")),
        seltype='Equal',over=",get("over.n"),"))\n\n", sep="", append = TRUE, file = log_con)
  
  cat("# Draw the sample via the grts function in package spsurvey.\n
       Equalsites <- grts(design=Equaldsgn,
                     DesignID='Site',
                     type.frame=",dQuote(get("sframe.type")),",
                     src.frame='sp.object',
                     sp.object=shp,
                     shapefile=FALSE)\n\n", sep="", append = TRUE, file = log_con)
  
  cat("# Plot the original shapefile, along with the sample.\n
      plot(shp)
      plot(Equalsites,col='red',pch=19,add=TRUE)\n\n", sep="", append = TRUE, file = log_con)
  
  close(log_con)
  options(useFancyQuotes = TRUE)
}

}



