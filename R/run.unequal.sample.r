run.unequal.sample <- function(button, dat){
  #tester for running unequal probability sample
  
  #   Query the entry fields
  
  # testing
#   n <- "48"
#   fn <- "SRI_Watersheds_shapefile_J"
#   dir <- "\\\\LAR-FILE-SRV/Data/NPS/GitHub/2015.06.11/inst/doc/Shapefiles"
#   unequal.var <- "C"
#   outobj <- "sdraw.2015.06.11.160942"
#   over.n <- 2
#   seed <- 1234
#   stype <- "GRTS"
#   alloc.type <- "continuous"
#   testShp <- readOGR(dir,fn)      # shpfile object for testing.
  
  
#   spplot(shp, zcol="Shape_Leng", colorkey=TRUE, col.regions = bpy.colors(100))   
#   points(samp) 
  
  
  
  n <- dat$n.entry$getText()
  fn <- dat$shape.in.entry$getText()
  dir <- dat$shape.in.dir$getText()
  unequal.var <- dat$unequal.var.entry$getText()
  outobj <- dat$out.r.entry$getText()
  over.n <- dat$over.entry$getText()
  seed <- dat$seed.entry$getText()
  stype <- dat$samp.type.combo$getActiveText()
  stype <- substring(stype, 1, 4)
  
  #   Set seed if there is a number present
  if( nchar(seed) > 0 ){
    seed <- as.numeric( seed )
    set.seed( seed )
  }
  
  #   Get sample allocation information from radio buttons
  if( dat$cont.rb$getActive() ){
    alloc.type <- "continuous"
  } else if ( dat$const.rb$getActive() ){
    alloc.type <- "constant"
  } else {
    alloc.type <- "uneqproportion"
  }
  ## will need a function call here to use sample allocation info to determine sample sizes
  ## first step is to get it to read a user-input vector of strata sample sizes
  
  # actually, this can be generalized to a function that determines the sample sizes of the strata,
  # regardless of whether they are proportional, constant, or user-defined
  #
  # strata.sizes <- function(alloc.type, n.strata=NULL)
  # 
  #}
  # #   Check input parameters
  if( length(n) == 0 | (n <= 0) ){
    error.message("Sample size not specified or is less than 0.")
    return()
  }
  
  if( nchar(fn) == 0 | (fn == " ") ){
    error.message("Input shape file not specified.")
    return()
  }
  
  #     if( exists( outobj, where=.GlobalEnv ) ){
  #         #   Ideally, we could ask the user here if they want to overwrite.
  #         #   This should be easy using RGtk windows, but I am in a rush.
  #         #   For now, just save a backup copy
  #         assign( paste(outobj, ".previous", sep=""), get(outobj, pos=.GlobalEnv), pos=.GlobalEnv)
  #         cat( paste( "Old version of", outobj, "copied to", paste(outobj, ".previous", sep=""), "\n"))
  #     }
  
  
  #   fix up the sample sizes
  # n <- as.numeric(as.vector( n ))
  # over.n <- as.numeric(as.vector(over.n))
  if( nchar(over.n) == 0 ){ #right now this just works on 1 oversample, which is fine
    over.n <- 0
  } else {
    over.n <- as.numeric( over.n ) #whatever the over.n is, we can use that in each strata
  }
  if( is.na(over.n) ){
    warning("Oversample set to 0.")
    over.n <- 0
  }
  
  #   Actually draw the sample
  #   Remember that fn is the text string name of the shapefile, without .shp, and without path.
  
  samp <- switch( stype, 
                  #"BAS " = draw.bas(n,over.n,fn),
                  "GRTS" = draw.unequal.grts(n,over.n,unequal.var,alloc.type,fn,dir), 
                  #"SSS " = draw.sss(n,over.n,fn),
                  stop(paste("Unknown sample type:",stype)))
  
  #   Save the sample in global environment.  Type of sample is an attribute.                         
  print("back from draw.unequal.grts in run.unequal.sample")
  assign( outobj, samp, pos=.GlobalEnv )
  
  #   Tell user we are finished.
  cat("First 10 sample locations:\n")
  print(samp[1:10,])
  
  dialog <- gtkMessageDialogNew(NULL, c("modal"), "info", "ok", stype, "draw successful.")
  dialog$run()
  dialog$destroy()
  
}
