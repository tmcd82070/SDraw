hal.line <- function( n, shp, J=NULL, pt.spacing = NULL, bases=c(2,3)){
  #
  #   Draw a HAL sample from a shape containing lines .
  #
  #   input:
  #   n = desired sample size,
  #   shp = a SpatialPolygons* object, according to package sp.
  #   J = 2X1 vector of base powers.  J[1] is for horizontal, J[2] for vertical dimension
  #     J determines the size and shape of the lowest level of Halton boxes. If J=NULL (the default), 
  #     J is choosen so that Halton boxes are as square as possible. 
  #   pt.spacing = spacing of points on lines prior to sampling via HAL.  First step in 
  #     sampling lines is to descritize the lines by placing points on the lines, then sampling
  #     the points using Halton sampling.  If the user wishes, they can specify the spacing 
  #     of points with this parameter.  For example, specifying 50, and assuming the shp is 
  #     projected to UTM meters, means points will be placed every 50 meters along all lines in shp. 
  #     It is best if shp is projected here. Otherwise, if pt.spacing is not specified, the 
  #     algorithm places 1000*n points along the lines during descretization. 
  #   bases = 2X1 vector of Halton bases.  These must be co-prime. 
  
  #   Output:
  #   A SpatialPOints* object containing n HAL sample points, in order. 

  #   Details
  #   If J is missing, the approximate frame size or approximate lattice size is set to 100 times 
  #     the number of samples requested. If J is NULL, the algorithm attempts to place 100*n points in the bounding box 
  #     using Halton boxes that are as close to square as possible.  The lattice size is not exact, but is a target. 
  
  
  if( regexpr("Lines", class(shp)) < 0 ) stop("Must call hal.line with a SpatialLinesX object.")
  
  #   Check n
  if( n < 1 ){
    n <- 1
    warning("Sample size less than one has been reset to 1")
  }

  # If lattice is not specified, set frame size to some multiple of n
  if(is.null(pt.spacing)){
    N <- 1000*n
  } else {
    N <- NULL
  }
  
  #   Discretize the line with many more points than needed for sample
  pt.frame <- spsample( shp, N, type="regular" )
  
  # Now that we have points, we can draw a HAL point sample. 
  samp <- hal.point( n, pt.frame, J, bases )
  
  samp
  
  
}


# ----- Some examples

# samp<- hal.polygon(1000, WA.utm)
# plot(WA.utm)
# points(samp)
