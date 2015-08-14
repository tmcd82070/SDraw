#' @export halton.frame
#' 
#' @title Construction of sampling frame from a Halton lattice and indicies.
#' 
#' @description Makes a Halton frame from a set of points that have  
#' their Halton indicies attached. This function uses a Halton index column
#' to identify points in the same Halton box,  and 
#' randomly adds Halton cycles to separate geographically nearby points in the frame. 
#' The frame is then sorted by the new frame indicies for sampling.
#' 
#'  @param hl.points Either a data frame or a \code{SpatialPointsDataFrame} object.  In 
#'  particular, the output of \code{halton.indicies} is acceptable.
#'  The data frame, or data frame of the \code{SpatialPointsDataFrame}, 
#'  must contain the Halton indicies, which is assumed to be named 
#'  \code{attr(hl.points, "index.name")}.  The default name for this column when using output from 
#'  \code{halton.indicies} is \code{SDraw.HaltonIndex}.  Each row of the data frame is a sampling unit
#'  to be included in the frame, and the Halton index of the unit is the Halton box 
#'  the unit falls in. A \code{SpatialPoints} object without the data frame is not 
#'  acceptable. 
#'  @param order.name Name of the column in the output object by which the frame should 
#'  be sorted to produce the proper sampling order.  This index is unique to all units.  This 
#'  parameter is saved as an attribute of the output object. 
#'
#'  @return A data frame suitable for use as a sampling frame. This is a data frame, 
#'  like \code{hl.points}, but with a new index column separating points in the same Halton box.  
#'  The output data frame is sorted by the new indicies.  New indicies are in column 
#'  specified by the "order.name" attribute (i.e., \code{attr(obj, "order.name")} ). 
#'   
#'   @author Trent McDonald
#'   
#'   @seealso \code{\link{halton.indicies}}
#'   
#'   @examples 
#'# This is the "brute force" method to draw HAL samples. hal.point does all this.
#'
#'# Define Halton lattice
#'attr(WA.cities,"J") <- c(6,3)
#'attr(WA.cities,"bases") <- c(2,3)
#'# Add tiny amount to right and top of bounding box because Halton boxes are 
#'# closed on the left and bottom.  This includes points exactly on the bounding lines.
#'attr(WA.cities,"hl.bbox") <- bbox(WA.cities) + c(0,0,1,1) 
#'
#'# Compute Halton indicies
#'frame <- halton.indicies( WA.cities )
#'
#'# Separate points in frame that are in same box
#'frame <- halton.frame( frame )
#'
#'# Draw sample of size 20
#'n <- 20
#'random.start <- floor( runif(1,0,nrow(frame)-1 ) )
#'samp <- frame[ ( ((0:(n-1))+random.start) %% nrow(frame) ) + 1, ]
#'
halton.frame <- function( hl.points, order.name="SDraw.siteID" ){

  # Extract halton index (should probably check for presence first, and exit nicely)
  if( regexpr("SpatialPointsDataFrame", class(hl.points)) > 0 ){
    df <- data.frame(hl.points)  # use data.frame here because we want the coordinates in df
    is.points <- TRUE
  } else {
    df <- hl.points
    is.points <- FALSE
  }  

  halton.index.name <- attr(hl.points, "index.name")
  
  if( !(halton.index.name %in% names(df)) ) stop(paste(halton.index.name, "undefined or column not found in data frame."))
  hl.index <- df[,halton.index.name]

  # Find order of indicies and save for later
  ord.hl <- order(hl.index)  

  # Sort data frame by halton index.  This has to happen regardless whether theres 1 or multiple 
  # points per box.  If multiple points per box, this puts all points in same box side-by-side in frame.
  df <- df[ord.hl,]
  
  # Compute maximum number of points in any Halton box.  This is well defined because halton indecies are integers.
  # It would be interesting to inspect the distribution of points per box here. e.g., hist(hl.index)
  mx.count <- max( table( hl.index ))
  
  
  if( mx.count > 1 ){
    # Find points that have more than one other point in the same box, create cycle variable 
    hl.index <- hl.index[ ord.hl ]   # sort indicies
    cycle <- unlist(tapply(rep(1,length(hl.index)), hl.index, 
                           function(x,mx){
                             sample(1:mx,size=length(x),replace=F)
                           }, 
                           mx=mx.count))

    # Add cycle to hl.index to make it easy to sort outside this routine
    hl.digits <- floor(log10(max(hl.index))) + 1
    cycle.hl.index <- cycle * 10^hl.digits + hl.index
    df$zzzzframe.order <- cycle.hl.index 
  
    # Sort data frame by cycle, hl.index.  This puts points in same box at least one cycle away from each other in frame.
    ord <- order( cycle.hl.index )
    df <- df[ord,]
  } else {
    df$zzzzframe.order <- hl.index[ ord.hl ]  # a duplicate copy of halton.index when one point per box
  }
  names(df)[names(df)=="zzzzframe.order"] <- order.name
  
  # Convert to SpatialPOintsDataFrame if input was same
  if( is.points ){
    # Return a SpatialPoints* object
    cnames <- coordnames(hl.points)
    df.nocoords <- df[, -which(names(df) %in% cnames)]
    df <- SpatialPointsDataFrame(df[,cnames], data=df.nocoords, 
                                 proj4string=CRS(proj4string(hl.points)))
  } 
  
  # Add attributes
  attr(df,"J") <- attr(hl.points, "J")
  attr(df,"eta") <- attr(hl.points, "eta")
  attr(df,"bases") <- attr(hl.points, "bases")
  attr(df,"hl.bbox") <- attr(hl.points, "hl.bbox")
  attr(df,"triangular") <- attr(hl.points, "trianglular")
  attr(df,"index.name") <- halton.index.name
  attr(df,"order.name") <- order.name
  
  df
}


