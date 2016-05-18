#' @export halton.frame
#' 
#' @title Construct a Halton sampling frame. 
#' 
#' @description Makes a Halton frame from a set of points that have  
#' their Halton indices attached. This function identifies points in the 
#' same Halton box,  and 
#' randomly adds Halton cycles to geographically separate nearby points. 
#' The frame is then sorted by the new frame indices for sampling.
#' 
#' @param x Either a data frame or a \code{SpatialPointsDataFrame} object.  In 
#'  particular, the output of \code{halton.indices} is acceptable.
#'  The data frame, or data frame of the \code{SpatialPointsDataFrame}, 
#'  must contain the Halton indices, which is assumed to be named 
#'  \code{attr(x, "index.name")}.  The default name for this column when using output from 
#'  \code{halton.indices} is \code{HaltonIndex}.  
#'  Each row of the data frame is a sampling unit
#'  to be included in the frame, and the Halton index of the unit is the Halton box 
#'  the unit falls in. A \code{SpatialPoints} object without the data frame is not 
#'  acceptable.
#'  
#' @param index.name Name of the Halton index column in the input object. This 
#'  names the column of \code{x} containing the index of the Halton box that contains 
#'  the point represented on that row of \code{x}.  In reality, this could be any 
#'  ordering for the rows in \code{x}.  Duplicate values of this column are randomly 
#'  moved to the end of the frame.  If \code{index.name} is missing (the default), 
#'  \code{x} is assumed to have an attribute names \code{index.name} 
#'     
#' @param order.name Name of the Halton order column in the output object. 
#'  See description of returned object. 
#'  This parameter is saved as an attribute of the output object. 
#'
#' @return A data frame or \code{SpatialPointsDataFrame} suitable for use as 
#'  a sampling frame. The  data frame, or attributes of the points,
#'  contain a new index column separating points in the same Halton box, and the  
#'  output is sorted by this new index.  Returned object has the following 
#'  attributes, 
#'  \itemize{
#'    \item \code{index.name}: Name of the Halton index column used to construct 
#'    the frame.
#'    \item \code{order.name}:  The name of the Halton ordering column. 
#'    This column is unique across rows of the output, and orders 
#'    the output frame, but is not consecutive. This differs from column 
#'    \code{index.name} because points with identical \code{index.name} 
#'    indices have been randomly moved to the end of the frame by adding 
#'    random Halton cycles. 
#'    \item \code{J}: Halton base powers defining lattice of Halton boxes, 
#'    if \code{x} has a \code{J} attribute. 
#'    \item \code{bases}: Base powers defining lattice of Halton boxes, 
#'    if \code{x} has a \code{bases} attribute. 
#'    \item \code{hl.bbox}: Bounding box for lattice of Halton boxes, 
#'    if \code{x} has a \code{hl.bbox} attribute. 
#'  } 
#'   
#' @author Trent McDonald
#'   
#' @seealso \code{\link{halton.indices}}
#'   
#' @examples 
#'# The following is equivalent to hal.point(WA.cities,20,J=c(6,3))
#'
#'# Define Halton lattice
#'attr(WA.cities,"J") <- c(6,3)
#'attr(WA.cities,"bases") <- c(2,3)
#'
#'# Add tiny amount to right and top of bounding box because Halton boxes are 
#'# closed on the left and bottom.  This includes points exactly on top and right 
#'# bounding lines.
#'attr(WA.cities,"hl.bbox") <- bbox(WA.cities) + c(0,0,1,1) 
#'
#'# Compute Halton indices
#'frame <- halton.indices( WA.cities )
#'
#'# Separate points in frame that are in same box
#'frame <- halton.frame( frame )
#'
#'# Draw sample of size 20
#'n <- 20
#'random.start <- floor( runif(1,0,nrow(frame)-1 ) )
#'samp <- frame[ ( ((0:(n-1))+random.start) %% nrow(frame) ) + 1, ]
#'
halton.frame <- function( x, index.name=attr(x,"index.name"), 
                          order.name="HaltonOrder" ){

  # Extract halton index (should probably check for presence first, and exit nicely)
  if( inherits(x, "SpatialPointsDataFrame") ){
    df <- data.frame(x) # we want coordinates to go along with x during ordering
    if( any(tmp <- "optional" == names(df)) ) df <- df[,!tmp] # drop strange "optional" column
    is.points <- TRUE
  } else {
    df <- x
    is.points <- FALSE
  }  

  if(missing(index.name)){
    index.name <- attr(x, "index.name")
  }
  if( !(index.name %in% names(df)) ) stop(paste(index.name, "column not found in data frame."))
  hl.index <- df[,index.name]

  # Find order of indices and save for later
  ord.hl <- order(hl.index)  

  # Sort data frame by halton index.  This has to happen regardless whether theres 1 or multiple 
  # points per box.  If multiple points per box, this puts all points in same box side-by-side in frame.
  df <- df[ord.hl,]
  hl.index <- hl.index[ ord.hl ]   # sort indicies too
  
  # Compute maximum number of points in any Halton box.  This is well defined because halton indecies are integers.
  # It would be interesting to inspect the distribution of points per box here. e.g., hist(mx.count)
  # mx.count <- max( table( hl.index ))
  
  
  if( any(duplicated(hl.index)) ){
    # Find points that have more than one other point in the same box, create cycle variable 
    cycle <- unlist(tapply(rep(1,length(hl.index)), hl.index, 
                           function(x){
                             sample(1:length(x),replace=FALSE)
                           }))

    # Add cycle to hl.index to make it easy to sort outside this routine
    hl.digits <- floor(log10(max(hl.index))) + 1
    cycle.hl.index <- cycle * 10^hl.digits + hl.index

    df <- data.frame(zzzzframe.order=cycle.hl.index, df, row.names=row.names(df))

    # Sort data frame by cycle, hl.index.  This puts points in same box at least one cycle away from each other in frame.
    ord <- order( cycle.hl.index )
    df <- df[ord,]
  } else {
    df <- data.frame(zzzzframe.order=hl.index, df) # a duplicate copy of halton.index when one point per box
  }
  names(df)[names(df)=="zzzzframe.order"] <- order.name

  # Convert to SpatialPOintsDataFrame if input was same
  if( is.points ){
    # Return a SpatialPoints* object
    cnames <- coordnames(x)
    df.nocoords <- df[, -which(names(df) %in% cnames)]
    df <- SpatialPointsDataFrame(df[,cnames], data=df.nocoords, 
                                 proj4string=CRS(proj4string(x)))
  } 
  
  # Add attributes.  Transfer over attributes of x, if it has them
  if(!is.null(tmp <- attr(x,"J"))) attr(df,"J") <- tmp
  if(!is.null(tmp <- attr(x,"bases"))) attr(df,"bases") <- tmp
  if(!is.null(tmp <- attr(x,"hl.bbox"))) attr(df,"hl.bbox") <- tmp
  attr(df,"index.name") <- index.name
  attr(df,"order.name") <- order.name
  
  df
}


