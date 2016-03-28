#' @export sss.line
#' 
#' @title Draw a Simple Systematic Sample (SSS) from a linear resource. 
#' 
#' @description Draws a systematic sample from a \code{SpatialLines*} object.  The 
#' \code{SpatialLines*} object represents a 2-dimensional line resource, such as a
#' river, highway, or coastline.  
#' 
#' @details If \code{x} contains multiple lines, the lines are amalgomated before
#' sampling.   Conceptually, under amalgomation the lines in \code{x} are 
#' "stretched" straight and laid end-to-end in order of appearence in \code{x}.
#' The simple systematic sample is then drawn from the amalgomated line. 
#' Finally, sample points on the amalgomated line are mapped back to 2-dimensional 
#' space to fall on the lines in \code{x}. 
#' 
#' Note that spacing between sample points is enforced on the amalgomated 
#' line, and may not look correct if the lines loop back on themselves. 
#' For example, consider a line tracing a circle.  The spacing between 
#' the first and last sample point along the circle will not be the 
#' prescribed \code{spacing} because the circle starts between them. 
#' Spacing of all other points (2 to n-1) will be as requested. 
#'
#' 
#' @param n Sample size.  Number of points to draw from the set of all lines
#' contained in \code{x}.  Specification of \code{n} takes precedence 
#' over specification of \code{spacing}.
#' 
#' @param x A \code{SpatialLiness} or \code{SpatialLinessDataFrame} object. 
#' This object must contain at least 1 line.  
#' 
#' @param spacing Assuming, \code{n} is not given, this is the distance 
#' between sample points on the amalgomated line 
#' in \code{x}. For example, if \code{x} is projected 
#' in UTM coordinates and \code{spacing=100}, the returned sample has one point 
#' every 100 meters along the amalgomated line in \code{x}. Keep in mind that the start
#' of line i+1 in \code{x} may not coencide with the end of line i in \code{x}, and that 
#' lines in \code{x} may not be straight.  Thus, 2-dimensional distances between
#' sample points will not, in general, equal \code{spacing}.   
#' 
#' @param random.start Whether to start the sequence of points at a 
#' random place.  If \code{TRUE}, a random uniform variate is selected 
#' between 0 and either \code{spacing} or (length/\code{n}) and the first 
#' location is placed at that location along the line.  Subsequent points occur
#' every \code{spacing} units along the lines.  If \code{random.start==FALSE}, 
#' the first sample point occurs at 0 (first vertex of the lines).
#' 
#' @return A SpatialPointsDataFrame containing locations in the SSS sample, in
#' order along the amalgomated line.  Those on line 1 appear first, those on line 2 
#' second, etc. Attributes of the sample points (in the 
#' embedded data frame) are 
#' as follows: 
#' \itemize{
#'   \item \code{siteID}: A unique identifier for every sample point.  
#'   \code{siteID} starts with 1 at the first point and 
#'   increments by one for each.  \code{siteID} orders 
#'   sample points along the amalgomated line.
#'   \item \code{geometryID}: The ID of the lines object in \code{x} on which each 
#'   sample point falls.  The 
#'   ID of lines in \code{x} are \code{row.names(geometry(x))}. 
#'   \item Any attributes of the original lines (in \code{x}) on which each sample 
#'   point falls.
#' }
#'
#' 
#' Additional attributes of the output object, beyond those which 
#' make it a \code{SpatialPointsDataFrame}, are:
#' \itemize{
#'    \item \code{frame}: Name of the input sampling frame.
#'    \item \code{frame.type}: Type of resource in sampling frame. (i.e., "line").
#'    \item \code{sample.type}: Type of sample drawn. (i.e., "SSS").
#'    \item \code{sample.spacing}: The spacing between sample points along the 
#'    amalgomated line. This is the input \code{spacing} parameter if specified,
#'    or is computed as (length/n) if \code{n} is specified.
#'    \item \code{random.start}: Whether random start was requested.
#' }
#' 
#' @author Trent McDonald
#' @seealso \code{\link{sss.polygon}}, \code{\link{sss.point}}, \code{\link{sdraw}}
#' @keywords design survey
#' @examples
#' 
#' # Draw fixed number of equi-distant points
#' HI.samp <- sss.line( HI.coast, 100 )   
#' plot( HI.coast, col=rainbow(length(HI.coast)) )
#' points( HI.samp, col="red", pch=16 )
#' 
#' # Draw points every 20 km along Hawaii's coastline
#' HI.samp <- sss.line( HI.coast, spacing=20000 )   
#' plot( HI.coast, col=rainbow(length(HI.coast)) )
#' points( HI.samp, col="red", pch=16 )
#' 
#' 
#' 
sss.line <- function(x, n, spacing, random.start=TRUE){

  if( !(inherits(x, "SpatialLines")) ) stop("Must call sss.line with a SpatialLines* object.")

  if(!missing(n) & !missing(spacing)) warning("n and spacing both specified in sss.line.  n is being used.")

  # check input parameters
  if( !missing(n)){
    if (!is.finite(n) || n < 1) return(NULL)
  }
  
  # Merge lines functtion ==========================================
  merge.lines <- function(x){
    # "merge" all the lines into one big matrix of coordinates with NA's between
    
    # keep in mind that x@lines[[1]] could be 2+ Lines. 
    # This unlists the Lines objects to produce one list of coordinates
    # which is different than coordinates(x)
    tmp <- lapply( unlist(x@lines), slot, "Lines")
    l.id <- sapply( x@lines, slot, "ID")
    nline <- sapply(tmp,length) # num Line objects in each Lines object
    tmp <- lapply( unlist(tmp), slot, "coords")

    # Construct matrix of all coordinates, with duplicate 
    # indicies and lengths between 
    # lines. Do this so that aprox (below) works.
    merged.line <- matrix(NA, length(unlist(tmp))/2, 5)
    l.id <- l.id[rep(1:length(l.id)), nline]  # reps out ID's so loop works
    strt <- 1
    strt.tt <- 1
    strt.len <- 0
    for(i in 1:length(tmp)){
      l1 <- tmp[[i]]
      l1.id <- l.id[i]
      l1.seg.lengths = cumsum(LineLength(l1,sum=FALSE))
      l1.seg.lengths = strt.len + c(0,l1.seg.lengths)
      
      end <- strt + nrow(l1) - 1
      tt <- seq(strt.tt, length=nrow(l1))
      
      merged.line[strt:end,1] <- tt
      merged.line[strt:end,2] <- l1.seg.lengths
      merged.line[strt:end,3:4]<-l1
      merged.line[strt:end,5] <- l1.id
      
      strt.tt <- tt[length(tt)] 
      strt <- end + 1
      strt.len <- l1.seg.lengths[length(l1.seg.lengths)]
    }
    
    if( is.null(cnms <- coordnames(x))) cnms <- c("x", "y")
    dimnames(merged.line)<- list(NULL, c("t","l",cnms,"geometryID"))
    merged.line
  }

  # My approx function ===============================================
  aprox <- function( x, y, x.out ){
    y.out <- rep(NA, length(x.out))
    
    for( i in 1:length(x.out)){
      if( all(is.na(l.x <- which(x < x.out[i])))  ){
        y.out[i] <- y[1]
        next
      } 
      l.x <- max(l.x)
      
      if( all(is.na(u.x <- which(x > x.out[i])))  ){
        y.out[i] <- y[length(y)]
        next
      } 
      u.x <- min(u.x)
      
      if( any(e.x <- which(x == x.out[i]))){
        y.out[i] <- y[e.x[length(e.x)]]
        next
      }
      
      y.out[i] <- y[l.x] + (y[u.x]-y[l.x])*(x.out[i] - x[l.x])/(x[u.x]-x[l.x])
    }
    y.out
  }
  
  
  # Main code ========================================================
  # Get all coordinates from all lines "back to back" in a matrix
  mline <- merge.lines(x)
  

  # Figure out l.out sequence along parameterized line ("l","x","y")
  tot.len <- mline[nrow(mline),"l"]
  if(!missing(n)){
    # figure out spacing. Want n equally space points along 
    # entire length of line.
    spacing <- tot.len / n
  } 
  l.out <- seq(0,tot.len-spacing,by=spacing)

  if( random.start ){
    l.out <- l.out + runif(1, 0, spacing)
  }

  x.out <- aprox( mline[,"l"], mline[,3], l.out)
  y.out <- aprox( mline[,"l"], mline[,4], l.out)
    
Here!! do something like a.out<-aprox( mline[,"l"], mline[,"t"], l.out)
then ids <- mline[a.out,"geometryID"]
then df <- data.frame(x)[ids,]

  # output ===========================================================
  crds <- data.frame(x.out,y.out)
  names(crds)<- dimnames(mline)[[2]][3:4]
  samp<-SpatialPoints( crds, proj4string = CRS(proj4string(x)) )

  #   Add additional attributes
  attr(samp, "frame") <- deparse(substitute(x))
  attr(samp, "frame.type") <- "line"
  attr(samp, "sample.type") <- "SSS"
  attr(samp, "sample.spacing") <- spacing
  attr(samp, "random.start") <- random.start
  
  samp
}

# HERE!!! make this return a SpatialPointsDataFrame
# Update documentaiton to reflect return of spatialpointsdataframe
# test with SpatialPoints object (no data frame)
# test with lat long projection.

# tmp <- sss.line(HI.coast, spacing=20000)
# plot(HI.coast[2,])
# points(tmp,pch=16)
# points(tmp4[1],tmp4[2],pch=15,col="red")

# tmp <- sss.line(Sl, 10)
# plot(Sl)
# points(tmp)
