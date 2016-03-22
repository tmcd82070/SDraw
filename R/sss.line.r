#' @export sss.line
#' 
#' @title Draw a Simple Systematic Sample (SSS) from a linear resource. 
#' 
#' @description Draws a systematic sample from a \code{SpatialLines*} object.  The 
#' \code{SpatialLines*} object represents a 2-dimensional line resource, such as a
#' river, highway, or coastline.  
#' 
#' @details This function is simply a wrapper for \code{spsample} in package 'sp'.
#' 
#' @param n Sample size.  Number of locations to draw from the set of all lines
#' contained in \code{x}.
#' @param x A \code{SpatialLiness} or \code{SpatialLinessDataFrame} object. 
#' This object
#' must contain at least 1 line.  If it contains more than 1 line, the sample
#' is drawn from the amalgomation of all lines.  Conceptually, the lines in \code{x} are 
#' "stretched" straight, laid end-to-end in order of appearence in \code{x}, 
#' and the systematic sample is drawn from this amalgomated line. 
#' Points on the amalgomated line are then mapped back to 2-dimensional 
#' space to fall on the lines in \code{x}. 
#' 
#' @param spacing Assuming, \code{n} is not given, this is the distance 
#' between sample points on the amalgomated line 
#' in \code{x}. For example, if \code{x} is projected 
#' in UTM coordinates and \code{spacing=100}, the returned sample has one point 
#' every 100 meters along the amalgomated line in \code{x}. Keep in mind that 
#' line i+1 in \code{x} may not touch the end of line i in \code{x}, and that 
#' lines in \code{x} may not be straight.  Thus, 2-dimensional distances between
#' sample points will not, in general, equal \code{spacing}.   
#' 
#' @return A SpatialPointsDataFrame containing locations in the SSS sample, in
#' order along the amalgomated line.  Those on line 1 appear first, those on line 2 
#' second, etc. 
#' 
#' @author Trent McDonald
#' @seealso \code{\link{sss.polygon}}, \code{\link{sss.point}}, \code{\link{sdraw}}
#' @keywords design survey
#' @examples
#' 
#' 
#' HI.samp <- sss.line( 100, HI.coast )   
#' plot( HI.coast, col=rainbow(length(HI.coast)) )
#' points( HI.samp, col="red", pch=16 )
#' 
#' 
sss.line <- function(x, n, spacing, random.start=TRUE){

  if( !(inherits(x, "SpatialLines")) ) stop("Must call sss.line with a SpatialLines* object.")

  if(!missing(n) & !missing(spacing)) stop("Must specify only one of n or spacing in sss.line")

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
    tmp <- lapply( unlist(tmp), slot, "coords")
  
    # Construct matrix of all coordinates, with c(NA,NA) between lines
    merged.line <- matrix(NA, length(unlist(tmp))/2, 3)
    strt <- 1
    strt.tt <- 1
    for(i in 1:length(tmp)){
      l1 <- tmp[[i]]
      end <- strt + nrow(l1) - 1
      tt <- seq(strt.tt, length=nrow(l1))
      merged.line[strt:end,1] <- tt
      merged.line[strt:end,2:3]<-l1
      strt.tt <- max(tt) 
      strt <- end + 1
    }
    
    if( is.null(cnms <- coordnames(x))) cnms <- c("x", "y")
    dimnames(merged.line)<- list(NULL, c("t",cnms))
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
  
  # Figure out x.out sequence along parameterized line
  if(missing(n)){
    # spacing has been specified
    lens = sapply(x@lines, LinesLength)
    n <- sum(lens) / spacing
  } 
  t.out <- seq(1,mline[nrow(mline),1],by=(mline[nrow(mline),1]-1)/n)
  t.out <- t.out[-length(t.out)]
  
  if( random.start ){
    t.out <- t.out + runif(1, 0, t.out[2]-t.out[1])
  }

  x.out <- aprox( mline[,1], mline[,2], t.out)
  y.out <- aprox( mline[,1], mline[,3], t.out)
    

  # HERE!!! check/change way that spacing is used.  you will need to 
  # cumulatively add lengths along mline to get proper spacing
  
  # output ===========================================================
  SpatialPoints( cbind(x.out, y.out), proj4string = CRS(proj4string(x)) )


}
