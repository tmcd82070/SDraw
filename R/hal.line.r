#' @export hal.line
#' 
#' @title  Draws a Halton Lattice sample from a linear (line) resource .
#' 
#' @description  Draws a Halton Lattice sample from a \code{SpatialLines*} object.
#' 
#' @details  A HAL sample is drawn from the union of all lines in \code{x} by 
#' discretization of lines using points spaced \code{pt.spacing} apart. The
#' discretized points are then sampled using the HAL method
#' for points (see \code{\link{hal.point}}).
#' 
#' @param n Sample size.  Number of locations to draw from the set of all lines
#' contained in \code{x}.
#' @param x A SpatialLines or SpatialLinesDataFrame object. This object must
#' contain at least 1 line.  If it contains more than 1 line, the HAL sample is
#' drawn from the union of all lines.
#' @param J A 2X1 vector of base powers.  \code{J[1]} is for horizontal,
#' \code{J[2]} for vertical dimension. \code{J} determines the size and shape
#' of the smallest Halton boxes. There are \code{bases[1]^J[1]} vertical columns 
#' of Halton boxes over \code{x}'s bounding box, and \code{bases[2]^J[2]} 
#' horizontal rows of Halton boxes over the bounding box, for a total 
#' of \code{prod(bases^J)} total boxes.  The dimension of each box is 
#' \code{c(dx,dy)/(bases^J)}, where \code{c(dx,dy)} are the horizontal and 
#' vertical extents of \code{x}'s bounding box.  If \code{J=NULL} (the default),
#' \code{J} is chosen so that Halton boxes are as square as possible.
#' 
#' 
#' @param balance Option specifying how spatial balance is maintained. The options
#' are "1D" or "2D".  
#' 
#' Under "1D" all lines in \code{x} are stretched straight 
#' and laid end-to-end in the order the appear in \code{x} and a 1-dimensional 
#' BAS sample is taken from the amalgamated line.  Sample locations are then 
#' mapped back to two dimensional space and appear on the original lines.  This
#' method maintains 1D spatial balance, but not 2D balance.  Spatially 
#' balanced samples in 1D may not look spatially balanced when plotted 
#' in 2 dimensions.   
#' 
#' Under "2D" a systematic sample of points along the union of all lines 
#' in \code{x} is drawn first, and a 2-dimensional BAS sample of the points
#' is drawn (see \code{\link{bas.point}}).  This maintains 2D spatial 
#' balance of sample locations on the lines, but is slower than "1D". 
#' 
#' @param frame.spacing If \code{balance == "2D"}, this is the desired spacing of 
#' points on lines prior to sampling via
#' HAL.  If \code{balance == "2D"}, the first step is discretization of lines by 
#' placing 
#' equally-spaced points on all lines. Then, points are sampled using Halton sampling
#' (see \code{hal.point}) for points.  This parameter controls spacing of points during the
#' discretization of lines.  For example, specifying 50, and assuming
#' \code{x} is projected to UTM meters, means points will be placed every 50
#' meters along all lines in \code{x} before sampling. \code{x} should be projected before
#' sampling so that \code{pt.spacing} makes sense.  If \code{pt.spacing} is not
#' specified and \code{balance == "2D"}, 1000*\code{n} points will be placed along 
#' all lines during
#' discretization.
#' 
#' @param bases If \code{balance == "2D"}, this is a 2X1 vector of co-prime Halton bases.  
#' If \code{balance == "1D"}, this is the single (scalar) Halton base to use.  
#' If \code{length(bases) == 2} and \code{balance == "1D"}, the first element of 
#' \code{bases} is used.
#'
#' @return A \code{SpatialPointsDataFrame} containing locations in the HAL sample, 
#' in HAL order.
#'  Attributes of the sample points are: 
#' \itemize{
#'   \item \code{sampleID}: A unique identifier for every sample point.  This 
#'   encodes the HAL order.  \code{return[order(return$sampleID),]} will sort the 
#'   returned object in HAL order. \code{sampleID}'s, in the HAL case, are not 
#'   consecutive. \code{sampleID}'s are usually the Halton 
#'   indices for the Halton boxes 
#'   containing the point, after adding random cycles for multiple points in 
#'   the same box (see \code{\link{halton.frame}}). If the sample 
#'   cycled around
#'   to the beginning of the frame, because random start 
#'   fell at the end, the sample number is appended 
#'   to the beginning of the normal \code{sampleID}'s so they
#'   will sort the frame in the proper order.
#'   
#'   
#'   \item \code{geometryID}: The ID of the line in \code{x} on which each 
#'   sample point falls.  The 
#'   ID of lines in \code{x} are \code{row.names(x)}. 
#'   \item Any attributes of the original lines (in \code{x}). 
#' }
#'
#' Additional attributes of the output object, beyond those which 
#' make it a \code{SpatialPointsDataFrame}, are:
#' \itemize{
#'    \item \code{frame}: Name of the input sampling frame.
#'    \item \code{frame.type}: Type of resource in sampling frame. (i.e., "line").
#'    \item \code{sample.type}: Type of sample drawn. (i.e., "BAS").
#'    \item \code{balance}: The type of balance ("1d" or "2d").
#'    \item \code{random.start}: The random seed for the random-start 
#'    1D or 2D Halton sequence 
#'    that produced the sample.  
#'    If \code{balance=="1D"}, 
#'    \item \code{random.start}: The random start of the sample in the 
#'    1D or 2D Halton
#'    frame.  The Halton frame is a list of all points sorted in 
#'    Halton order.  Halton order is the Halton index of each point, with 
#'    random cycles added to multiple points 
#'    in the same Halton box.  If \code{balance=="2D"},
#'    this is a random number between 0 and the number of points 
#'    in the discretization of \code{x} (see \code{frame.spacing}).
#'    If \code{balance=="1D"}, this is a random number between 0 
#'    and the number of points in the 1D Halton lattice 
#'    discretization of \code{x} (see parameters \code{J} and \code{eta}).
#'    The sample consists of the 
#'    \code{n} consecutive units starting at \code{random.start+1} in 
#'    the sorted Halton frame. 
#'    
#' }
#' 
#' 
#' @author Trent McDonald
#' @seealso \code{\link{bas.line}}, \code{\link{hal.point}}, \code{\link{hal.polygon}}, 
#' \code{\link{sdraw}}
#' 
#' @keywords design survey
#' 
#' @examples
#' 
#' # Default sample of Hawaii coastline. 1D balance
#' samp <- hal.line( HI.coast, 100 )
#' 
#'    
#' # Frame spacing ~157 meters. 
#' # Frame has ~10,000 = lineLength(HI.coast)/157 points
#' samp <- hal.line( HI.coast, 100, balance="2D", frame.spacing=157.1855)
#'   
#' # Different Halton lattice 
#' samp <- hal.line( HI.coast, 100, J=c(5,3), balance="2D", frame.spacing=157.1855)
#' 
#' 
hal.line <- function( x, n, J=NULL, eta=c(1,1), bases=c(2,3), balance="1D", 
                      frame.spacing = NULL){

  
  if( !inherits(x, "SpatialLines") ) stop("Must call hal.line with a SpatialLines* object.")

  # Check for pathological lines
  if( length(x@lines) < 1 )
    stop("No lines in SpatialLines object")
  
  L <- lineLength(x)
  if (L < .Machine$double.eps) 
    stop("Lines object of no length")
  
    
  #   Check n
  if( n < 1 ){
    n <- 1
    warning("Sample size less than one has been reset to 1")
  }

  # do the work, either 2d or 1d balance
  if( tolower(balance) == "2d"){
    if(is.null(frame.spacing)){
      frame.spacing <- L / (1000*n)
    } 
    
    #   Discretize the line with n.pixel.factor more points than needed for sample
    pt.frame <- sss.line( x, spacing=frame.spacing )
    
    # rename geometryID and drop sampleID
    names(pt.frame)[names(pt.frame) == "geometryID"] <- "lineID"
    pt.frame <- pt.frame[,names(pt.frame) != "sampleID"]
    
    #   Sample as points
    samp <- hal.point( pt.frame, n, J=J, bases=bases )
    
    # Drop sampleID and fix up geometryID 
    samp <- samp[,names(samp) != "geometryID"]
    names(samp)[names(samp) == "lineID"] <- "geometryID"
    
    row.names(samp) <- 1:length(samp)
    
    attr(samp, "frame") <- deparse(substitute(x))  
    attr(samp, "frame.type") <- "line"
    attr(samp, "frame.spacing") <- frame.spacing
    attr(samp, "balance") <- tolower(balance)
  } else {
    # place Halton lattice in 1D =============================================
    # Get all coordinates from all lines "back to back" in a matrix
    # parameterized line is ("length", "x", "y") or ("l","x","y")
    mline.ids <- merge.lines(x)
    mline <- mline.ids$geometry
    mline.ids <- mline.ids$IDs
    
    # Total length of all lines
    tot.len <- mline[nrow(mline),"l"]
    
    # place halton lattice in 1D on line
    hl.lattice <- halton.lattice(matrix(c(0,tot.len),nrow=1), J=J[1], 
                                 eta=eta[1], bases = bases[1])
    

    hl.bbox <- attr(hl.lattice,"hl.bbox") #save for attributes later
    J <- attr(hl.lattice, "J")
    
    # Compute Halton indices in 1D ==========================
    hl.ind <- halton.indices( hl.lattice, 
                               J=attr(hl.lattice,"J"), 
                               bases=attr(hl.lattice,"bases"), 
                               hl.bbox = hl.bbox)
    
    # Construct the Halton frame =============================
    hl.ind <- halton.frame(hl.ind)
    
    # Draw random start and sample ===========================
    # Draw sample from the frame
    N <- nrow(hl.ind)
    m <- floor(runif(1, 0, N)) # Integer 0,...,N-1
    n <- min( n, N )  # Can't take more than a census. 
    ind <- (((0:(n-1))+m) %% N ) + 1  # Cycle the indices around to start of frame if necessary
    
    
    samp <- hl.ind[ind,]
    
    hl.ind <- samp[,attr(hl.ind,"order.name")]
    if( m+n > N ){
      # We have cycled. Fix up sort index
      hl.digits <- floor(log10(max(hl.ind))) + 1
      hl.ind <- (1:length(hl.ind)) * 10^hl.digits + hl.ind
    } 
    
    l.out <- samp[,names(hl.lattice)]
    
    # Map distances back to 2D ========================
    # Extract or compute points on the parameterized line, and indices (tt)
    x.out <- aprox( mline[,"l"], mline[,3], l.out)
    y.out <- aprox( mline[,"l"], mline[,4], l.out)
    t.out <- aprox( mline[,"l"], mline[,"t"], l.out)
    
    # Extract line ID's at each point
    geoID.out <- mline.ids[ceiling(t.out)]
    
    # output ===========================================================
    
    crds <- data.frame(x.out,y.out)
    names(crds)<- dimnames(mline)[[2]][3:4]
    row.names(crds) <- 1:length(x.out)
    samp<-SpatialPoints( crds, proj4string = CRS(proj4string(x)) )
    
    if( inherits(x, "SpatialLinesDataFrame") ){
      # x has attributes, extract them at the points
      df <- data.frame(x)[geoID.out, ]
      df <- data.frame( sampleID=hl.ind, 
                        geometryID=geoID.out, df)
      row.names(df) <- 1:length(x.out)
    } else {
      df <- data.frame( sampleID=hl.ind, 
                        geometryID=geoID.out )
      row.names(df) <- 1:length(x.out)
    }
    samp <- SpatialPointsDataFrame(samp, df, proj4string = CRS(proj4string(x)), match.ID = TRUE)
    
    attr(samp, "frame") <- deparse(substitute(x))
    attr(samp, "frame.type") <- "line"
    attr(samp, "sample.type") <- "HAL"
    attr(samp, "J") <- J[1]
    attr(samp, "bases") <- bases[1]
    attr(samp, "eta") <- eta[1]
    attr(samp, "hl.bbox") <- hl.bbox
    attr(samp, "balance") <- tolower(balance)
    attr(samp, "random.start") <- m
  }

  samp

}


# ----- Some examples

#  samp<- hal.line(100, HI.coast)
#  plot(HI.coast)
#  points(samp)
