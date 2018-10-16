#' @export hip.lattice.point
#' 
#' @title Halton Iterative Partition lattice inside a \code{bbox} (bounding box) matrix object.
#' 
#' @description Constructs an iteratively partitioned lattice of Halton boxes (a Halton lattice) inside a 
#' bounding box \code{bbox} of the sample space. This method does the 
#' hard work of partitioning the boxes to sample from. It is meant to be used internally by 
#' \code{hip.point} only. 
#' 
#' @param coords A \code{data.frame} of the coordinates from the sample space.
#' 
#' @param J A 2X1 vector of base powers which determines the size and shape 
#' of the Halton boxes. See additional description in help for 
#' \code{\link{hip.polygon}} function.  
#' 
#' @param bases A 2X1 vector of Halton bases.  These must be co-prime. 
#' 
#' @return A \code{list} of \code{data.frames} containing locations in the Halton lattice of the 
#' partitioned boxes
#' 
#' @details This routine is called internally by \code{hip.point}, and is not 
#' normally called by the user. This should be avoided.
#' 
#' @author Michael J Kleinsasser
#' @seealso \code{\link{hip.point}}, \code{\link{hip.polygon}}, \code{\link{hip.lattice.polygon}}
#' @keywords design survey
#' @examples
#'
#' # Take a simple HIP lattice for illustration
#' # make a plot for test
#' plot(x = c(0,1), y = c(0,1), type="n")
#'
#' # create data.frame of points to plot
#' pnts <- matrix(c(0.2, 0.2,
#'                  0.1, 0.5,
#'                  0.3, 0.9,
#'                  1.0, 0.5,
#'                  0.9, 0.6,
#'                  0.7, 0.7), nrow = 6, byrow = T)
#' pnts <- data.frame(pnts)
#'
#' # plot data.frame of points
#' points(pnts)
#'
#' # Test of hip.lattice.point.r
#' hip.lattice.point(coords = data.frame(pnts), J = c(1,1), bases = c(2,3)) 
#' # output ordering of points are Halton order
#' 
# Function to order points in same order obtained through hip.lattice.polygon
# coords is a data.frame containing the coordinates X,Y to sample from
hip.lattice.point <- function(coords, J, bases, ...) {
  
  # Method of splitting coordinates up into groups of boxes
  # colist is list of points (coordinates (X,Y)) to split into boxes
  splitPoints <- function(coma, base, dimension) {
    # coma <- t(data.frame(colist)); rownames(coma) <- NULL
    pointper <- floor(length(coma[,1]) / base) # number of points per box after split
    # order points by the x dimension if dimension == 1
    if(dimension == 1) {
      # order points from min x to max x
      ord <- coma[order(coma[,1]),]
      # Take care of extra point if there is one
      if( length(coma[,1]) %% base == 1 ) { #there is an extra point
        ord <- rbind(ord, ord[length(ord[,1]),])
        pointper <- floor(length(ord[,1]) / base)
      }
      # split ordered data.frame of points into sets of size pointper
      # list of vectors to subset rows of ord by
      subvec <- list(); cd <- 0; box <- list()
      for(i in 1:base) {
        subvec[[i]] <- (1 + cd):(pointper + cd)
        cd <- cd + pointper
        # need to adapt for set with remainder terms
        # subset the coma into data.frames/matrices for each new box
        box[[i]] <- data.frame(ord[subvec[[i]],])
      }
    } else if(dimension == 2) { # order points by the y dimension if dimension == 2
      # order points from min x to max x
      ord <- coma[order(coma[,2]),]
      # Take care of extra point if there is one
      if( length(coma[,1]) %% base == 1) { #there is an extra point
        ord <- rbind(ord, ord[length(ord[,1]),])
        ord <- rbind(ord, ord[length(ord[,1]),])
        pointper <- floor(length(ord[,1]) / base)
      } else if( length(coma[,1]) %% base == 2) {
        ord <- rbind(ord, ord[length(ord[,1]),])
        pointper <- floor(length(ord[,1]) / base)
      }
      # split ordered data.frame of points into sets of size pointper
      # list of vectors to subset rows of ord by
      subvec <- list(); cd <- 0; box <- list()
      for(i in 1:base) {
        subvec[[i]] <- (1 + cd):(pointper + cd)
        cd <- cd + pointper
        # need to adapt for set with remainder terms
        # subset the coma into data.frames/matrices for each new box
        box[[i]] <- data.frame(ord[subvec[[i]],])
      }
    }
    
    # return list of data.frames/matrices for each box
    return(box)
  }
  
  is.xpower <- FALSE # Be done with each dimension?
  is.ypower <- FALSE
  
  # Use split points method to construct list iteratively
  stList <- list()
  stnList <- list()
  
  stList[[1]] <- coords
  for(i in 1:(max(J))) {
    
    if(i > J[1]) is.xpower = TRUE
    if(i > J[2]) is.ypower = TRUE
    
    # count = 1
    for(j in (1:2)) { # iterate over each dimension
      
      if(is.xpower == TRUE && j == 1) next
      if(is.ypower == TRUE && j == 2) break # needs work
      
      count = 1
      
      for(k in stList) {
        stnList[[count]] <- splitPoints(coma = k, base = bases[j], dimension = j)
        count = count + 1
      }
      
      # convert list of list of data.frames to simply a list of data.frames
      co <- 1
      for(p in stnList) {
        for(u in p) {
          stList[[co]] <- u
          co <- co + 1
        }
      }
    }
    # reset count at each iteration 
    count <- 1
    # reset stnList for each iteration
    stnList <- list()
    
  }
  # Return list of data.frames for each box in order
  return(stList)
  
}









