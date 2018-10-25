#' @export hip.partition
#' 
#' @title Halton Iterative Partition lattice inside a \code{bbox} (bounding box) matrix object.
#' 
#' @description Constructs an iteratively partitioned lattice of Halton boxes (a Halton lattice) inside a 
#' bounding box \code{bbox} of the sample space. This method does the 
#' hard work of partitioning the boxes to sample from. It is meant to be used internally by 
#' \code{hip.polygon} only. 
#' 
#' @param box A \code{bbox} bounding box for the sample space.
#' 
#' @param J A 2X1 vector of base powers which determines the size and shape 
#' of the Halton boxes. See additional description in help for 
#' \code{\link{hip.polygon}} function.  
#' 
#' @param bases A 2X1 vector of Halton bases.  These must be co-prime. 
#' 
#' @return A \code{list} of \code{matrices} containing locations in the Halton lattice of the 
#' partitioned boxes
#' 
#' @details This routine is called internally by \code{hip.polygon}, and is not 
#' normally called by the user. This should be avoided
#' 
#' @author Michael J Kleinsasser
#' @seealso \code{\link{hip.polygon}}, \code{\link{hip.point}}, \code{\link{hip.lattice.point}}
#' @keywords design survey
#' @examples
#'
#' # Take a simple HIP lattice for illustration
#' # nboxes = 2^3 * 3^2 = 72 
#' lat1 <- hip.lattice(box = matrix(data = c(0,1,0,1), nrow = 2, byrow = TRUE),
#'                    J = c(3,2),
#'                    bases = c(2,3))
#' 
#' # legth lat1, should be 72
#' length(lat1)
#' # prep points for plotting
#' trans <- list()
#' i=1
#' for(mat in lat1) {
#'   trans[[i]] <-   t(mat)
#'   i=i+1
#' }
#' # plot points 
#' plot(c(0,1),c(0,1))
#' 
#' for(mat in trans) {
#'   points(mat[1,1],mat[1,2])
#'   points(mat[2,1],mat[2,2])
#' }
#' 
#' 
hip.partition <- function(pnts, box, samp, J, bases=NULL, ...) {
  
  # Note: all coords must plot inside box.
  box <- t(box)
  delta <- apply( box, 1, diff )   # size/extent of box in each dimension
  
  D <- nrow( box ) # number of dimensions
  
  if(is.null(bases)){ # If bases not specified, retrieve them
    
    bases <- primes(D) 
    
  } else if(length(bases)!=D){
    
    stop("Dimensions must equal length of bases. Make nrow(box) == length(bases)")
    
  }
  
  sampSplits <- halton.coefficients(samp, J, bases)
  ans <- hip.part.along.d( 1, J, pnts, box, sampSplits, bases )
  
  
  n.boxes <- bases^J

  
  
  # Function to find the split points in each dimension 
  splitPoints <- function(givenBox, base, dimension) { 
    
    # the number of splits to be made in chosen dimension
    numsplits <- base - 1 
    
    # gets length of x and y dimensions of givenBox to split
    deltaSplit <- apply(givenBox, 1, diff)
    
    # length of space from old coordinate to new coordinate in each dimension
    lengthCoor <- deltaSplit[dimension] / base 
    
    numboxes <- base # How many boxes are we returning as a list?
    
    # Store coordinates of splits 
    splits <- matrix(data = NA, nrow = (numboxes * 2), ncol = 2)
    count <- 0
    if (dimension == 1) {
      for(i in 1: (numsplits + 1)) {
        splits[i+count,] <- c(givenBox[1,1] + (i-1)*lengthCoor, givenBox[2,1])
        splits[i+count+1,] <- c(givenBox[1,1] + i*lengthCoor, givenBox[2,2]) 
        count = count + 1
      }
    } else if (dimension == 2) {
      for(i in 1: (numsplits + 1)) {
        splits[i+count,] <- c(givenBox[1,1], givenBox[2,1] + (i-1)*lengthCoor)
        splits[i+count+1,] <- c(givenBox[1,2], givenBox[2,1] + i*lengthCoor) 
        count = count + 1
      }
    } else {
      stop("HIP not supported for dimensions greater than 2")
    }
    
    # Return list of matrix objects, with each storing the coordinates of new boxes
    listSplit <- list()
    
    for(i in 1:(numboxes)) {
      listSplit[[i]] <- t(splits[(i*2 - 1):(i*2),])
    }
    
    # Return list of new boxes
    return(listSplit)
    
  }
  
  is.xpower <- FALSE # Be done with each dimension?
  is.ypower <- FALSE
  
  # Use split points method to construct list iteratively
  stList <- list()
  stnList <- list()
  
  stList[[1]] <- box
  for(i in 1:(max(J))) {
    
    if(i > J[1]) is.xpower = TRUE
    if(i > J[2]) is.ypower = TRUE
    
    # count = 1
    for(j in (1:2)) { # iterate over each dimension
      
      if(is.xpower == TRUE && j == 1) next
      if(is.ypower == TRUE && j == 2) break # needs work
      
      count = 1
      
      for(k in stList) {
        stnList[[count]] <- splitPoints(givenBox = k, base = bases[j], dimension = j)
        count = count + 1
      }
      # Convert list of lists to simple list object with all boxes
      stList <- unlist(stnList, recursive = FALSE)
      
      # if(is.ypower == TRUE && is.xpower == FALSE) break
    }
    # reset count at each iteration 
    count <- 1
    # reset stnList for each iteration
    stnList <- list()
    
  }
  
  # store list of index matrices and return
  return(stList)
  
}




