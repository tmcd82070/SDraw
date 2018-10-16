#' @export hip.point
#' 
#' @title  hip.point - Halton Iterative Partition (HIP) of point resources. 
#' 
#' @description  Draws a Halton Iterative Partition (HIP) sample from a 
#' \code{SpatialPoints*} object. 
#' 
#' @details  \bold{A brief description of Halton Iterative Partition (HIP) sampling for points:} 
#' Given a set of Halton Iterative Partition parameters
#' \code{x} (SpatialPoints* object) and \code{n} (sample size),
#' a lattice of Halton boxes is constructed iteratively over the bounding box 
#' of \code{x}.  
#' This results in enough Halton boxes on the bounding box to uniquely 
#' cover the point resource.  That is, one and only one point per 
#' box.   
#' The Halton index (the inverse of the Halton sequence) of all boxes 
#' is computed and assigned to points that lie 
#' in each box. Finally, a random number between 0 and the largest Halton index is 
#' drawn, and the next \code{n} points associated with the 
#' next \code{n} Halton boxes are taken as 
#' the sample, wrapping to the beginning if necessary. 
#' 
#' @param n Sample size.  The number locations to draw from the set of points
#' contained in \code{x}. If the sample size returned is less than the desired sample size,
#' increase \code{n} until the desired sample size is reached.
#' 
#' @param x A \code{SpatialPoints} or \code{SpatialPointsDataFrame} object
#' representing the 2-dimensional point resource
#' from which samples are taken. 
#' This object must contain at least 1 point. 
#' 
#' @return A \code{SpatialPoints} objects containing locations in the 
#' HIP sample, in HIP order.
#'
#' Additional attributes of the output object, beyond those which 
#' make it a \code{SpatialPoints}, are:
#' \itemize{
#'    \item \code{frame}: Name of the input sampling frame.
#'    \item \code{frame.type}: Type of resource in sampling frame. (i.e., "point").
#'    \item \code{sample.type}: Type of sample drawn. (i.e., "HIP").
#'    
#'    \item \code{J}: Exponents of the bases used to form the lattice of 
#'    Halton boxes. This is either the input \code{J}, or the \code{J} vector
#'    computed by \code{\link{halton.indices}}.  
#'    
#'    \item \code{bases}: Bases of the Halton sequence used to draw the sample. 
#'    
#'    \item \code{hl.bbox}: The bounding box around points in \code{x} used to 
#'    draw the sample.  See \code{\link{halton.indices}}.
#'  
#' }
#' 
#' 
#' @author Michael J. Kleinsasser
#' 
#' @seealso \code{\link{hip.line}}, \code{\link{hip.polygon}}, \code{\link{SDraw}}, 
#' \code{\link{bas.point}}
#' 
#' @keywords design survey
#' @examples
#' 
#'#   Draw sample of cities in the state of Washington
#'data(WA.cities)
#'samp <- hip.point( WA.cities, 100 )
#'   
#' 
hip.point <- function( x, n, J=NULL ){
  
  ########################## Check inputs ###########################################
  
  if( !(inherits(x, "SpatialPoints"))  ) stop("Must call hal.point with a SpatialPoints* object.")
  
  bases <- c(2,3) # Fix bases to 2 and 3
  
  #   Check n
  if( n < 1 ){
    n <- 1
    warning("Sample size less than one has been reset to 1")
  }
  
  N <- length(x)
  
  if ( n > N ) {
    n <- N
    warning("Sample size is greater than points in the sample frame. 
            n has been reset to the total number of points (i.e., drawing a census).")
  }
  
  #######################################################################################
  
  #######################################################################################
  # If lattice is not specified, set number of boxes
  # to greater than number of points in frame
  if(is.null(J)){
    J <- getJ( N, bases )
  }
  B <- prod(bases ^ J)

  
  ## ---- Before the partition, construct indices of boxes to be sampled
  k <- trunc( runif(1, 0, B) )
  Sk <- (k:(k+n-1)) %% B
  box <- bbox(x)
  coords <- coordinates(x)

  ## ---- Partion the points
  hipPartition <- hip.partition(coords, Sk, J, bases)
  
  HERE!!!
  ############# My halton index function ##################################
  hal.ind <- function(boxn) {
    ## Scale incoming points to between 0 and 1
    ## subtract min(x,y) off of box
    boxn <- boxn - box[,1]
    ## divide by delta
    boxn[1,] <- boxn[1,]/delta[1]
    boxn[2,] <- boxn[2,]/delta[2]
    # Compute a_i's
    powers <- function(x, len, st) { x <- x ^ (st:len); x } # get vector of powers
    a_1 <- sum(floor( ((boxn[1,1]+0.0001) * powers(bases[1], J[1], 1) ) %% bases[1]) * (powers(bases[1], J[1]-1, 0))) # %% bases[1]^J[1]
    a_2 <- sum(floor( ((boxn[2,1]+0.0001) * powers(bases[2], J[2], 1) ) %% bases[2]) * (powers(bases[2], J[2]-1, 0))) # %% bases[2]^J[2]
    av <- c(a_1, a_2) # vector of a_i's
    # CRT (Chinese Remainder Theorem)
    N <- prod(bases^J); y <- N / (bases^J)
    # Use Extended Euclidean Algorithm to get inverse of y_i mod n_i
    z <- extended.gcd(y, bases^J)$t # CHECK with an example !!!
    # for(i in 1:length(z)) { if(z[i] < 0) z[i]<-z[i]+bases[i] }
    sol <- sum(av * y * z) %% N
    # return halton index for a box
    return(sol)
  }
  #######################################################################
  
  # Get halton indices for each box
  hal.indices <- lapply(latt, hal.ind)
  hal.indices <- unlist(hal.indices)
  
  # If you want a plot of the Halton boxes to check:
  # hal.indices <- hal.indices %% prod(bases^J)
  # plotLattice(latt, indices=hal.indices, J=J)
  
  ########################################################################
  ### Here are the guts of HIP.point. We correctly order the points to 
  ### match the Halton sequence, then draw from halton sequense points
  ########################################################################
  
  # Match hal.indices to output from hip.lattice.point
  hal.point.grid <- hip.lattice.point(coords = data.frame(coordis), J = J, bases = bases)
  hal.point.grid <- data.frame(matrix(unlist(hal.point.grid), nrow=length(hal.point.grid), byrow=T))
  
  # construct data.frame of form: X, Y, halton.index
  samp.frame <- data.frame(cbind(hal.point.grid, hal.indices))
  
  ## Now we can take samples:
  
  # eliminate duplicate coordinate sets 
  samp.frame <- samp.frame[!duplicated(samp.frame[,1:2]),]
  
  # Get a random start from the the set of integers 0 to B-1 i.e. k to k+n-1 mod B, where k is random
  Kstart <- samp.frame[sample(1:(length(samp.frame[,3])), 1), 3]
  
  Kvec <- Kstart:(Kstart + n - 1) %% prod(bases^J)
  Kvec <- Kvec[which(Kvec %in% samp.frame[,3])]
  
  # Subsets latt to the matrices we want to sample, in the correct order
  drawSet <- samp.frame[match(Kvec, samp.frame[,3]),]
  drawSet <- drawSet[complete.cases(drawSet),]
  
  # Make draws from drawSet as close to sample size needed as possible
  samp.coords <- drawSet[,1:2]
  
  # draws
  samp <- SpatialPoints(samp.coords, proj4string = CRS(proj4string(x)))
  
  #################################   Returns   ##############################################
  
  # Add attributes
  attr(samp, "frame") <- deparse(substitute(x))
  attr(samp, "frame.type") <- "point"
  attr(samp, "sample.type") <- "HIP"
  attr(samp,"J") <- J
  attr(samp,"bases") <- bases
  attr(samp,"hl.bbox") <- box
  
  samp
  
  #############################################################################################
  
}

# Some examples

# with SpatialPointsDataFrame and coordnames
# samp <- hal.point(WA.cities, 30)






