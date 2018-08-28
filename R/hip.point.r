#' @export hip.point
#' 
#' @title  Draws a Halton Iterative Partition (HIP) sample from a discrete 
#' 2-dimensional (point) resource. 
#' 
#' @description  Draws a Halton Iterative Partition (HIP) sample from a 
#' \code{SpatialPoints*} object. 
#' 
#' @details  \bold{A brief description of Halton Iterative Partition (HIP) sampling for points:} 
#' Given a set of Halton Iterative Partition parameters \code{x} (SpatialPoints* object) and \code{n} (sample size),
#' a lattice of Halton boxes is constructed iteratively over the bounding box of the input points.  
#' This results in enough Halton boxes on the bounding box to uniquely cover all points in the point resource (one box
#' for each point in the point resource). 
#' The Halton index of all boxes is computed and assigned to points that lie 
#' in each box. Finally, a random number between 0 and the largest Halton index is 
#' drawn, and the next \code{n} coordinates in the mapped real numbers are taken as 
#' the sample. 
#' 
#' @param n Target sample size.  Target number of locations to draw from the set of points
#' contained in \code{x}. If the sample size returned is less than the desired sample size,
#' increase \code{n} until the desired sample size is reached.
#' 
#' @param x A \code{SpatialPoints} or \code{SpatialPointsDataFrame} object. 
#' This object must contain at least 1 point. \code{x} is the 2-dimensional point resource
#' from which samples are taken.
#' 
#' @return A \code{SpatialPoints} containing locations in the HIP sample.
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
hip.point <- function( x, n ){
  
  ########################## Check inputs ###########################################
  
  if( !(inherits(x, "SpatialPoints"))  ) stop("Must call hal.point with a SpatialPoints* object.")
  
  bases <- c(2,3) # Fix bases to 2 and 3
  
  J <- NULL
  
  #   Check n
  if( n < 1 ){
    n <- 1
    warning("Sample size less than one has been reset to 1")
  }
  
  N <- length(x)
  
  if ( n > N ) {
    n <- N
    warning("Sample size is greater than points available to sample from. 
            n has been reset to the maximum possible points to sample from.")
  }
  
  #######################################################################################
  
  #######################################################################################
  # If lattice is not specified, set approximate number of boxes
  # to number of points in frame, so approximately one per box.
  if(is.null(J)){
    # bb <- bbox( x )
    # Make call to hip.lattice to partition the box
    box <- matrix(c(0,1,0,1), byrow = TRUE, nrow = 2)
    
    D <- nrow( box )   # number of dimensions
    
    if(D != 2) { stop("HIP point implemented for 2-dimensional objects.
                      Use HIP.line for line objects.") }
    
    delta <- apply( box, 1, diff )   # size/extent of box in each dimension
    
    # Set default values of J so Halton boxes are as close to squares as possible
    n.boxes <- rep(NA,D)  # n boxes in each dimension
    for( i in 1:D ){
      n.boxes[i] <- ((delta[i]^(D-1))/prod(delta[-i]) * N)^(1/D)
    }
    
    # compute J which gives something close to n
    J <- round( log(n.boxes)/log(bases) )
    J <- ifelse(J <= 0,1,J)  # ensure all J > 0
  }
  # While J yields less than the number of points in the space, 
  # increase J until the space is covered by at least one box each
  while ( prod(bases^J) < N ) {
    if ( bases[1]^J[1] > bases[2]^J[2] ) {
      J[2] <- J[2] + 1
    } else {
      J[1] <- J[1] + 1
    }
  }
  #######################################################################################
  
  # construct box from coordinates
  coordis <- coordinates(x)
  
  if (length(coordis[1,]) > 2) {
    stop("hip.point only implemented for 2 dimensions.")
  }
  
  # Obtain lattice in Halton order
  latt <- hip.lattice.polygon( box = box, J = J, bases = bases )
  
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






