#' @export hip.polygon
#' 
#' @title  Draws a Halton Iterative Partition (HIP) sample from a continuous 
#' 2-dimensional (polygon) resource. 
#' 
#' @description  Draws a Halton Iterative Partition (HIP) sample from a 
#' \code{SpatialPoints*} object. 
#' 
#' @details  \bold{A brief description of Halton Iterative Partition (HIP) sampling for polygons:} 
#' Given a set of Halton Iterative Partition parameters \code{x} (SpatialPoints* object),  \code{n} (sample size),
#' \code{bases}, and \code{J},
#' a lattice of Halton boxes is constructed iteratively over the bounding box of the input points.  
#' This results in \code{prod(bases^J)} Halton boxes on the bounding box to cover all points in the point resource. The
#' target should be one point per box, or \code{prod(bases^J) == n}.
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
#' @param J A 2X1 vector of base powers.  \code{J[1]} is for horizontal,
#' \code{J[2]} for vertical dimension. \code{J} determines the size and shape
#' of the smallest Halton boxes. There are \code{bases[1]^J[1]} vertical columns 
#' of Halton boxes over \code{x}'s bounding box, and \code{bases[2]^J[2]} 
#' horizontal rows of Halton boxes over the bounding box, for a total 
#' of \code{prod(bases^J)} boxes.  The dimension of each box is 
#' \code{c(dx,dy)/} \code{(bases^J)}, where \code{c(dx,dy)} are the horizontal and 
#' vertical extents of \code{x}'s bounding box.  If \code{J=NULL} (the default),
#' \code{J} is chosen so that Halton boxes are as square as possible.
#' 
#' @param bases 2X1 vector of Halton bases.  These must be co-prime.
#' 
#' @return A \code{SpatialPoints*} object containing locations in the HIP sample.
#'
#' Additional attributes of the output object, beyond those which 
#' make it a \code{SpatialPoints*}, are:
#' \itemize{
#'    \item \code{frame}: Name of the input sampling frame.
#'    \item \code{frame.type}: Type of resource in sampling frame. (i.e., "polygon").
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
#' @author Michael Kleinsasser
#' 
#' @seealso \code{\link{hip.line}}, \code{\link{hip.point}}, \code{\link{SDraw}}, 
#' \code{\link{bas.point}}
#' 
#' @keywords design survey
#' @examples
#' 
#'# Draw sample of cities in the state of Washington
#'data(WA)
#'samp <- hip.polygon( WA, 100 )
#'   
#' 
hip.polygon <- function( x, n, bases=c(2,3), J = NULL ){
  
  ####################  Check inputs #################################################
  
  # Stop function if user wants 3-D sampling
  if (length(bases) > 2) {
    stop("HIP polygon sampling not implemented for dimensions greater
         than 2. ")
  }
  # Check n
  if( n < 1 ){
    n <- 1
    warning("Sample size less than one has been reset to 1")
  }
  
  if(is.null(x)) {
    stop("Need to specify spatial points data.frame.")
  } else {
    sp.df <- x # Store spatial points data.frame
  }
  
  if (length(bases) != 2) {
    stop("Bases must be 2-dimensional, i.e. c(2,3)")
  }
  
  ####################################################################################
  
  N <- n
  
  #################### If J and or Bases not given, construct sizes based on n ########
  
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
  
  #####################################################################################
  
  
  # construct box from coordinates
  box <- sp.df@bbox
  
  # size/extent of box in each dimension
  delta <- apply( box, 1, diff )   
  
  # Make call to hip.lattice to partition the box
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
  
  # Get a random start from the the set of integers 0 to B-1 i.e. k to k+n-1 mod B, where k is random
  Kstart <- sample(0:(prod(bases^J)-1), 1)
  
  
  # take a random draw from a box
  rudraw <- function(boxl) {
    xc <- runif(1, min = boxl[1,1], max = boxl[1,2])
    yc <- runif(1, min = boxl[2,1], max = boxl[2,2])
    coord <- c(xc, yc)
    return(coord) # Return X,Y coordinate of draw
  }
  
  # # Get box in latt with index Kstart from hal.ind
  # Subsets latt to the matrices we want to sample, in the correct order
  drawSet <- latt[match(Kstart, hal.indices)]
  beginbox <- drawSet[[1]]
  
  # get first point to draw
  beginpt <- rudraw(beginbox)
  beginpt <- rbind(beginpt, beginpt); rownames(beginpt) <- c("beginpt", "2")
  
  # Convert pnts to SpatialPointsDataFrame
  pspac <- SpatialPointsDataFrame(beginpt, data.frame(id=1:2))
  proj4string(pspac) <- proj4string(sp.df)
  pt.in.poly <- sp::over( x = pspac, y = sp.df )
  
  # If beginpt is not in poygon, loop until one is
  while (is.na(pt.in.poly[1,1])) {
    # Generate new first box to draw from
    Kstart <- sample(0:(prod(bases^J)-1), 1)
    # Get box in latt with index Kstart from hal.ind
    # Subsets latt to the matrices we want to sample, in the correct order
    beginbox <- latt[match(Kstart, hal.indices)]
    
    # get first point to draw
    beginpt <- rudraw(beginbox[[1]])
    beginpt <- rbind(beginpt, beginpt); rownames(beginpt) <- c("beginpt", "2")
    
    # Convert pnts to SpatialPointsDataFrame
    pspac <- SpatialPointsDataFrame(beginpt, data.frame(id=1:2))
    proj4string(pspac) <- proj4string(sp.df)
    pt.in.poly <- sp::over( x = pspac, y = sp.df )
  }
  
  # If it is, do this:
  Kvec <- Kstart:(Kstart + n - 1) %% prod(bases^J)
  
  # Subsets latt to the matrices we want to sample, in the correct order
  drawSet <- latt[match(Kvec, hal.indices)]
  
  draws <- list()
  # Add first draw back to list
  draws[[1]] <- beginpt[1,]
  ic <- 1; i <- 1; ndraw <- 1 # need = TRUE
  while(ndraw < n) {
    i <- i + 1; ic <- ic + 1
    if (ic > length(drawSet)) ic <- 1 # reset i because end of Kvec reached
    # Make a draw from Kvec at the appropriate position
    draws[[i]] <- rudraw(drawSet[[ic]])
    # If the draw is not in the poygon, move Kvec up one and repeat process
    pt <- rbind(draws[[i]], draws[[i]]); rownames(pt) <- c("pt", "2")
    pspac <- SpatialPointsDataFrame(pt, data.frame(id=1:2))
    proj4string(pspac) <- proj4string(sp.df)
    pt.in.poly <- sp::over( x = pspac, y = sp.df )
    
    while(is.na(pt.in.poly[1,1])) {
      ic <- ic + 1
      if (ic > length(drawSet)) ic <- 1 # reset i because end of Kvec reached
      # Make a draw from Kvec at the appropriate position
      draws[[i]] <- rudraw(drawSet[[ic]])
      # If the draw is not in the poygon, move Kvec up one and repeat process
      pt <- rbind(draws[[i]], draws[[i]]); rownames(pt) <- c("pt", "2")
      pspac <- SpatialPointsDataFrame(pt, data.frame(id=1:2))
      proj4string(pspac) <- proj4string(sp.df)
      pt.in.poly <- sp::over( x = pspac, y = sp.df )
    }
    ndraw <- ndraw + 1
  }
  
  draws <- data.frame(matrix(data = unlist(draws), byrow = TRUE, ncol = 2))
  # Set up all the attributes and return statements
  samp <- SpatialPoints(draws, proj4string = CRS(proj4string(x))) # Does this work?

  #############################   Returns   ######################################
  
  # Add attributes
  attr(samp, "frame") <- deparse(substitute(x))
  attr(samp, "frame.type") <- "polygon"
  attr(samp, "sample.type") <- "HIP"
  attr(samp,"J") <- J
  attr(samp,"bases") <- bases
  attr(samp,"hl.bbox") <- box
  
  samp
  
  ################################################################################
  
}
# ----- Some examples

# samp<- hip.polygon(1000, WA)
# plot(WA)
# points(samp)


