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
#' @author Michael Kleinsasser\cr
#' Aidan McDonald
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
#' @export
#' 

hip.point <- function( x, n, J = NULL, plot.lattice = FALSE ){
  
  ########################## Check inputs ###########################################
  #set.seed(12345)
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
  
  if(is.null(J)){J <- getJ( N, bases )}
  B <- prod(bases ^ J)
  if(B > N){
    warning("J results in more Halton boxes than there are points. J has been set to default value.")
    J <- getJ( N, bases )
    B <- prod(bases ^ J)
  }
  
  # Warn if n is more than the number of points that will be left after throwing out points
  maxSampleSize <- prod(bases ^ J)
  keepGoing <- TRUE
  multiplier <- 2
  while(keepGoing){
    if(maxSampleSize * multiplier > N){
      keepGoing <- FALSE
    }else{multiplier <- multiplier + 1}
  }
  maxSampleSize <- maxSampleSize * (multiplier-1)
  if(n > maxSampleSize){
    warning(c("Sample size is greater than max sample size for HIP sampling.
  HIP sampling discards some points while drawing Halton lattice.
  n has been set to ", maxSampleSize, ", the largest possible sample for N = ", N, " and J = (", J[1], ",", J[2], ")"))
    n <- maxSampleSize
  }
  
  ## ---- Before the partition, construct indices of boxes to be sampled
  kStart <- trunc( runif(1, 0, B) )
  Sk <- (kStart:(kStart+n-1)) %% B
  box <- bbox(x) # Bounding box
  
  # Generate list of coordinate points from x
  pointsInbbox <- list()
  coords <- coordinates(x)
  for(l in 1:nrow(coords)){pointsInbbox[[l]] <- coords[l,]}
  
  # Define function to check if a point is in a box
  in.box <- function(point, box){
    point[1]>=box[1,1] && point[1]<=box[1,2] && point[2]>=box[2,1] && point[2]<=box[2,2]
  }
  
  if(plot.lattice == TRUE){
    # Plot resource
    plot(x, pch = 1)
    # Define function to plot boxes
    plot.box <- function(box){rect(box[1,1], box[2,1], box[1,2], box[2,2])}
  }
  
  # Initial matrix of coordinates for points in final sample
  samp.coords <- matrix(nrow = n, ncol = length(bases))
  splitsPerformed <- list()
  #browser()
  
  for(k in 1:n){ # For each point we are sampling
    
    #browser()
    #print(k)
    currentBox <- box # Start with bounding box
    if(plot.lattice){plot.box(currentBox)}
    pointsInCurrentBox <- pointsInbbox # Start with all points that haven't been discarded
    # Get halton coefficients (path to box)
    haltonCoeffs <- halton.coefficients(Sk[k], J, bases)
    haltonCoeffsSoFar <- c()
    
    # Partition box
    for(j in 1:max(J)){ # For each order of partitioning
      for(i in 1:length(bases)){ # For each dimension
        if(j<=J[i]){ # Partition only if this dimension needs another
          # Check if we have already done this split
          newboxlower <- NA
          newboxupper <- NA
          newSplit <- TRUE
          for(oldsplit in splitsPerformed){
            if(length(haltonCoeffsSoFar) == (length(oldsplit[[1]])-1) && all(haltonCoeffsSoFar == oldsplit[[1]][-length(oldsplit[[1]])])){ # If the path the box of the current split is the same as the path to the box of an old split
              newSplit <- FALSE
              newboxlower <- oldsplit[[2]][haltonCoeffs[,,i][j]+1]
              newboxupper <- oldsplit[[2]][haltonCoeffs[,,i][j]+2]
            }
          }
          
          # If needed, remove points from resource until we can make an even split
          if(newSplit){
            pointsToRemove <- sample(1:length(pointsInCurrentBox), length(pointsInCurrentBox)%%bases[i])
            pointsToRemoveFromPointsInbbox <- c()
            for(l in pointsToRemove){ # This is horrendously inefficient, is there a better way?
              if(plot.lattice){points(pointsInCurrentBox[[l]][1], pointsInCurrentBox[[l]][2], pch=4)}
              for(m in 1:length(pointsInbbox)){
                if(all(pointsInbbox[[m]] == pointsInCurrentBox[[l]])){
                  pointsToRemoveFromPointsInbbox <- c(pointsToRemoveFromPointsInbbox, m)
                }
              }
            }
            
            if(length(pointsToRemoveFromPointsInbbox>0)){pointsInbbox <- pointsInbbox[-pointsToRemoveFromPointsInbbox]}
            if(length(pointsToRemove>0)){pointsInCurrentBox <- pointsInCurrentBox[-pointsToRemove]}
            #for(m in pointsToRemoveFromPointsInbbox){pointsInbbox[[m]] <- NULL}
            #for(l in pointsToRemove){pointsInCurrentBox[[l]] <- NULL}
          }
          
          # Define newbox based on haltonCoeffs
          # get vector of relevant coordinates of points
          coordsOfPointsToSplit <- c()
          for(l in 1:length(pointsInCurrentBox)){
            coordsOfPointsToSplit <- c(coordsOfPointsToSplit, pointsInCurrentBox[[l]][i])
          }
          
          # Find boundary of newbox
          if(is.na(newboxlower)){
            if(haltonCoeffs[,,i][j] == 0){
              newboxlower <- currentBox[i,1]
            }else{
              newboxlower <- quantile(coordsOfPointsToSplit, haltonCoeffs[,,i][j]/bases[i])
            }
          }
          if(is.na(newboxupper)){
            if((haltonCoeffs[,,i][j]+1) == bases[i]){
              newboxupper <- currentBox[i,2]
            }else{
              newboxupper <- quantile(coordsOfPointsToSplit, (haltonCoeffs[,,i][j]+1)/bases[i])
            }
          }
          newbox <- currentBox
          newbox[i,1] <- newboxlower
          newbox[i,2] <- newboxupper
          
          haltonCoeffsSoFar <- c(haltonCoeffsSoFar, haltonCoeffs[,,i][j])
          
          # Store split so we can repeat it later
          if(newSplit){
            splitValues <- c()
            splitValues[1] <- currentBox[i,1]
            for(l in 2:bases[i]){
              splitValues[l] <- quantile(coordsOfPointsToSplit, (l-1)/bases[i])
            }
            splitValues[bases[i]+1] <- currentBox[i,2]
            splitsPerformed[[length(splitsPerformed)+1]] <- list(haltonCoeffsSoFar, splitValues)
          }
          
          # Discard points not in newbox
          pointsToRemove <- c()
          for(l in 1:length(pointsInCurrentBox)){
            if(!in.box(pointsInCurrentBox[[l]],newbox)){
              pointsToRemove <- c(pointsToRemove, l)
            }
          }
          if(length(pointsToRemove>0)){pointsInCurrentBox <- pointsInCurrentBox[-pointsToRemove]}
          currentBox <- newbox
          if(plot.lattice){plot.box(currentBox)}
          #for(l in pointsToRemove){pointsInCurrentBox[[l]] <- NULL}
        }
      }
    }
    # Pick a point from the box, add to final sample
    pointIsNew <- FALSE
    while(!pointIsNew){
      candidatePoint <- pointsInCurrentBox[[sample(1:length(pointsInCurrentBox), 1)]]
      pointIsNew <- TRUE
      for(i in 1:nrow(samp.coords)){
        if(!all(is.na(samp.coords[i,])) && all(samp.coords[i,] == candidatePoint)){pointIsNew <- FALSE}
      }
    }
    if(plot.lattice){text(mean(c(currentBox[1,1],currentBox[1,2])),mean(c(currentBox[2,1],currentBox[2,2])), labels = Sk[k])}
    
    samp.coords[k,] <- candidatePoint
  }
  
  
  
  #  ## ---- Partion the points
  #  hipPartition <- hip.partition(coords, Sk, J, bases)
  #  
  ##HERE!!!
  ############## My halton index function ##################################
  #hal.ind <- function(boxn) {
  #  ## Scale incoming points to between 0 and 1
  #  ## subtract min(x,y) off of box
  #  boxn <- boxn - box[,1]
  #  ## divide by delta
  #  boxn[1,] <- boxn[1,]/delta[1]
  #    boxn[2,] <- boxn[2,]/delta[2]
  #  # Compute a_i's
  #  powers <- function(x, len, st) { x <- x ^ (st:len); x } # get vector of powers
  #  a_1 <- sum(floor( ((boxn[1,1]+0.0001) * powers(bases[1], J[1], 1) ) %% bases[1]) * (powers(bases[1], J[1]-1, 0))) # %% bases[1]^J[1]
  #  a_2 <- sum(floor( ((boxn[2,1]+0.0001) * powers(bases[2], J[2], 1) ) %% bases[2]) * (powers(bases[2], J[2]-1, 0))) # %% bases[2]^J[2]
  #  av <- c(a_1, a_2) # vector of a_i's
  #  # CRT (Chinese Remainder Theorem)
  #  N <- prod(bases^J); y <- N / (bases^J)
  ##  # Use Extended Euclidean Algorithm to get inverse of y_i mod n_i
  #  z <- extended.gcd(y, bases^J)$t # CHECK with an example !!!
  #  # for(i in 1:length(z)) { if(z[i] < 0) z[i]<-z[i]+bases[i] }
  # sol <- sum(av * y * z) %% N
  #  # return halton index for a box
  #  return(sol)
  #}
  ########################################################################
  #  
  ## Get halton indices for each box
  #hal.indices <- lapply(latt, hal.ind)
  #hal.indices <- unlist(hal.indices)
  #  
  ## If you want a plot of the Halton boxes to check:
  ## hal.indices <- hal.indices %% prod(bases^J)
  ## plotLattice(latt, indices=hal.indices, J=J)
  #  
  #########################################################################
  #### Here are the guts of HIP.point. We correctly order the points to 
  #### match the Halton sequence, then draw from halton sequense points
  #########################################################################
  #  
  ## Match hal.indices to output from hip.lattice.point
  #hal.point.grid <- hip.lattice.point(coords = data.frame(coordis), J = J, bases = bases)
  #hal.point.grid <- data.frame(matrix(unlist(hal.point.grid), nrow=length(hal.point.grid), byrow=T))
  #  
  ## construct data.frame of form: X, Y, halton.index
  #samp.frame <- data.frame(cbind(hal.point.grid, hal.indices))
  #  
  ### Now we can take samples:
  #  
  ## eliminate duplicate coordinate sets 
  #samp.frame <- samp.frame[!duplicated(samp.frame[,1:2]),]
  #  
  ## Get a random start from the the set of integers 0 to B-1 i.e. k to k+n-1 mod B, where k is random
  #Kstart <- samp.frame[sample(1:(length(samp.frame[,3])), 1), 3]
  #  
  #Kvec <- Kstart:(Kstart + n - 1) %% prod(bases^J)
  #Kvec <- Kvec[which(Kvec %in% samp.frame[,3])]
  #  
  ## Subsets latt to the matrices we want to sample, in the correct order
  #drawSet <- samp.frame[match(Kvec, samp.frame[,3]),]
  #drawSet <- drawSet[complete.cases(drawSet),]
  #
  ## Make draws from drawSet as close to sample size needed as possible
  #samp.coords <- drawSet[,1:2]
  
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
  
  if(plot.lattice){points(samp, col='red', pch = 19)}
  samp
  
  #############################################################################################
  
}

# Some examples

# with SpatialPointsDataFrame and coordnames
# samp <- hal.point(WA.cities, 30)






