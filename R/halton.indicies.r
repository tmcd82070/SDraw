#' @export halton.indicies
#'  
#' @title Halton indicies
#'  
#' @description Compute and attach indicies of the Halton sequence to points.  Points can be
#'  an abitrary set or a Halton lattice. 
#'  
#' @param x Either a data frame or a \code{SpatialPoints*} object.
#' Suitable input objects are the output of 
#' functions \code{halton.lattice} (a data frame) and 
#'  \code{halton.lattice.polygon} (a \code{SpatialPointsDataFrame} object). 
#'  
#'  If \code{x} is a data frame, it must either contain the names of 
#'  coordinates columns as attribute "coordnames", or coordinates must 
#'  be the first D columns of the data frame.  
#'  I.e., coordinates are either \code{x[,attr(x,"coordnames")]} or 
#'  \code{x[,1:length(bases)]}. 
#'  
#'  This function  works for dimensions >2 if \code{x} is a data.frame.  
#'  \code{SpatialPoints*} objects are not defined for D>2. 
#'  
#' @param J A DX1 vector of base powers.  \code{J} determines the size and shape
#' of the smallest Halton boxes in D space. There are \code{bases[i]^J[i]} boxes
#' over the i-th dimension of \code{x}'s bounding box.  Total number Halton boxes 
#' is \code{prod(bases^J)}.  The size of each box in the i-th dimension is 
#' \code{delta[i]/(bases[i]^J[i])}, where \code{delta[i]} is the  
#' extent of \code{x}'s bounding box along the i-th dimension.  
#' If \code{J} is NULL (the default), approximately \code{length(x)} boxes 
#' will be choosen (approx. one point per box) and boxes will be as square 
#' as possible. 
#' 
#' @param bases DX1 vector of Halton bases.  These must be co-prime.
#' 
#' @param hl.bbox  DX2 matrix containing bounding box of the full set of Halton boxes. 
#'    First column of this matrix is the lower-left coordinate (i.e., minimums) 
#'    of the bounding box.  Second column is the upper-right coordinate 
#'    (i.e., maximums) of the bounding box.
#'    For example, if \code{D} = 2, \code{hl.bbox} = 
#'    \code{matrix( c(min(x),min(y),max(x),max(y)),2)}.  If \code{hl.bbox} is 
#'    missing (the default), the bounding box of \code{x} is used, but expanded 
#'    on the top and right by 1% to include any points exactly on the top and right 
#'    boundaries. If \code{hl.bbox} is supplied, keep in mind that all point outside
#'    the box, or on the maximums (i.e., \code{hl.bbox[,2]}), will not be assigned 
#'    Halton indices.
#'    
#' @param index.name A character string giving the name of the column in 
#'  the output data frame or \code{SpatialPoints} object to contain 
#'  the Halton indicies.  This name is saved as an attribute attached to 
#'  the output object.
#'  
#' @param use.CRT A logical values specifying whether to invert the 
#'  Halton sequence using the Chinese Remainder Theorem (CRT).  The other 
#'  method (\code{use.CRT == FALSE}) is a direct method, and is very fast, 
#'  but requires multiple huge vectors be allocated (size of vectors is 
#'  \code{prod{bases^J}}, see Details). As the number of points grows, 
#'  eventually the direct method will not be able to allocate sufficient  
#'  memory (tips: Make sure to run 64-bit R, and try increasing 
#'  memory limit with \code{\link{memory.limiit}}).   
#'  The CRT method, while much  (much) slower, does not require 
#'  as much memory, and should eventually complete a much larger problem.
#'  Patience is required if your problem is big enough to require 
#'  \code{use.CRT == TRUE}.      
#'  
#'  
#' @return If \code{x} is a data frame, \code{x} is returned
#' with an addition column.  The additional column is named \code{index.name} 
#' and stores the index of the Halton box containing the point represented 
#' on that line of \code{x}.  If \code{x} is a \code{SpatialPoints*} object, 
#' a \code{SpatialPointsDataFrame} is returned containing the points in \code{x}. 
#' The attributes of the returned object have an additional column, the index of the Halton 
#' box containing the point. Name of the attribute is \code{index.name}. 
#' If multiple points fall in the same Halton box, their Halton indicies are 
#' identical. 
#'   
#' @author Trent McDonald
#'   
#' @seealso \code{\link{halton.frame}}, \code{\link{hal.point}}
#'   
#' @examples 
#'# The following is equivalent to hal.point(WA.cities,25,J=c(3,2))
#'#
#'# Add tiny amount to right and top of bounding box because Halton boxes are 
#'# closed on the left and bottom.  This includes points exactly on the bounding lines.
#'bb <- bbox(WA.cities) + c(0,0,1,1) 
#'
#'# Compute Halton indicies
#'frame <- halton.indicies( WA.cities, J=c(3,2), hl.box=bb  )
#'
#'# Construct Halton frame
#'frame <- halton.frame( frame )
#'
#'# Draw HAL sample
#'N.frame <- nrow(frame)
#'m <- floor(runif(1, 0, N.frame)) # Integer 0,...,N.frame-1
#'ind <- (((0:(n-1))+m) %% N.frame ) + 1  # Cycle around frame if necessary
#'samp <- frame[ind,]  # draw sample
#'
#'
halton.indicies <- function(x, J=NULL, hl.bbox, bases=c(2,3),  
                            index.name="HaltonIndex", use.CRT=FALSE){

  D <- length(bases)

  if(missing(hl.bbox)){
    if( inherits(x, "SpatialPoints")){
      hl.bbox <- bbox(x)
    } else if( !is.null(cnames<-attr(x,"coordnames")) ){
      hl.bbox <- t(apply( x[,cnames], 2, range, na.rm=TRUE ))
    } else {
      hl.bbox <- t(apply( x[,1:D], 2, range, na.rm=TRUE ))
    }
    # Add a bit to top and right
    hl.bbox[,2] <- hl.bbox[,2] + 0.01*apply(hl.bbox,1,diff) 
  }
  
  # size/extent of box in each dimension
  delta <- apply( hl.bbox, 1, diff )   
  
  # minimum coordinate of bbox in each dimension
  ll.corner <- apply(hl.bbox, 1, min)  
  
  if(is.null(J)){
    # Compute J to produce approx N square boxes 
    N.boxes <- nrow(x) 
    
    # Set default values of J so Halton boxes are as close to squares as possible
    n.boxes <- rep(NA,D)  # n boxes in each dimension
    for( i in 1:D ){
      n.boxes[i] <- ((delta[i]^(D-1))/prod(delta[-i]) * N.boxes)^(1/D)
    }
    
    
    # compute J which gives something close to n
    J <- round( log(n.boxes)/log(bases) )
    J <- ifelse(J <= 0,1,J)  # ensure all J > 0
    n.boxes <- bases^J
    
  } else {
    J <- ifelse(J <= 0,1,J)  # ensure all J > 0
    n.boxes <- bases^J
  }
  
  # Lattice coordinates.  After this, x is a data.frame or NULL
  if( inherits(x, "SpatialPoints") ){
    x.coords <- coordinates(x)
    p4string <- CRS(proj4string(x))  # will need this later when convert back to SpatialPoints
    if( inherits(x, "SpatialPointsDataFrame") ){
      x <- x@data  # extract from slot because we don't want extra copy of coordinates
    } else {
      x <- NULL  # re-assign to x to save space.  don't need x again.
    }
    is.points <- TRUE
  } else {
    if(!is.null(cnames<-attr(x,"coordnames"))){
      x.coords <- x[,cnames]
    } else {
      x.coords <- x[,1:D]
    }
    is.points <- FALSE
  }
  
  # Note, this is the hard part.  This is where we invert the Halton sequence
  # either directly or using the Chinese Remainder Theorem. 


  if( !use.CRT ){
    x.out <- halton.indicies.vector(x.coords, n.boxes, D, bases, delta, ll.corner)
  } else {
    x.out <- halton.indicies.CRT(x.coords, n.boxes, D, bases, delta, ll.corner)
  }
  
  if(is.null(x)){
    x.out <- data.frame(halton.index=x.out)
  } else {
    x.out <- data.frame( halton.index=x.out, x )
  }
  names(x.out)[ names(x.out) == "halton.index" ] <- index.name
  

  if( is.points ){
    # Return a SpatialPoints* object
    x.out <- SpatialPointsDataFrame(x.coords, data=x.out, proj4string = p4string)
  } 
  
  # Add attributes
  attr(x.out,"J") <- J
  attr(x.out,"bases") <- bases
  attr(x.out,"hl.bbox") <- hl.bbox
  attr(x.out,"index.name") <- index.name

  x.out
  
}

# tmp <- coordinates(WA.cities)
# tmp <- as.data.frame(cbind(runif(nrow(tmp)), tmp))
# attr(tmp, "coordnames")<-coordnames(WA.cities)
# 
#   tmp1 <- halton.indicies(as.data.frame(tmp))
#   tmp2 <- halton.indicies(tmp)
#   tmp3 <- halton.indicies(tmp,use.CRT = T)
# 
# tmp2 <- halton.indicies(WA.cities)
# print(class(tmp2))
# print(tmp2[1:10,])