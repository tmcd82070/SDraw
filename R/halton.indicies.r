#'  @export halton.indicies
#'  
#'  @title Compute Halton indicies
#'  
#'  @description Compute and attach indicies of the Halton sequence to points.  Points can be
#'  an abitrary set or a Halton lattice. 
#'  
#'  @param hl Either a data frame or a \code{SpatialPoints} object, both of 
#'  which must have 
#'  additional attributes (see Details). Suitable input objects are the output of 
#'  functions \code{halton.lattice} (a data frame with attributes) and 
#'  \code{halton.lattice.polygon} (a \code{SpatialPoints} object with attributes). 
#'  
#'  If \code{hl} is a data frame, it must contain coordinates columns 
#'  named the same as 
#'  row names of the Halton bounding box attribute (i.e., 
#'  \code{dimnames(attr(hl,"hl.bbox"))[[1]]} name the coordinate columns). 
#'  That is, coordinates are \code{hl[,dimnames(bb)[[1]]]]}, where \code{bb}
#'  is the "hl.bbox" bounding box attribute of \code{hl}. 
#'  
#'  If \code{hl} is a data.frame, this function 
#'  works for dimensions >2. 
#'  
#'  @param index.name A character string giving the name of the column in 
#'  the output data frame or SpatialPoints object to contain 
#'  the Halton indicies.  This name is saved as an attribute attached to 
#'  the output object.
#'  
#'  @param use.CRT A logical values specifying whether to invert the 
#'  Halton sequence using the Chinese Remainder Theorem (CRT).  The other 
#'  method (\code{use.CRT == FALSE}) is a direct method, and is very fast, 
#'  but requires multiple huge vectors be allocated (size of vectors is 
#'  \code{prod{bases^J}}, see Details). As the number of points grows, 
#'  eventually the direct method will not be able to allocate sufficient  
#'  memory (tips: Make sure to run 64-bit R, and try increasing 
#'  memory limit with \code{memory.limiit}).   
#'  The CRT method, while much  (much) slower, does not require 
#'  as much memory, and should eventually complete a much larger problem.
#'  Patience is required if your problem is big enough to require 
#'  \code{use.CRT == TRUE}.      
#'  
#'  @details The input object \code{hl} must have the following attributes:   
#'  \itemize{
#'    \item \code{bases} = Dx1 vector of bases to use in each dimension.  
#'    \item \code{J} = DX1 vector of the exponents for bases defining the Halton boxes.
#'     The number of Halton divisions in dimension \code{i} is \code{bases[i]^J[i]}.  
#'     Total number of Halton boxes is \code{prod(bases^J)}. 
#'    \item \code{hl.bbox} = DX2 matrix containing bounding box of the full set of Halton boxes. 
#'    First column of this matrix is the lower-left coordinate (i.e., minimums in D dimensions) 
#'    of the bounding box, while the second column is the upper-right coordinate 
#'    (i.e., maximums in D dimensions) of the bounding box.
#'    For example, if \code{D} = 2, \code{hl.bbox} = \code{matrix( c(min(x),min(y),max(x),max(y)),2)} 
#'  }
#'  
#'  If, in addition, \code{hl} has attributes \code{eta} or \code{triangular}, their values  
#'  are transfered to the output object for continuity of attributes within the more general  
#'  sample frame object.
#'  
#'  @return A data frame, structured like \code{data.frame(hl)}, but with indicies of the 
#'  Halton sequence attached in a column named \code{index.name}. 
#'   
#'  @author Trent McDonald
#'   
#'  @seealso \code{\link{halton.frame}}
#'   
#'  @examples 
#'# Define the Halton lattice
#'attr(WA.cities,"J") <- c(3,2)
#'attr(WA.cities,"bases") <- c(2,3)
#'# Add tiny amount to right and top of bounding box because Halton boxes are 
#'# closed on the left and bottom.  This includes points exactly on the bounding lines.
#'attr(WA.cities,"hl.bbox") <- bbox(WA.cities) + c(0,0,1,1) 
#'
#'# Compute Halton indicies
#'frame <- halton.indicies( WA.cities )
#'
#'
halton.indicies <- function(hl, index.name="SDraw.HaltonIndex", use.CRT=FALSE){

  J <- attr(hl,"J")
  b <- attr(hl,"bases")
  bb <- attr(hl,"hl.bbox") 
  eta <- attr(hl,"eta") # not used here, but saved to transfer to attributes of output object
  triangle <- attr(hl,"triangular")   # not used here, but saved to transfer to attributes of output object
  
  if( is.null(J)) stop("Attribute 'J' of input object missing.")
  if( is.null(b)) stop("Attribute 'bases' of input object missing.")
  if( is.null(bb)) stop("Attribute 'hl.bbox' of input object missing.")

    # Number of dimensions
  D <- nrow(bb)
  
  # size/extent of box in each dimension
  delta <- apply( bb, 1, diff )   
  
  # minimum coordinate of bbox in each dimension
  ll.corner <- apply(bb, 1, min)  
  
  
  # This is number of halton boxes in each direction
  n.boxes <- b ^ J
  
  # Lattice coordinates.  After this, hl is a data.frame or NULL
  if( regexpr("SpatialPoints", class(hl)) > 0 ){
    hl.coords <- coordinates(hl)
    p4string <- CRS(proj4string(hl))  # will need this later when convert back to SpatialPoints
    if( regexpr("SpatialPointsDataFrame", class(hl)) > 0 ){
      hl <- hl@data  # extract from slot because we don't want extra copy of coordinates
    } else {
      hl <- NULL
    }
    is.points <- TRUE
  } else {
    hl.coords <- hl[,dimnames(bb)[[1]]]
    is.points <- FALSE
  }
  
  # Note, this is the hard part.  This is where we invert the Halton sequence
  # either directly or using the Chinese Remainder Theorem. 
  # In general, if using another sequence, we would be solving the Chinese remainder
  # theorem here. 
  

  if( !use.CRT ){
    hl.out <- halton.indicies.vector(hl.coords, n.boxes, D, b, delta, ll.corner)
  } else {
    hl.out <- halton.indicies.CRT(hl.coords, n.boxes, D, b, delta, ll.corner)
  }
  
  if(is.null(hl)){
    hl.out <- data.frame(halton.index=hl.out)
    names(hl.out) <- index.name
  } else {
    hl.out <- data.frame( hl, halton.index=hl.out )
    names(hl.out) <- c(names(hl), index.name)
  }
  

  if( is.points ){
    # Return a SpatialPoints* object
    hl.out <- SpatialPointsDataFrame(hl.coords, data=hl.out, proj4string = p4string)
  } 
  
  # Add attributes
  attr(hl.out,"J") <- J
  attr(hl.out,"eta") <- eta
  attr(hl.out,"bases") <- b
  attr(hl.out,"hl.bbox") <- bb
  attr(hl.out,"triangular") <- triangle
  attr(hl.out,"index.name") <- index.name
  
  
  hl.out
  
}

