#' @title plotLattice
#' 
#' @description Plot a Halton lattice
#' 
#' @param latt A list containing the Halton boxes in the lattice. 
#' One item in the list for each Halton box.  Each list item is a 
#' 2 X 2 matrix where first row is min and max of dimension 1, 
#' second row is min and max of dimension 2. 
#' 
#' @param ... Options of \code{graphics::lines} that control appearance of 
#' the boxes.  For example, 
#' \code{lty}, \code{col}, etc.
#' 
#' @param box The bounding box of all the halton boxes in \code{latt}. If missing
#' or NULL, the min and max extent of \code{latt} are used. 
#' 
#' @param indices The halton indices of all boxes.  \code{length(latt)} must equal
#' \code{length(indices)} and \code{indices[i]} is the index of box \code{latt[[i]]}. 
#' If missing or NULL, indices are not printed.
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
#' @return NULL is returned invisibly.
#' 
#' @details Lines on the current plot are produced.  
#' 
#' @author Trent McDonald
#' 
#'   @examples 
#' lattice <- hip.lattice.polygon( box = matrix(c(0,0,1,1),2), J = c(2,2) bases = c(2,3) )
#' plotLattice(lattice)  
#' 
#' @export
#' 
plotLattice <- function(latt, indices=NULL, J=NULL, bases=c(2,3), box=NULL, ...){
  
  drawHBox <- function(x){
    lines(x[1,],rep(x[2,1],2), ...); 
    lines(x[1,],rep(x[2,2],2), ...); 
    lines(rep(x[1,1],2),x[2,], ...); 
    lines(rep(x[1,2],2), x[2,], ...)
  }
  
  labelHBox <- function(i, x, lab){
    d1 <- mean(x[[i]][1,])
    d2 <- mean(x[[i]][2,])    
    text(d1,d2, lab[i], adj=.5, ...)
  }
  
  if(is.null(box)){
    d1 <- lapply(latt, FUN=function(x){x[1,]})
    d2 <- lapply(latt, FUN=function(x){x[2,]})
    box <- matrix(c(range(unlist(d1)), range(unlist(d2))), 2,2, byrow=TRUE)
  }

  
  plot(box[1,], box[2,], type="n", ...)
  
  tmp <- lapply(latt, FUN=drawHBox)

  if( !is.null(indices)){
    lapply(1:length(latt), FUN=labelHBox, x=latt, lab=indices)
  }
  
  if( !is.null(J) ){
    b <- bases^J
    J <- J - 1
    myCols <- rainbow(max(J))
    for( d in 1:2){
      if( J[d] >= 1 ){
        for(i in J[d]:1){
          if(d == 1){
            abline(v=(1:(b[d]-1))/(bases[d]^i), lwd=2, col=myCols[i])
          } else {
            abline(h=(1:(b[d]-1))/(bases[d]^i), lwd=2, col=myCols[i])
          }
        }
      }
    }
  }
  
  invisible(NULL)
}