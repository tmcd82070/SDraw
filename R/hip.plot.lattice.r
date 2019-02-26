#' @title Plot a Halton Lattice over a polygon resource
#' @description Plots a halton lattice over a polygon resource. Primarily for demonstation of HIP sampling theory.
#' @param resource \code{SpatialPolygons} object. The resource the halton lattice will be plotted over.
#' @param bases The bases of the halton lattice. Should be equal to those used to draw the sample.
#' @param J The orders of the halton lattice. Should be equal to those used to draw the sample.
#' @param sample The HIP sample to be plotted with the lattice.
#' @export

hip.plot.lattice <- function(resource, bases=c(2,3), J=c(8,5), sample=NULL){
  # Only works for polygons right now
  box <- resource@bbox
  
  # Construct lattice
  latt <- hip.lattice.polygon( box = box, J = J, bases = bases)
  
  # Plot resource
  plot(resource, lwd=2)
  
  # Overlay lattice
  for(i in 1:length(latt)){
    rect(latt[[i]][1,1],latt[[i]][2,1],latt[[i]][1,2],latt[[i]][2,2], border=TRUE)
  }
  
  # If sample provided, overlay it too
  if(!is.null(sample)){
    points(sample, col="red")
  }
}