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