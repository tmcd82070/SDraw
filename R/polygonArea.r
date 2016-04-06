polygonArea <- function(x){
  # Compute area of union of all polygons, Taking account 
  # of holes.  And don't throw warnings if x is unprojected
  
  holes <- sapply(x@polygons, function(xx){sapply(xx@Polygons,slot,"hole")})
  areas <- sapply(x@polygons, function(xx){sapply(xx@Polygons,slot,"area")})

  sum((-2*unlist(holes)+1) * unlist(areas))
  
}