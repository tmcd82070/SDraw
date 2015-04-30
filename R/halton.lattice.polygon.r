halton.lattice.polygon <- function(sp.obj, N=10000, J=NULL, eta=c(1,1), triangular=FALSE, bases=c(2,3)){
  #
  # Function to construct a Halton lattice inside a SpatialPolygon object. This is a wrapper for 
  # halton.lattice, which does all the hard work. This routine can only handle D=2 dimensional space.
  #
  # Inputs:
  #   sp.obj = A SpatialPolygon object from the sp package
  #   J = 2X1 vector of base powers.  J[1] is for horizontal, J[2] for vertical dimension
  #     J determines the size and shape of the lowest level of Halton boxes. If J=NULL (the default), 
  #     J is choosen so that Halton boxes are as square as possible. 
  #   N = Approximate number of points to place in the lattice.  If J is specified, it 
  #     takes precedence.  If J is NULL, the algorithm attempts to place N points in the bounding box 
  #     using Halton boxes that are as close to square as possible.  This N is not exact, but is a target. 
  #   eta = 2X1 vector of number of points to add inside each Halton box.  e.g., if 
  #     eta = c(3,2), a small grid of 3 by 2 points is added inside each Halton box. eta[1] is for 
  #     horizontal dimension, eta[2] is for vertical dimension. 
  #   triangular = boolean, if TRUE, construct a triangular grid. If FALSE, construct rectangluar grid.
  #   bases = 2X1 vector of Halton bases.  These must be co-prime. 
  
  
  if( is.null(J)){
    # Get area of object, subtracting holes.  This requires rgeos package.
    sp.area <- gArea( sp.obj )
    
    # Bump up the N requested to account for area outside polygons, but within bbox.
    bb.area <- prod(apply(bbox(sp.obj),1,diff))
    N <- N * bb.area / sp.area
  }

  # Get the halton lattice inside the bounding box, this does all the hard work
  hl <- halton.lattice( bbox(sp.obj), N, J, eta, triangular, bases)

  
  # A nicety: copy over the names of the coordinates.
  if( is.null(coordnames(sp.obj))) {
    names(hl) <- c("easting","northing")
  } 
  
  # Convert lattice to SpatialPoints object
  hl.points <- SpatialPoints(hl, proj4string=CRS(proj4string(sp.obj)))
  
  # Do point-in-polygon selection to get just those inside polygon.
  # Take the data frame off of the sp.obj, so over returns vector, not data frame.
  # Later, re-attache attributes
  sp.obj.nodataframe <- SpatialPolygons(sp.obj@polygons, proj4string=CRS(proj4string(sp.obj)))
  pip <- over( hl.points, sp.obj.nodataframe )
  hl.points <- hl.points[!is.na(pip),]
  if( regexpr("DataFrame", class(sp.obj)) > 0 ){
    hl.df <- data.frame(sp.obj)[pip,][!is.na(pip),]
    hl.points <- SpatialPointsDataFrame(hl.points,data=hl.df)
  }
  
  # Add attributes
  attr(hl.points,"J") <- attr(hl, "J")
  attr(hl.points,"eta") <- attr(hl, "eta")
  attr(hl.points,"bases") <- attr(hl, "bases")
  attr(hl.points,"triangular") <- attr(hl, "triangular")
  attr(hl.points,"hl.bbox") <- attr(hl, "hl.bbox")  # save original bbox, because bbox(hl.points) != bbox(sp.obj)
  
  hl.points
  
}


# ---------------------------------------------
# Example of this fuctions use and plotting

# tmp <- halton.lattice.polygon( WA.utm[3,], N=200, eta=c(3,2), triangular=T )
# 
# plot(WA.utm)
# points(tmp, pch=16, cex=.5, col="red" )
# 
# tmp.J <- attr(tmp,"J")
# tmp.b <- attr(tmp,"bases")
# tmp.bb <- attr(tmp,"hl.bbox")
# 
# for(d in 1:2){
#   tmp2 <- tmp.bb[d,1] + (0:(tmp.b[d]^tmp.J[d]))*(diff(tmp.bb[d,]))/(tmp.b[d]^tmp.J[d])
#   if( d == 1){
#       abline(v=tmp2, col="blue")
#   } else{
#       abline(h=tmp2, col="blue")
#   }
# }
#
## Compute Voronoi polygons and variance of sizes
# library(dismo)
# tmp.v <- voronoi(tmp)
# plot(WA.utm)
# plot(tmp.v, add=T, col=rainbow(length(tmp.v)))
# plot(WA.utm, add=T)
# plot(tmp, pch=16, add=T)
# tmp.v.sizes <- unlist(lapply( tmp.v@polygons, function(x){x@area} ))
# cat("Standard deviation of tesselation sizes (m)\n")
# print(sd(tmp.v.sizes))