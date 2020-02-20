#' @export voronoi.polygons
#' 
#' @title Calculate Voronoi polygons for a set of points
#'  
#' @description Calculate Voronoi polygons (or tessellations) from a 
#'  \code{SpatialPoints*} object
#'  
#' @param x A \code{SpatialPoints} or \code{SpatialPointsDataFrame} object
#'  
#' @param bounding.polygon If present, this is a \code{SpatialPolygons*} object specifying the 
#' bounding polygon(s) for the Voronoi polygons.  If present, the 
#' Voronoi polygons from points in \code{x} are clipped to the outside 
#' bounding polygon of \code{bounding.polygon}.  The outside bounding polygon
#' is the union of all polygons 
#' in \code{bounding.polygon}.  If this is not present, the Voronoi polygons
#' extend to a rectangle outside the range of the input points in all directions
#' by 10 percent.    
#'    
#' @return A \code{SpatialPolygonsDataFrame} containing the Voronoi polygons
#'  (or tessellations) surrounding the points in \code{x}. Attributes of the 
#'  output polygons are: 
#'  \itemize{
#'    \item x : the horizontal coordinate of the tessellation's defining point
#'    \item y : the vertical coordinate of the tessellation's defining point
#'    \item area : area of tessellation, in units of \code{x}'s projection.
#'  }
#'  
#' @details This is a convenience routine for the 
#' \code{deldir} function.  The hard work, computing the Voronoi polygons,
#' is done by the \code{deldir::deldir} and \code{deldir::tile.list} functions. 
#' See documentation for those functions for details of computations.
#' 
#' This function is convenient because it takes a \code{SpatialPoints*} 
#' object and returns a \code{Spatial} \code{Polygons} \code{DataFrame} object. 
#' 
#' @examples 
#' 
#'# Triangular grid inside a set of polygons
#'WA.samp <- sss.polygon(WA,100,triangular=TRUE) 
#'
#'# Voronoi polygons of triangular grid 
#'WA.tess <- voronoi.polygons(WA.samp)
#'
#'# Plot 
#'plot(WA)
#'plot(WA.tess, add=TRUE, col=rainbow(length(WA.samp)))
#'plot(WA.samp, add=TRUE, pch=16)
#'
#'# One way to measure spatial balance: 
#'# Compare variance of Voronoi polygons to same sized 
#'# SRS sample.  
#'WA.bas <- bas.polygon(WA, 100)
#'WA.srs <- srs.polygon(WA, 100)
#'WA.bas.tess <- voronoi.polygons(WA.bas)
#'WA.srs.tess <- voronoi.polygons(WA.srs)
#'rel.balance <- var(WA.bas.tess$area)/var(WA.srs.tess$area)
#'
#'# Example clipping to fixed polygon (from @paul-vdb)
#'set.seed(101)
#'pts <- SpatialPoints(cbind(runif(1000), runif(1000)))
#'smp <- pts[sample(1:length(pts), 10),]
#'bound.pts <- cbind(c(0.2503111693,  0.5215198166,  0.8074680642,
#'                     0.9312807075,  0.9047494268,  0.7750409433,
#'                     0.3033737308,  0.0000000000,  0.0321650835,
#'                     0.0321650835),
#'                   c(0.03098592, 0.14595480, 0.03688176,
#'                     0.25502784, 0.89472650, 1.00000000,
#'                     0.80334098, 0.52918441, 0.14005896,
#'                     0.14005896))
#'bounding.poly <- SpatialPolygons(list(Polygons(list(Polygon(bound.pts)), "b")), as.integer(1))
#'vor <- SDraw::voronoi.polygons(smp, bounding.poly)
#'plot(vor)
#'points(pts, pch = 20)
#'points(smp, col = "red", pch = 20, cex=2)
#'plot(bounding.poly, border="green", lwd=2, add=T)
#'
voronoi.polygons <- function(x, bounding.polygon) {
  if( !inherits(x,"SpatialPoints") ){
    stop("Must pass a SpatialPoints* object to voronoi.polygons.")
  }
  crds = coordinates(x)
  bbox = c(t(bbox(bounding.polygon)))
  z = deldir::deldir(crds[,1], crds[,2], rw = bbox)
  w = deldir::tile.list(z)
  polys = vector(mode='list', length=length(w))
  for (i in seq(along=polys)) {
    pcrds = cbind(w[[i]]$x, w[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1,])
    polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i))
  }
  SP = SpatialPolygons(polys, proj4string=CRS(proj4string(x)))
  voronoi = SpatialPolygonsDataFrame(SP, 
     data=data.frame(x=crds[,1], 
                     y=crds[,2], 
                     area=sapply(slot(SP, "polygons"), 
                                 slot, "area"),
                     row.names=sapply(slot(SP, 'polygons'),
                                 slot, "ID")))
  
  # Clip to some layer, if called for
  if(!missing(bounding.polygon)){
    # If multiple polygons in bound, get just the outside bounding polygon
    bounding.polygon <- gUnion( bounding.polygon, bounding.polygon )
    voronoi.clipped <- gIntersection( voronoi, bounding.polygon, byid=TRUE,
                                      id=row.names(voronoi))
    df <- data.frame(voronoi)
    df$area <- sapply(slot(voronoi.clipped,"polygons"), slot, "area")  # new areas
    voronoi <- SpatialPolygonsDataFrame( voronoi.clipped, df)
  }
  
  voronoi
}
