#' @title Calculate Voronoi polygons for a set of points
#'  
#' @description Calculate Voronoi polygons (or tesselations) from a 
#'  \code{SpatialPoints*} object
#'  
#' @param x A \code{SpatialPoints} or \code{SpatialPointsDataFrame} object
#'  
#' @param bounding.polygon If present, this is a \code{SpatialPolygons*} object specifing the 
#' bounding polygon(s) for the Voronoi polygons.  If present, the 
#' Voronoi polygons from points in \code{x} are clipped to the outside 
#' bounding polygon of \code{bounding.polygon}.  The outside bounding polygon
#' is the union of all polygons 
#' in \code{bounding.polygon}.  If this is not present, the Voronoi polygons
#' extend to a rectangle outside the range of the input points in all directions
#' by 10 percent.    
#'    
#' @return A \code{SpatialPolygonsDataFrame} containing the Voronoi polygons
#'  (or tesselations) surrounding the points in \code{x}. Attributes of the 
#'  output polygons are: 
#'  \itemize{
#'    \item x : the horizontal coordinate of the tesselation's defining point
#'    \item y : the vertical coordinate of the tesselations's defining point
#'    \item area : area of tesselation, in units of \code{x}'s projection.
#'  }
#'  
#' @details This is a convieniece routine for the 
#' \code{deldir} function.  The hard work, computing the Voronoi polygons,
#' is done by the \code{deldir::deldir} and \code{deldir::tile.list} functions. 
#' See documentation for those functions for details of computations.
#' 
#' This function is convienient because it takes a \code{SpatialPoints*} 
#' object and returns a \code{SpatialPolygonsDataFrame} object. 
#' 
#' @examples 
#' 
#'# Triangular grid inside a set of polygons
#'WA.samp <- sss.polygon(WA,100,triangular=T) 
#'
#'# Voronoi polygons of triangular grid 
#'WA.tess <- voronoi.polygons(WA.samp)
#'
#'# Plot 
#'plot(WA)
#'plot(WA.tess, add=T, col=rainbow(length(WA.samp)))
#'plot(WA.samp, add=T, pch=16)
#'
#'# One way to measure spatial balance: 
#'# Compare varinace of Voronoi polygons to same sized 
#'# SRS sample.  
#'WA.bas <- bas.polygon(WA, 100)
#'WA.srs <- srs.polygon(WA, 100)
# WA.bas.tess <- voronoi.polygons(WA.bas)
# WA.srs.tess <- voronoi.polygons(WA.srs)
#'rel.balance <- var(WA.bas.tess$area)/var(WA.srs.tess$area)
#'
#' @export


voronoi.polygons <- function(x, bounding.polygon) {
  if( !inherits(x,"SpatialPoints") ){
    stop("Must pass a SpatialPoints* object to voronoi.polygons.")
  }
  crds = coordinates(x)
  z = deldir(crds[,1], crds[,2])
  w = tile.list(z)
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
