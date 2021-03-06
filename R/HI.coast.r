#' @title SpatialLinesDataFrame of the coastline of Hawaii, USA
#' 
#' @description A SpatialLinesDataFrame [package "sp"] containing lines 
#' outlining the coast of the Hawaiian Islands, USA.
#' 
#' @usage data("HI.coast")
#' 
#' @format
#' A SpatialLinesDataFrame containing 12 lines outlining the coastline of
#' Hawaii. The Shapefile from which this coastline was queried can be found at
#' \url{http://nationalmap.gov/small_scale/atlasftp.html}  (file 'coastll010g.shp.tar.gz'). 
#' 
#' From metadata of the shapefile, attributes of the points are as follows:
#' \enumerate{
#'       \item Coastln010 = An internal sequence number delineating lines.
#'       \item Miles =  The length of the coastline segment, in miles.
#' }
#' 
#' proj4string is \code{+proj=utm +zone=4 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0},
#' meaning among other things that the coordinates are projected (UTM's). 
#' 
#' The rectangular bounding box of the polygon is
#'  
#' \tabular{lrr}{
#' \tab       min  \tab      max \cr
#' x \tab 371155 \tab 940304.1 \cr
#' y \tab  2094278 \tab  2458600.9 \cr
#' }
#' 
#' @examples
#'plot(HI.coast)
#'
#'@name HI.coast
NULL

