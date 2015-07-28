#' @title SpatialPolygonsDataFrame for the state of Washington, USA
#' 
#' @description A SpatialPolygonsDataFrame [package "sp"] containing polygons 
#' that comprise the state of Washington. 
#' 
#' @usage data("WA")
#' 
#' @format
#' A SpatialPolygonsDataFrame containing 50 polygons whose union outline 
#' boundaries of the state of Washington. Source of the Shapefile from which these 
#' polygons were queried is 
#' \url{http://nationalmap.gov/small_scale/atlasftp.html}  (file 'statesp020.tar.gz').
#' 
#' Attributes of the polygons are:
#' \enumerate{
#'  \item  AREA = Size of the polygon in square kilometers.  
#'  \item  PERIMETER = The perimeter of polygon in kilometers. 
#'  \item  STATESP020 = Internal feature number
#'  \item  STATE = The name of the State or State equivalent.
#'  \item  STATE_FIPS = The 2-digit FIPS code of the State or State equivalent.
#'  \item  ORDER_ADM =  An ordinal value indicating the State's order of admission to the United States.
#'  \item  MONTH_ADM = The month when the State was admitted to the United States.
#'  \item  DAY_ADM = The day when the State was admitted to the United States.
#'  \item  YEAR_ADM = The year when the State was admitted to the United States.
#'  \item  LAND_TYPE = Type of the polygon. Types are "ISLAND", "MAINLAND", "OCEAN"
#' }
#'
#' The proj4string is \code{+proj=utm +zone=10 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0}, 
#' meaning among other things that the coordinates are projected zone 10 UTM's in meters. 

#' The rectangular bounding box of all polygons is
#'  
#' \tabular{lrr}{
#' \tab       min  \tab      max \cr
#' x \tab 369439 \tab   971361.3 \cr
#' y \tab  5044642 \tab  5444677.5 \cr
#' }
#' 
#' @examples
#'data(WA)
#'plot(WA[WA$LAND_TYPE == "MAINLAND",], col="red")
#'plot(WA[WA$LAND_TYPE == "ISLAND",], col="blue",add=TRUE)
#'plot(WA[WA$LAND_TYPE == "OCEAN",], col="turquoise",add=TRUE)
#'@name WA
NULL
