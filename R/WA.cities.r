#' @title SpatialPointsDataFrame of cities in Washington state, USA
#' 
#' @description A SpatialPointsDataFrame [package "sp"] containing the locations of
#' cities in Washington state, USA.
#' 
#' @usage data("WA.cities")
#' 
#' @format
#' A SpatialPointsDataFrame containing one point for each of 815 cities in Washington state. Source
#' of the Shapefile from which these cities were queried is
#' \url{http://nationalmap.gov/small_scale/atlasftp.html}  (file 'citiesx010g.shp.tar.gz'). 
#' 
#' The attributes of each point are:
#' \enumerate{
#'\item GNIS_ID = A unique identification number assigned by the Geographic Names
#'Information System (GNIS).  This number can be used to link places in this data set
#'with GNIS.
#'\item ANSICODE =  A unique identification number assigned by the U.S. Census Bureau.  This
#'number can be used to link places in this data set with the Census Gazetteer data.
#'\item FEATURE = The type of feature, as assigned by GNIS. Values are 'Census', 'Civil',
#'and 'Populated Place'.  
#'\item FEATURE2 = The status of the city or town. Values are -999 (missing), 'County Seat',
#'and 'State Capital County Seat'.
#'\item NAME = The name of the city or town.
#'\item POP_2010 =  The 2010 population of the city or town.  Locations with a population of
#'0 are listed as such in the Census source data.
#'\item COUNTY = The name of the county or county equivalent where the city or town is
#'located.
#'\item COUNTYFIPS = The 3-digit FIPS code of the county or county equivalent.
#'\item STATE = The 2-character abbreviation for the State in which the city or town is
#'located. Values are 'WA'.
#'\item STATE_FIPS = The 2-digit FIPS code for the State in which the city or town is
#'located.
#'\item LATITUDE = The latitude of the city or town as it appears in this data set.
#'\item LONGITUDE = The longitude of the city or town as it appears in this data set.
#'\item PopPlLat = The latitude of the city or town as it appears in the source data.
#'\item PopPlLong = The longitude of the city or town as it appears in the source data.
#'\item ELEV_IN_M = The elevation, in meters, of the city or town.  Determined from GNIS or
#'from topographic map sources.
#'\item ELEV_IN_FT = The elevation, in feet, of the city or town.  Determined from GNIS or
#'from topographic map sources.
#'}

#' 
#' proj4string is \code{+proj=utm +zone=10 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0},
#' meaning among other things that the coordinates are projected zone 10 UTM's in meters.
#' 
#' The rectangular bounding box containing all points is
#'  
#' \tabular{lrr}{
#' \tab       min  \tab      max \cr
#' x \tab  377703.1  \tab 957996.8\cr
#' y \tab  5047878.6 \tab   5438319.2 \cr
#' }
#' 
#' @examples
#'max.popln <- max(WA.cities$POP_2010)
#'plot(WA.cities, pch=16, cex=5*WA.cities$POP_2010/max.popln, col="red" )
#'
#'@name WA.cities
NULL
