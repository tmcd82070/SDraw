\name{WA.cities}
\alias{WA.cities}
\docType{data}
\title{SpatialPointsDataFrame of cities in Washington state, USA
}
\description{
A SpatialPointsDataFrame [package "sp"] containing the latitude-longitude locations of
cities in Washington state, USA.
}

\usage{data("WA.cities")}

\format{
A SpatialPointsDataFrame containing one point for 815 cities in Washington state. Source
of the Shapefile from which these cities were queried can be found at
\url{http://nationalmap.gov/small_scale/atlasftp.html}  (file 'citiesx010g.shp.tar.gz').


From metadata of the shapefile, attributes of the points are as follows:

\enumerate{
\item GNIS_ID = A unique identification number assigned by the Geographic Names
    Information System (GNIS).  This number can be used to link places in this data set
    with GNIS.
\item ANSICODE =  A unique identification number assigned by the U.S. Census Bureau.  This
    number can be used to link places in this data set with the Census Gazetteer data.
\item FEATURE = The type of feature, as assigned by GNIS. Values are 'Census', 'Civil',
    and 'Populated Place'.  
\item FEATURE2 = The status of the city or town. Values are -999 (missing), 'County Seat',
    and 'State Capital County Seat'.
\item NAME = The name of the city or town.
\item POP_2010 =  The 2010 population of the city or town.  Locations with a population of
    0 are listed as such in the Census source data.
\item COUNTY = The name of the county or county equivalent where the city or town is
    located.
\item COUNTYFIPS = The 3-digit FIPS code of the county or county equivalent.
\item STATE = The 2-character abbreviation for the State in which the city or town is
    located. Values are 'WA'.
\item STATE_FIPS = The 2-digit FIPS code for the State in which the city or town is
    located.
\item LATITUDE = The latitude of the city or town as it appears in this data set.
\item LONGITUDE = The longitude of the city or town as it appears in this data set.
\item PopPlLat = The latitude of the city or town as it appears in the source data.
\item PopPlLong = The longitude of the city or town as it appears in the source data.
\item ELEV_IN_M = The elevation, in meters, of the city or town.  Determined from GNIS or
    from topographic map sources.
\item ELEV_IN_FT = The elevation, in feet, of the city or town.  Determined from GNIS or
    from topographic map sources.
}

proj2string is "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0",
meaning among other things that the coordinates are decimal degrees of latitude and
longitude.

The rectangular bounding box of the polygon is
\tabular{lrr}{
          \tab       min  \tab      max \cr
coords.x1 \tab -124.63660 \tab -117.04326 \cr
coords.x2 \tab  45.58262  \tab 49.00012 \cr
}

}


\examples{
require(sp)
data(WA.cities)
plot(WA.cities, pch=16)
}

\keyword{datasets}
