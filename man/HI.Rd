\name{HI}
\alias{HI}
\docType{data}
\title{SpatialPolygonsDataFrame of the state of Hawaii, USA
}
\description{
A SpatialPolygonsDataFrame [package "sp"] containing state boundaries for Hawaii, USA.
}

\usage{data("HI")}

\format{
A SpatialPolygonsDataFrame containing 11 polygons whose union outlines the boundaries of the state of Hawaii. Source
of the Shapefile from which these polygons were queried can be found at
\url{http://nationalmap.gov/small_scale/atlasftp.html}  (file 'statesp020.tar.gz').


From metadata of the shapefile, attributes of the polygons are:

\enumerate{
\item  AREA = The size of the shape in square coverage units.  In the distributed file, coverage units represent square decimal degrees.
\item  PERIMETER = The perimeter of shape in coverage units.  In the distributed file, coverage units represent decimal degrees.
\item  STATESP020 =  Internal feature number.
\item  STATE = The name of the State or State equivalent.
\item  STATE_FIPS = The 2-digit FIPS code of the State or State equivalent.
\item  ORDER_ADM =  An ordinal value indicating the State's order of admission to the United States.
\item  MONTH_ADM = The month when the State was admitted to the United States.
\item  DAY_ADM = The day when the State was admitted to the United States.
\item  YEAR_ADM = The year when the State was admitted to the United States.
}

proj2string is "+proj=longlat +datum=NAD83 +ellps=GRS80 +towgs84=0,0,0",
meaning among other things that the coordinates are decimal degrees of latitude and
longitude.

The rectangular bounding box of the polygon is
\tabular{lrr}{
          \tab       min  \tab      max \cr
x \tab -160.23607 \tab -154.79858 \cr
y \tab  18.91549  \tab 22.23439 \cr

}

}


\examples{
require(sp)
data(HI)
plot(HI, col=rainbow(11))
}

\keyword{datasets}
