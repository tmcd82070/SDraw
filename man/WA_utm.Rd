\name{WA.utm}
\alias{WA.utm}
\docType{data}
\title{SpatialPolygonsDataFrame of the state of Washington, USA, in Universal Transverse Mercator (UTM) coordinates.
}
\description{
A SpatialPolygonsDataFrame [package "sp"] containing state boundaries for Washington, USA.  Boundaries in Strait of Juan de Fuca and islands are included.
}

\usage{data("WA.utm")}

\format{
A SpatialPolygonsDataFrame containing 50 polygons whose union outlines the boundaries of the state of Washington. Source
of the Shapefile from which these polygons were queried can be found at
\url{http://nationalmap.gov/small_scale/atlasftp.html}  (file 'statesp020.tar.gz').


From metadata of the shapefile, attributes of the polygons are:

\enumerate{
\item  AREA = The size of the shape in square coverage units.  In the distributed file, coverage units represent square decimal degrees.
\item  PERIMETER = The perimeter of shape in coverage units.  In the distributed file, coverage units represent decimal degrees.
\item  STATESP020 = Internal feature number
\item  STATE = The name of the State or State equivalent.
\item  STATE_FIPS = The 2-digit FIPS code of the State or State equivalent.
\item  ORDER_ADM =  An ordinal value indicating the State's order of admission to the United States.
\item  MONTH_ADM = The month when the State was admitted to the United States.
\item  DAY_ADM = The day when the State was admitted to the United States.
\item  YEAR_ADM = The year when the State was admitted to the United States.
}

proj2string is "+init=epsg:26910", which means among other things coordinates are in UTM, meters, zone 10.

The rectangular bounding box of the polygon is
\tabular{lrr}{
          \tab       min  \tab      max \cr
utmx \tab  369439 \tab 971361.3 \cr
utmy \tab 5044642 \tab 5444677.5  \cr
}

}


\examples{
data(WA.utm)
plot(WA.utm, col=1:50)
}

\keyword{datasets}
