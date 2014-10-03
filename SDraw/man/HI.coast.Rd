\name{HI.coast}
\alias{HI.coast}
\docType{data}
\title{SpatialLinesDataFrame of the coastline of Hawaii, USA
}
\description{
A SpatialLinesDataFrame [package "sp"] containing lines outlining the coast of
the Hawaiian Islands, USA.
}

\usage{data("HI.coast")}

\format{
A SpatialLinesDataFrame containing 12 lines outlining the coastline of Hawaii. Source
of the Shapefile from which this coastline was queried can be found at
\url{http://nationalmap.gov/small_scale/atlasftp.html}  (file 'coastll010g.shp.tar.gz').


From metadata of the shapefile, attributes of the points are as follows:

\enumerate{
\item Coastln010 = An internal sequence number delineating lines.
\item Miles =  The length of the coastline segment, in miles.
}

proj2string is "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0",
meaning among other things that the coordinates are decimal degrees of latitude and
longitude.

The rectangular bounding box of the polygon is
\tabular{lrr}{
          \tab       min  \tab      max \cr
x \tab -160.24656 \tab -154.80638 \cr
y \tab  18.91112 \tab  22.23236 \cr
}

}


\examples{
require(sp)
data(HI.coast)
plot(HI.coast)
}

\keyword{datasets}
