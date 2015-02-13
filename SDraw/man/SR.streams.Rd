\name{SR.streams}
\alias{SR.streams}
\docType{data}
\title{SpatialLinesDataFrame of streams on Santa Rosa Island, USA
}
\description{
A SpatialLinesDataFrame [package "sp"] containing lines indicating streams on 
Santa Rosa Island, USA.
}

\usage{data("SR.streams")}

\format{
A SpatialLinesDataFrame containing 434 lines indicating streams on Santa Rosa Island, USA. 
The Shapefile was provided by the National Park Service for SDraw testing.
}


From metadata of the shapefile, attributes of the points are as follows:
# not sure what this is. GTD, 2/13/15 (lines 21-23)
\enumerate{
\item OBJECTID = An internal sequence number delineating linear segments.
\item SHAPE_Leng =  The length of the mapped stream segment, in meters.
}

#not sure where to find this info. GTD, 2/13/15
proj2string is "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0",
meaning among other things that the coordinates are decimal degrees of latitude and
longitude.

#not sure where to find this info. GTD, 2/13/2015
The rectangular bounding box of the polygon is
\tabular{lrr}{
          \tab       min  \tab      max \cr
x \tab -160.24656 \tab -154.80638 \cr
y \tab  18.91112 \tab  22.23236 \cr
}

}


\examples{
require(sp)
data(SR.streams) #not sure this works
plot(SR.streams)
}

\keyword{datasets}
