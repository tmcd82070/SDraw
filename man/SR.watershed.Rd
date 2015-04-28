\name{SR.watershed}
\alias{SR.watershed}
\docType{data}
\title{Polygon map of watersheds on Santa Rosa Island, USA 
}
\description{
A SpatialPolygonsDataFrame [see package "sp"] containing outlines of the watersheds on 
Santa Rosa Island, USA.
}

\usage{data("SR.watershed")}

\format{
A SpatialPolygonsDataFrame containing 191 polygons outlining watersheds on Santa Rosa Island, USA.  

The proj2string is +proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0", 
meaning among other things that the coordinates are UTM meters zone 10. 

The rectangular bounding box of the polygon is 
\tabular{lrr}{
    \tab    min  \tab     max \cr
x \tab 754695 \tab 779932 \cr
y \tab  3754227 \tab 3769909 \cr
}

The associated data frame is: 
\enumerate{
  \item OBJECTID : Polygon ID
  \item WatershedN : Watershed name.  Only "large" watersheds are named. 
  \item Shape_Leng : Perimeter of watershed (?)
  \item Shape_Area : Area of watershed (square meters) 
  \item size.strata : Size strata = 0 for "small" watersheds, 1 for "large" named watershed.
}

}


\examples{
require(sp)
data(SR.watershed)
plot(SR.watershed) 
}

\keyword{datasets}
