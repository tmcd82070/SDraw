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




proj2string is "+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0",
meaning among other things that the coordinates are UTM meters zone 10.


The rectangular bounding box of the polygon is
\tabular{lrr}{
          \tab       min  \tab      max \cr
x \tab 757124.9  \tab 779223.1 \cr
y \tab 3755898.9 \tab 3768545.2 \cr
}



From metadata of the shapefile, attributes of the points are as follows:
\enumerate{
\item OBJECTID = An internal sequence number delineating linear segments.
\item Notes = Notes, all blank
\item SHAPE_Leng =  The length of the mapped stream segment, in meters.
\item created_us = User who created the linear segment.
\item created_da = Date of creation. 
\item last_edite = User making last edit
\item last_edi_1 = Date of last edit
}

}


\examples{
require(sp)
data(SR.streams) #not sure this works
plot(SR.streams)
}

\keyword{datasets}
