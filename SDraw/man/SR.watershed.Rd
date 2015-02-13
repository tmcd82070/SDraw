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

#don't know where to find this. GTD, 2/13/15
The proj2string is "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0", 
meaning among other things that the coordinates are decimal degrees of latitude and longitude. 

#don't know where to find this. GTD, 2/13/15
The rectangular bounding box of the polygon is 
\tabular{lrr}{
    \tab    min  \tab     max \cr
x \tab -67.81762 \tab -52.61917 \cr
y \tab 46.60950 \tab 60.37024 \cr
}
#don't know where to find this. GTD, 2/13/15
The associated data frame is: 
\tabular{rrrrrrr}{
ObjectID \tab PROV \tab NAME \tab SQKM \tab FIPS_ADMIN \tab POP1996 \tab DWELL1996 \cr
65537 \tab NF \tab Newfoundland And Labrador \tab 395344.4  \tab CA05 \tab 551850  \tab  187423 \cr
}
}


\examples{
require(sp)
data(SR.watershed)
plot(SR.watershed) 
}

\keyword{datasets}
