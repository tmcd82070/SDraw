\name{Canadian_Province_NL}
\alias{Canadian_Province_NL}
\docType{data}
\title{Polygon map of Newfoundland 
}
\description{
A SpatialPolygonsDataFrame [package "sp"] containing an outline of the Canadian province of 
Newfoundland.
}

\usage{data("Canadian_Province_NL")}

\format{
A SpatialPolygonsDataFrame containing one polygon outlining the Canadian province of Newfoundland.  
The polygon is comprised of 383 sub-polygons (mainland and islands). 

The proj2string is "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0", 
meaning among other things that the coordinates are decimal degrees of latitude and longitude. 

The rectangular bounding box of the polygon is 
        min       max
x -67.81762 -52.61917
y  46.60950  60.37024

The associated data frame is: 
  ObjectID PROV                      NAME     SQKM FIPS_ADMIN POP1996 DWELL1996
0    65537   NF Newfoundland And Labrador 395344.4       CA05  551850    187423
}


\examples{
require(sp)
data(Canadian_Province_NL)
plot(Canadian_Province_NL) 
}

\keyword{datasets}
