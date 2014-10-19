\name{NL}
\alias{NL}
\docType{data}
\title{Polygon map of Newfoundland, Canada 
}
\description{
A SpatialPolygonsDataFrame [see package "sp"] containing an outline of the Canadian province of 
Newfoundland.
}

\usage{data("NL")}

\format{
A SpatialPolygonsDataFrame containing one polygon outlining the Canadian province of Newfoundland.  
The polygon is comprised of 383 sub-polygons (mainland and islands). 

The proj2string is "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0", 
meaning among other things that the coordinates are decimal degrees of latitude and longitude. 

The rectangular bounding box of the polygon is 
\tabular{lrr}{
    \tab    min  \tab     max \cr
x \tab -67.81762 \tab -52.61917 \cr
y \tab 46.60950 \tab 60.37024 \cr
}

The associated data frame is: 
\tabular{rrrrrrr}{
ObjectID \tab PROV \tab NAME \tab SQKM \tab FIPS_ADMIN \tab POP1996 \tab DWELL1996 \cr
65537 \tab NF \tab Newfoundland And Labrador \tab 395344.4  \tab CA05 \tab 551850  \tab  187423 \cr
}
}


\examples{
require(sp)
data(NL)
plot(NL) 
}

\keyword{datasets}
