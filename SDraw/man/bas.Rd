\name{bas}
\alias{bas}

\title{
Draw a Balanced Acceptance Sample (BAS) from a spatial object.
}

\description{
Draws a BAS sample from a SpatialPoints, SpatialLines, or SpatialPolygons object (as defined in package 'sp').
}
\usage{
bas(n, shp)
}

\arguments{
  \item{n}{
Sample size.  Number of locations to draw from the spatial object. 
}
  \item{shp}{
Name of the spatial object. This must be an existing SpatialPoints, SpatialLines, or SpatialPolygons object. The versions 
with attached data frames are also accepted (i.e., SpatialPointsDataFrame, SpatialLinesDataFrame, or SpatialPolygonsDataFrame).
}
}

\details{
If \code{shp} is a SpatialPoints object, \code{bas.point} is called.  If \code{shp} is a SpatialLines object, \code{bas.line} is called.
If \code{shp} is a SpatialPolygons object, \code{bas.polygon} is called.
}

\value{
In all cases, a SpatialPointsDataFrame containing locations in the BAS sample is returned.  A 'siteID' attribute is attached to each 
point in the embedded data frame and gives the BAS ordering of the sample.  In addition, if the input object 
has an attached data frame (i.e., is a SpatialXXDataFrame), the data frame of the output object contains all the attributes of 
the original data frame at the location of the sample point. 
}

\author{
Trent McDonald
}


\seealso{
\code{\link{bas.point}}, \code{\link{bas.line}}, \code{\link{bas.polygon}},
}

\examples{
require(sp)
data(Canadian_Province_NL)
NL.samp <- bas( 100, Canadian_Province_NL )
plot( Canadian_Province_NL )
plot( NL.samp, pch=16, add=TRUE )

#   Sample Hawaii, then count number of points in 
#   each polygons, and plot vs area of polygon.
#   N should be proportional to area.
data(HI)
HI.samp <- bas( 100, HI )
plot( HI, col=rainbow(length(HI)) )
points( HI.samp, pch=16 )

HI.poly <- data.frame(HI)
HI.counts <- data.frame(table(HI.samp$STATESP020))
names(HI.counts) <- c("STATESP020", "n")
HI.counts <- merge(HI.poly, HI.counts, all.x=TRUE)
HI.counts$n[ is.na(HI.counts$n) ] <- 0
plot( HI.counts$AREA, HI.counts$n )

}

\keyword{ design }
\keyword{ survey }
