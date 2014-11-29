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

#   Sample Polygons: Example draw from landmass of Newfoundland.
data(NL)
NL.samp <- bas( 100, NL )
plot( NL )
plot( NL.samp, pch=16, add=TRUE )

#   Sample polygons: Example draw of spatial points from 
#   the landmasses of Hawaii.  Count number of points in
#   each polygon, and plot vs area.
#   N should be proportional to length.
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


#   Sample lines: Example draw from coastline of Hawaii.  
#   This takes approx 10 seconds to run.
#data(HI.coast)
#HI.samp <- bas( 50, HI.coast )
#plot( HI.coast, col=rainbow(length(HI.coast)) )
#points( HI.samp, pch=16 )


#   Sample points: Example draw of cities in Washington
#   Note: spatial balence of point frames with variable density 
#   will look unbalanced when plotted in 2D.  However, consider that
#   the voids (spaces between points) do not contain any points and 
#   therefore cannot be sampled.  The selected sample is spatially 
#   balanced when you consider these voids are empty.  
#   This takes approx 10 seconds to run.
#data(WA.cities)
#WA.samp <- bas( 100, WA.cities )
#plot(WA.cities)
#points(WA.samp, col="red", pch=16 )
 
}

\keyword{ design }
\keyword{ survey }
