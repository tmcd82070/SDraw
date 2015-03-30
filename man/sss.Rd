\name{sss}
\alias{sss}

\title{
Draw a Simple Systematic Sample (SSS) from a spatial object.
}

\description{
Draws a SSS sample from a SpatialLines or SpatialPolygons object (as defined in package 'sp').
}
\usage{
sss(n, shp, spacing=c(1,1), triangular=FALSE, rand.dir=FALSE)
}

\arguments{
  \item{n}{
Sample size.  Number of locations to draw from the spatial object. 
}
  \item{shp}{
Name of the spatial object. This must be an existing SpatialLines or 
SpatialPolygons object. The versions 
with attached data frames are also accepted (i.e., SpatialLinesDataFrame or 
SpatialPolygonsDataFrame).
}

\item{spacing}{
If \code{shp} is a SpatialPolygonsX object, this is vector of length 2 containing 
the RELATIVE spacing of grid points 
in the horizontal (X) and vertical (Y) directions.  See \code{help(sss.polygon)} for details. 
}

\item{triangular}{
If \code{shp} is a SpatialPolygonsX object, this is a boolean scaler specifying 
whether to produce 
a rectangular (\code{triangular==FALSE}) or triangular (\code{triangular==TRUE}) grid.  
See \code{help(sss.polygon)} for details.
}

\item{rand.dir}{
If \code{shp} is a SpatialPolygonsX object, this is a boolean scaler specifying 
whether to randomly orient the grid  (\code{rand.dir==TRUE}) or not 
(\code{rand.dir==FALSE}).  If 
FALSE, orientation of the grid is parallel to the X and Y axes.  If TRUE, the 
X axis of the grid is randomly rotated by an angle between -45 and 45
degrees.  Note, relative spacing of the grid cells is computed prior to rotation. 
After rotation, the X axis of the grid could be close to 90 degress, 
thereby nearly swapping the relative spacings.  See \code{help(sss.polygon)} for details.
}
}

\details{
If \code{shp} is a SpatialLinesX object, \code{sss.line} is called.
If \code{shp} is a SpatialPolygonsX object, \code{sss.polygon} is called.
}

\value{
In all cases, a SpatialPointsDataFrame containing locations in the SSS sample is returned.  
A 'siteID' attribute is attached to each 
point in the embedded data frame.  In addition, if the input object 
has an attached data frame (i.e., is a SpatialXDataFrame), the data frame of the output object contains all the attributes of 
the original data frame at the location of the sample point. 
}

\author{
Trent McDonald
}


\seealso{
\code{\link{sss.line}}, \code{\link{sss.polygon}},
}

\examples{
require(sp)


#   Sample polygons: Example draw of spatial points from 
#   the multiple polygons that make up Hawaii.  
data(NL)
NL.samp <- sss( 100, NL )
plot( NL )
points( NL.samp, pch=16 )

#   The above code issued a warning because coordinate reference
#   system (CRS) of the NL object is lat-long. Compare the above plot
#   to following, which does not issue a warning.
utm.zone <- floor((mean( bbox(NL)["x",] ) + 180)/6) + 1  # one way to figure zone
NL.utm <- spTransform( NL, CRS(paste("+proj=utm +zone=", utm.zone, " +datum=WGS84", sep="")) )
NL.samp2 <- sss( 100, NL.utm )
plot( NL.utm )
points( NL.samp2, pch=16 )

#   Sample lines: Example draw of points along coastline of Hawaii
data(HI.coast)
HI.coast.samp <- sss( 100, HI.coast )
plot(HI.coast)
points(HI.coast.samp, col="red", pch=16 )
 
}

\keyword{ design }
\keyword{ survey }
