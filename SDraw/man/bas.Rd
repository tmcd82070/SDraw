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
bas( 100, Canadian_Province_NL )
}

\keyword{ design }
\keyword{ survey }
