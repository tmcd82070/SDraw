\name{bas.polygon}
\alias{bas.polygon}

\title{
Draws a Balanced Asseptance Sample (BAS) from an area resource (polygons).
}

\description{
Draws a BAS sample from a SpatialPolygons object, where SpatialPolygons are defined in library 'sp'.
}

\usage{
bas.polygon(n, shp)
}

\arguments{
  \item{n}{
Sample size.  Number of locations to draw from the set of all polygons contained in \code{shp}. 
}
  \item{shp}{
A SpatialPolygons or SpatialPolygonsDataFrame object. This object must contain at least 1 polygon.  If 
it contains more than 1 polygon, the BAS sample is drawn from the union of all polygons.     
}

}

\details{
A BAS sample is drawn from the union of all polygons in \code{shp} by enclosing all polygons 
in a bounding square and selecting a randomized Halton sequence of points from the bounding square. 
Points falling outside all polygons are discarded until exactly \code{n} locations are selected 
inside the polygons.  
}

\value{
A SpatialPointsDataFrame containing locations in the BAS sample, in order they are to be visited.  A 'siteID' attribute is attached to each 
point (in the embedded data frame) and gives the BAS ordering of the sample (i.e., sort on 'siteID' to get proper BAS order).  In addition, if the input object 
has an attached data frame (i.e., is a SpatialPolygonsDataFrame), the attrributes of the polygon in which each BAS point fell 
is attached in the associated data frame. 
}

\author{
Trent McDonald
}


\seealso{
\code{\link{bas}}
}

\examples{
data(NL)

#   Draw sample
#   Here, first 100 are 'sample' points.
#   Second 100 are 'over sample' points
NL_sample <- bas.polygon(200, NL)  

#   Plot
require(sp)
plot( NL )

# Plot 'sample' locations
plot( NL_sample[ NL_sample$siteID <= 100, ], pch=16, add=TRUE ) 
# Plot 'over sample' locations 
plot( NL_sample[ NL_sample$siteID >  100, ], pch=1, add=TRUE )  

}


\keyword{ design }
\keyword{ survey }
