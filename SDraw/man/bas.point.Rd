\name{bas.point}
\alias{bas.point}

\title{
Draws a Balanced Asseptance Sample (BAS) from a discrete resource (points).
}

\description{
Draws a BAS sample from a SpatialPoints object, where SpatialPoints are defined in library 'sp'.
}

\usage{
bas.point(n, shp)
}

\arguments{
  \item{n}{
Sample size.  Number of points to select from the set of points contained in \code{shp}. 
}
  \item{shp}{
A SpatialPoints or SpatialPointsDataFrame object. This object must contain at least 1 point.  
}
}

\details{
The BAS method for points computs the minimum distance between all points 
in \code{shp} and places a small square (pixel) 
around each.  Size of the square around each point is d/sqrt(2) on a side, where d is the 
minimum distance between points. The BAS method for points then selects 
a BAS sample from the set of polygons (i.e., squares) surrounding each point (see \code{\link{bas.polygon}}). 
The BAS method of polygons selects Halton points until \code{n} points 
are located inside the squares surrounding the points.  
When a square contains a Halton point, 
the official sample location is the center of the square, or the original point. 
}

\value{
A SpatialPointsDataFrame containing locations in the BAS sample, in order they are to be visited.  A 'siteID' attribute is attached to each 
point (in the embedded data frame) and gives the BAS ordering of the sample (i.e., sort on 'siteID' to get proper BAS order).  In addition, if the input object 
has an attached data frame (i.e., is a SpatialPointsDataFrame), the attrributes of the point are 
attached in the associated data frame. 
}

\author{
Trent McDonald
}


\seealso{
\code{\link{bas}}
}

\examples{
## bas.point( 100, Luck_Ash)
}


\keyword{ design }
\keyword{ survey }
