\name{bas.line}
\alias{bas.line}

\title{
Draws a Balanced Asseptance Sample (BAS) from a linear resource (line).
}

\description{
Draws a BAS sample from a SpatialLines object, where SpatialLines are defined in library 'sp'.
}

\usage{
bas.line(n, shp)
}

\arguments{
  \item{n}{
Sample size.  Number of locations to draw from the set of all lines contained in \code{shp}. 
}
  \item{shp}{
A SpatialLines or SpatialLinesDataFrame object. This object must contain at least 1 line.  If 
it contains more than 1 line, the BAS sample is drawn from the union of all lines.     
}
}

\details{
A BAS sample is drawn from the union of all lines in \code{shp} by first discretizing all lines
using a dense sample of points located systematically on the lines.  After discretizing the line, points 
on the line are treated as a discrete sample universe and sampled using the 
BAS method for points (see \code{\link{bas.point}}).  The BAS method for points places a small square (pixel) 
around each point and samples the set of all squares using the BAS method for polygons (see \code{\link{bas.polygon}}). 
The BAS method of polygons selects Halton points until \code{n} points 
are located inside the squares that surround points that are located on the lines.  
When a square contains a Halton point, 
the official sample location is the center of the square, which falls on the line. 
}

\value{
A SpatialPointsDataFrame containing locations in the BAS sample, in order they are to be visited.  A 'siteID' attribute is attached to each 
point (in the embedded data frame) and gives the BAS ordering of the sample (i.e., sort on 'siteID' to get proper BAS order).  In addition, if the input object 
has an attached data frame (i.e., is a SpatialLinessDataFrame), the attrributes of the line on which each BAS point fell 
is attached in the associated data frame. 
}

\author{
Trent McDonald
}


\seealso{
\code{\link{bas}}
}

\examples{
## bas.line( 100, Luck_Ash)
}


\keyword{ design }
\keyword{ survey }
