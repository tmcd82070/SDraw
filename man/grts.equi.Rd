\name{grts.equi}
\alias{grts.equi}

\title{
 Draw a equi-probable GRTS sample from and area (polygon), linear (line), or 
 discrete (point) resource.
}
\description{
This is a wrapper for the \code{grts} function in package \code{spsurvey}.  This function 
makes it easier to draw an equi-probable Generalized Random Tesselation Stratified (GRTS) sample 
from the types of resources supported by \code{spsurvey} 
}
\usage{
grts.equi(n,over.n,shp)
}

\arguments{
  \item{n}{
    Sample size.  The number of 'sample' points to draw from \code{shp}
}
  \item{over.n}{
    Over-sample size.  The number of 'over-sample' points to draw from \code{shp}. 
    The actual number of points drawn from \code{shp} is \code{n + over.n}.  
}
  \item{shp}{
A SpatialPoints, SpatialPointsDataFrame, SpatialLines, SpatialLinesDataFrame, SpatialPolygons, or SpatialPolygonsDataFrame object.  
}
}
\details{
This routine draws equi-probable, unstratified, GRTS samples.  
}

\value{
A SpatialPointsDataFrame containing locations in the GRTS sample, in order they are to be visited.  A 'siteID' attribute is attached to each 
point (in the embedded data frame) and gives the GRTS ordering of the sample (i.e., sort on 'siteID' to get proper GRTS order).  In addition, if the input object 
has an attached data frame (i.e., is a SpatialXDataFrame), the attributes of the spatial objects in which or on which the GRTS points fall 
is attached in the associated data frame. 
}

\references{
Stevens, D. L. and A. R. Olsen (1999). Spatially restricted surveys over time for aquatic resources. Journal of Agricultural, Biological, and Environmental Statistics 4 (4), 415-428.

Stevens, D. L. and A. R. Olsen (2004). Spatially balanced sampling of natural resources. Journal of the American Statistical Association 99, 262-278.
}

\author{
Trent McDonald
}

\seealso{
\code{\link{equi.GUI}}, \code{\link{grts.strat}}, \code{\link{bas}}
}

\examples{
data(NL)

#   Draw sample
NL_sample <- grts.equi(100,100, NL)

#   Plot
require(sp)
plot( NL )

# Plot 'sample' locations
plot( NL_sample[ NL_sample$pointType == "Sample", ], pch=16, add=TRUE )  
# Plot 'over sample' locations
plot( NL_sample[ NL_sample$pointType == "OverSample", ], pch=1, add=TRUE )  


}


\keyword{ design }
\keyword{ survey }
