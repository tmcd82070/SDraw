\name{grts.strat}
\alias{grts.strat}

\title{
 Draw a stratified GRTS sample from and area (polygon), linear (line), or 
 discrete (point) resource.
}
\description{
This is a wrapper for the \code{grts} function in package \code{spsurvey}.  This function 
makes it easier to draw an stratified Generalized Random Tesselation Stratified (GRTS) sample 
from the types of resources supported by \code{spsurvey} 
}
\usage{
grts.strat(n,over.n,strat.var,shp)
}

\arguments{
  \item{n}{
    A vector of sample sizes, one element for each strata.  This is the number of sample points to draw from each strata defined by \code{strat.var} attribute of \code{shp}. 
}
  \item{over.n}{
    Over-sample size.  The number of 'over-sample' points to draw from each strata in \code{shp}. The same number of over-sample points are drawn from each strata.  I.e., it is not possible to draw different sized over-samples from different strata.  
    The actual number of points drawn from strata i is   \code{n[i] + over.n}.  
}
  \item{strat.var}{
    A single string naming a variable (attribute) which defines the strata.  This variable must be contained in the data attached to the spatial objects.  Usually, the data attached to spatial objects are attributes of the geometric objects contained in a shapefile which has been read into R.  For example, this variable could be "elev" which names the  elevational class associated with every point in a  shapefile.  This name is case sensitive and must match that in the  'sp' object exactly.  Constant values of this variable define the strata. The number of unique values of this variable defines the number of strata.   
}
  \item{shp}{
A SpatialPoints, SpatialPointsDataFrame, SpatialLines, SpatialLinesDataFrame, SpatialPolygons, or SpatialPolygonsDataFrame object.  
}
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
\code{\link{equi.GUI}}, \code{\link{grts.equi}}, \code{\link{bas}}
}

\examples{

# ----  An example drawing stratified sample from polygons
data(WA.utm)

#   Define strata
WA.utm$stratum <- factor( c("water","Islands","Mainland",rep("Islands",length(WA.utm)-3)))

#   Draw sample, sample sizes are in order of levels of stratum variable, 
#   here, Islands, Mainland, water. 
#   Note, spsurvey requires at least one point in each strata
WA.sample <- grts.strat(c(10,100,1),0, "stratum", WA.utm)

#   Plot
require(sp)
plot( WA.utm, col=WA.utm$stratum )

#   Plot locations
plot( WA.sample[ WA.sample$stratum == "Islands", ], pch=16, add=TRUE, col=4 )  
plot( WA.sample[ WA.sample$stratum == "Mainland", ], pch=16, add=TRUE, col=5 )  



# ----  An example drawing stratified sample of points
data(WA.cities)


#   Draw 2 points from each county
strat.names <- unique(WA.cities$COUNTY)
n.strata <- length(strat.names)
WA.sample <- grts.strat(rep(2,n.strata),0, "COUNTY", WA.cities)

#   Plot
plot(WA.cities, col=factor(WA.cities$COUNTY), pch=1)

for( i in 1:n.strata ){
  plot( WA.sample[WA.sample$stratum == strat.names[i],], col=i, 
  add=TRUE, pch=16)
}

}


\keyword{ design }
\keyword{ survey }
