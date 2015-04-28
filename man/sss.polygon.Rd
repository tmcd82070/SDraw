\name{sss.polygon}
\alias{sss.polygon}

\title{
Draws a Simple Systematic Sample (SSS) from an area resource (polygons).
}

\description{
Draws a systematic, or grid, sample from a SpatialPolygons object, where SpatialPolygons are defined in library 'sp'.
Options are available to control the relative spacing in horizontal and vertical directions, whether a square or triangular grid is produced, and 
whether the grid is randomly rotated. 
}

\usage{
sss.polygon(n, shp, spacing=c(1,1), triangular=FALSE, rand.dir=FALSE)
}

\arguments{
  \item{n}{
Sample size.  Number of locations to draw from the set of all polygons contained in \code{shp}. 
}
  \item{shp}{
A SpatialPolygons or SpatialPolygonsDataFrame object. This object must contain at least 1 polygon.  If 
it contains more than 1 polygon, the BAS sample is drawn from the union of all polygons.     
}
\item{spacing}{
A vector of length 2 containing the RELATIVE spacing of grid points in the horizontal (X) and vertical (Y) directions.  See details. 
}

\item{triangular}{
Boolean scaler specifying whether to produce a rectangular (\code{triangular==FALSE}) or triangular (\code{triangular==TRUE}) grid. 
}

\item{rand.dir}{
Boolean scaler specifying whether to randomly orient the grid direction (\code{rand.dir==TRUE}) or not (\code{rand.dir==FALSE}).  If 
FALSE, orientation of the grid is parallel to the X and Y axes.  If TRUE, the X axis of the grid is randomly rotated by an angle between -45 and 45
degrees.  Note, relative spacing of the grid cells is computed prior to rotation. After rotation, the X axis of the grid could be close to 90 degress, 
thereby nearly swapping the relative spacings.  
}
 

}

\details{
The projection system of the input shape object (\code{shp}) is checked.  If the projection system is 
anything other than UTM (e.g., "longlat"), \code{shp} is converted to UTM prior to constructing the systematic grid.  
UTM coordinate systems are necessary to obtain correct spacings (in UTM space).  
At the end, if the orginal projection system was not UTM, the sample grid is projected back to the original.  
This will make the output grid lines non-parrallel in the orginal system.  To get a 
proper grid, with consistent spacing, in the orginal projection, you must start with UTM's.  The author STRONGLY recommends 
converting \code{shp} to a UTM coordinate system prior to calling this function.   

Absolute spacing (size of grid cells) is determined by \code{n}. By default, grid spacing is equal in X and Y directions, which produces square grid cells. 
By default, grid spacing is \code{delta} (= \code{sqrt(A/n)}, where \code{A} = area of union of all polygons in \code{shp}. 

Relative spacing is controled by the input \code{spacing} vector.  If \code{spacing = c(rx, ry)}, spacing in X and Y directions
is \code{spacing*delta/rev(spacing)}. This assures area of each cell is \code{delta^2} while maintaining relative size specified. 

In general, realized sample size is not fixed.  Across multiple calls, realized sample size will not always equal \code{n}.  
}
\value{
A SpatialPointsDataFrame containing locations in the SSS sample, in arbitrary order.  If the input object 
has an attached data frame (i.e., is a SpatialPolygonsDataFrame), attrributes of the polygon in which each point fell 
is attached in the associated data frame.
}


\author{
Trent McDonald}


\seealso{
\code{\link{bas.polygon}}
}

\examples{
require(sp)
data(NL)

# A square grid oriented east-west
NL.samp <- sss.polygon( 100, NL )   
plot( NL )
points( NL.samp )

# A rectangular grid oriented east-west, with relative spacing c(0.667, 1.5)
NL.samp <- sss.polygon( 100, NL, spacing=c(2,3) )   
plot( NL )
points( NL.samp )

# A rectangular grid oriented east-west, with x spacing = 2**(y spacing). 
NL.samp <- sss.polygon( 100, NL, spacing=c(sqrt(2),1) )   
plot( NL )
points( NL.samp )

# A rectangular grid, random orientation, with y spacing = 3*(x spacing)
NL.samp <- sss.polygon( 100, NL, spacing=c(1,sqrt(3)), rand.dir=TRUE )   
plot( NL )
points( NL.samp )

# A triangular grid oriented east-west
NL.samp <- sss.polygon( 100, NL, triangular=TRUE )   
plot( NL )
points( NL.samp )

# A triangular grid oriented east-west, with relative spacing c(.667,1.5)
NL.samp <- sss.polygon( 100, NL, spacing=c(2,3), triangular=TRUE )   
plot( NL )
points( NL.samp )

}


\keyword{ design }
\keyword{ survey }
