\name{sss.line}
\alias{sss.line}

\title{
Draws a Simple Systematic Sample (SSS) from a linear resource (i.e., of a line).
}

\description{
Draws a systematic from a SpatialLines object (SpatialLines defined in library 'sp').
}

\usage{
sss.line(n, shp)
}

\arguments{
  \item{n}{
Sample size.  Number of locations to draw from the set of all lines contained in \code{shp}. 
}
  \item{shp}{
A SpatialLiness or SpatialLinessDataFrame object. This object must contain at least 1 line.  If 
it contains more than 1 line, the sample is drawn from the union of all lines.     
}
 

}

\details{
This function is simply a wrapper for \code{spsample} in package 'sp'.
}
\value{
A SpatialPointsDataFrame containing locations in the SSS sample, in arbitrary order.  
}


\author{
Trent McDonald}


\seealso{
\code{\link{sss.polygon}}, \code{\link{spsample}}
}

\examples{

data(HI.coast)
HI.samp <- sss.line( 100, HI.coast )   # square grid oriented east-west
plot( HI.coast, col=rainbow(length(HI.coast)) )
points( HI.samp, col="red", pch=16 )

}


\keyword{ design }
\keyword{ survey }
