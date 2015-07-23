\name{hal.line}
\alias{hal.line}

\title{
  Draws a Halton Lattice from a linear resource (line).
}

\description{
  Draws a Halton Lattice sample from a SpatialLines object, where SpatialLines 
  are defined in library 'sp'.
}

\usage{
  hal.line(n, shp, J=NULL, pt.spacing = NULL, bases=c(2,3))
}

\arguments{
  \item{n}{
    Sample size.  Number of locations to draw from the set of all lines contained in \code{shp}. 
  }
  \item{shp}{
    A SpatialLines or SpatialLinesDataFrame object. This object must contain at least 1 line.  If 
    it contains more than 1 line, the HAL sample is drawn from the union of all lines.     
  }

  \item{J}{ A 2X1 vector of base powers.  \code{J[1]} is for horizontal, \code{J[2]} for vertical dimension. \code{J} determines the size and shape of the lowest level of Halton boxes. If \code{J=NULL} (the default), \code{J} is choosen so that Halton boxes are as square as possible. }
  \item{pt.spacing}{Desired spacing of points on lines prior to sampling via HAL.  The first step in sampling is to descritize lines by placing equaly-spaced points on them, then sampling points using Halton sampling (see \code{hal.point}).  This parameter controls  spacing of points in the discretization of lines.  For example, specifying 50, and assuming \code{shp} is projected to UTM meters, means points will be placed every 50 meters along all lines in \code{shp}. \code{shp} should be projected before sampling so that \code{pt.spacing} makes sense.  If \code{pt.spacing} is not specified, 1000*\code{n} points will be placed along the lines during descretization. }
  \item{bases}{2X1 vector of Halton bases.  These must be co-prime. }
  
}

\details{
  A HAL sample is drawn from the union of all lines in \code{shp} by first discretizing lines
  with points spaced \code{pt.spacing} apart. After discretizing the lines, points 
  on the lines are sampled using the 
  HAL method for points, which uses base powers \code{J}.  (see \code{\link{hal.point}}).  
}

\value{
  A SpatialPointsDataFrame containing locations in the HAL sample, in the order they are to be visited.  A 'siteID' attribute is attached to each 
  point (in the embedded data frame) and gives the HAL ordering of the sample (i.e., sort on 'siteID' to get proper HAL order).  
  In addition, if the input object 
  has an attached data frame (i.e., is a SpatialLinessDataFrame), the attrributes of the line on which each HAL point fell 
  is attached in the associated data frame. The number of the line in \code{shp} on which each point falls also appears in the 
  attribute data frame.
}

\author{
  Trent McDonald
}


\seealso{
  \code{\link{bas}}
}

\examples{
  #   Draw sample of Hawaii coastline
  #   This takes approximately 30 seconds to run
  data(HI.coast)
  samp <- hal.line( 100, HI.coast )
  plot(HI.coast, col=rainbow(length(HI.coast)))
  points( samp, pch=16 )
  
}


\keyword{ design }
\keyword{ survey }
