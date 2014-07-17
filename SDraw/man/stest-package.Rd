\name{SDraw-package}
\alias{SDraw-package}
\alias{SDraw}
\docType{package}
\title{
A GUI interface to assist drawing spatial samples. 
}
\description{
This package provides a GUI interface to functions that draw 
Balanced-Acceptance-Samples (BAS), Generalized-Random-Tesselation-Stratified (GRTS) samples, 
and Simple-Systematic-Samples (SSS). These functions take shapefiles containing points, lines, or polygons as input. 
}
\details{
\tabular{ll}{
Package: \tab SDraw\cr
Type: \tab Package\cr
License: \tab GNU public license\cr
Depends on: \tab \code{spsurvey} \cr
}
To use, attach SDraw with \code{library(SDraw)}.  The SDraw menu will appear in the standard R GUI interface. 
Each entry in the SDraw menu starts a corresponding dialog box.  From these dialog boxes, the samples are drawn. 
}

\author{
Trent McDonald  (tmcdonald@west-inc.com)
}

\references{
Robertson, B.L., J. A. Brown, T. L. McDonald, and P. Jaksons (in press) “BAS: Balanced Acceptance Sampling of Natural Resources”, Biometrics.

Stevens, D. L., Jr. and Olsen, A. R. (2004). "Spatially balanced sampling of natural resources." Journal of the American Statistical Association 99, 262-278.
}

\keyword{ package }

\seealso{
\code{\link[spsurvey:grts]{grts}} in package spsurvey. 
}

\examples{

}
