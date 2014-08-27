\name{BAS-package}
\alias{BAS-package}
\alias{BAS}
\alias{GRTS}
\docType{package}
\title{Selection of spatially balanced samples.

}
\description{
SDraw is a GUI interface to a set of R functions that draw 
Balanced-Acceptance-Samples (BAS) and Generalized-Random-Tesselation-Stratified (GRTS) samples. 
Input sampling frames can be points (0-D, finite), lines (1-D, infinite), or polygons (2-D, infinit), 
and can be contained in ESRI shapefiles (SHP), Google Earth files (KML), Garmin files (GPX), 
or CSV files.  Functionality to visualize the frame and sample is included.
}
\details{
\tabular{ll}{
Package: \tab BAS\cr
Type: \tab Package\cr
License: \tab GNU General Public License\cr
Dependencies: \tab RGtk2, sp, spsurvey, utils, rgdal\cr 
}
When SDraw is attached (via \code{library(SDraw)}, a new menu item appears in the (Windows) R interface.  This 
menu ('SDraw') contains items which call dialog boxes that implement the interface to functions that 
actually do the work of drawing samples.   

The functions that actually draw the samples are \code{bas.polygon} (for BAS samples) and \code{grts} (for GRTS samples).  
\code{bas.polygon} is contained in SDraw.  \code{grts} is taken straight from package \code{spsurvey}.  The main 
purpose of SDraw is to provide a simple-to-use interface to these routines.  These routines, however, could be called
directly in code if needed.  BAS samples of n-dimensional resources (up to a max of n = 99 dimensions) can be drawn. 
GRTS samples can be drawn from resources having at most 2 dimensions.  Both equal and un-equal probability samples can 
be drawn through two different dialogs available in the SDraw menu.

The general work flow for drawning an equal probability sample from a 2-dimensional resource is as follows:
\tabular{ll}{
1: \tab Open 'Equi-probable Designs' item under SDraw menu (or execute equi.GUI())\cr
2: \tab Select sampling algorithm (BAS, GRTS, or SSS)\cr
3: \tab Input desired sample size into the 'Sample Size' box\cr
4: \tab Browse for shapefile containing a 2-D polygon of study area \cr
5: \tab Hit 'Run'\cr
6: \tab (optional) Hit 'Map'\cr
7: \tab (optional) Hit 'Export'\cr
} 


}
\author{
Trent McDonald \code{tmcdonald@west-inc.com} wrote the GUI interface parts and the BAS sampling routine. 
The GRTS routine comes from package \code{spsurvey}.  All the difficult  
routines of the GUI interface are in RGtk2, while routines to handle spatial 
objects are in package \code{sp} and \code{rgdal}.   

}
\references{
Robertson, et. al.

Stevens and Olsen

The monitoring book
}


\keyword{ package }
\seealso{
\code{\link[spsurvey:spsurvey-package]{spsurvey}} ~~
}

\examples{
~~ simple examples of the most important functions ~~
}
