#' Selection of spatially balanced samples.
#' 
#' SDraw provides a set of R functions that draw
#' Halton-Lattice samples (HAL),
#' Balanced-Acceptance-Samples (BAS), 
#' Generalized-Random-Tesselation-Stratified (GRTS) samples, 
#' and Simple-Systematic-Samples (SSS).  The input 
#' frames can contain points (0-D, finite), lines (1-D, infinite), or polygons (2-D,
#' infinite). 
#' 
#' \tabular{ll}{ Package: 
#' \tab SDraw\cr 
#' Type: \tab Package\cr 
#' License: \tab GNU General Public License\cr 
#' Dependencies: \tab sp, spsurvey, rgdal\cr 
#' } 
#' 
#' 
#' The work-horse functions are named \code{???.point}, \code{???.line}, and 
#' \code{???.polygon}, where '\code{???}' is either \code{hal}, \code{bas}, \code{grts}, or 
#' \code{sss}.  To ease implementation, this packages extends the generic 
#' \code{spsample} in package \code{sp} with methods for HAL, BAS, GRTS, and SSS.  
#' That is, the \code{type=} parameter of \code{spsample} can be one of \code{hal}, 
#' \code{bas}, \code{grts}, or \code{sss} and the appropriate function for the 
#' type of object will be dispatched. Thus, it is generally not necessary to 
#' call the \code{???.point}, \code{???.line}, or \code{???.polygon} routine 
#' directly. If \code{spsample} is used, it is generally not necessary to know 
#' which type of spatial object is being sampled. 
#' 

#' 
#' @name SDraw-package
#' @aliases SDraw-package BAS-package HAL-package GRTS SSS-package SDraw
#' @docType package
#' @author Trent McDonald \code{tmcdonald@@west-inc.com}. The GRTS routine comes from
#' package \code{spsurvey}.  
#' @seealso \code{\link{spsample}}, \code{\link{bas.polygon}}, \code{\link{bas.line}}, \code{\link{bas.point}},
#' \code{\link{hal.polygon}}, \code{\link{hal.line}}, \code{\link{hal.point}}, 
#' \code{\link{sss.line}}, \code{\link{sss.polygon}},
#' \code{\link{grts.polygon}}, \code{\link{grts.line}}, \code{\link{grts.point}}
#' documentation for package 'spsurvey'
#' 
#' @references 
#' 
#' Manly, B. F. J. and Navarro-Alberto, J. A., editors, (2015), "Introduction to Ecological Sampling", 
#' CRC Press, Boca Raton, FL. 
#' 
#' Robertson, B.L., J. A. Brown, T. L. McDonald, and P. Jaksons
#' (2013) "BAS: Balanced Acceptance Sampling of Natural Resources", Biometrics,
#' v69, p. 776-784.
#' 
#' Stevens, D. L., Jr. and A. R. Olsen (2004) "Spatially balanced sampling of
#' natural resources." Journal of the American Statistical Association 99,
#' 262-278.
#' 
#' @keywords package

"_PACKAGE"


