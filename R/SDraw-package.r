#' @title Selection of spatially balanced samples.
#' 
#' @description SDraw provides a set of R functions that draw
#' Halton-Lattice samples (HAL),
#' Balanced-Acceptance-Samples (BAS), 
#' Generalized-Random-Tesselation-Stratified (GRTS) samples, 
#' Simple-Systematic-Samples (SSS), and 
#' Simple-Random-Samples (SRS).  The types of input 
#' frames accepted are points (0-D, finite), lines (1-D, infinite), or polygons (2-D,
#' infinite). 
#' 
#' \tabular{ll}{ Package: 
#' \tab SDraw\cr 
#' Type: \tab Package\cr 
#' License: \tab GNU General Public License\cr 
#' Dependencies: \tab sp, spsurvey, rgdal, rgeos, shiny, deldir\cr 
#' (other than standard packages)
#' } 
#' 
#' 
#' The work-horse functions are named \code{???.point}, \code{???.line}, and 
#' \code{???.polygon}, where '\code{???}' is either \code{hal}, \code{bas}, \code{grts}, 
#' \code{sss}, or \code{srs}.  For simplicity, an S4 generic, 
#' \code{sdraw}, is provided to handle all combinations of sample and frame types 
#' (see \code{\link{sdraw}}).  
#' 

#' 
#' @name SDraw-package
#' @aliases SDraw-package BAS-package HAL-package GRTS SSS-package SDraw SRS
#' @docType package
#' @author Trent McDonald \code{tmcdonald@@west-inc.com}. The GRTS routine comes from
#' package \code{spsurvey}.  
#' @seealso \code{\link{sdraw}}, \code{\link{bas.polygon}}, \code{\link{bas.line}}, \code{\link{bas.point}},
#' \code{\link{hal.polygon}}, \code{\link{hal.line}}, \code{\link{hal.point}}, 
#' \code{\link{sss.line}}, \code{\link{sss.polygon}},
#' \code{\link{grts.polygon}}, \code{\link{grts.line}}, \code{\link{grts.point}}
#' documentation for package \code{sp}.
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


