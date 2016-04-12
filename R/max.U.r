#' @export maxU
#' 
#' @title Maximum integer used in the BAS routines
#' 
#' @description A function that returns the maximum integer used 
#' to construct the random-start Halton sequences.  By redefining 
#' this function and placing in the .GlobalEnv environment, the
#' user can change the maximum integer and hence the number of 
#' possible BAS samples. 
#'
#' @details 
#' CAUTION: The following comment is intended for 
#' those who wish to simulate or study statistical
#' properties of BAS, and want to completely enumerate
#' the sample space.  Don't do this if you are 
#' actually drawing a sample. 
#' 
#' To change maxU, redefine maxU() in .GlobalEnv. 
#' For example, \code{maxU <- function() 4}.
#' There are only 25 possible 2D Halton starts in this case.
#'  Random starts are = (0,1,2,3,4) X (0,1,2,3,4).
#'  
#'  In general,
#'  \code{all.possible.starts} = \code{expand.grid(x=0:maxU(),y=0:maxU()))
#' 
#'  Number of possible BAS samples is less than or equal to
#'   \code{(maxU()+1)^2}
#'  because the first sample point is required to land in a 
#'  valid polygon. So, starts that do not land in polygon 
#'  are discarded.   

#' @return 10e7 or 100,000,000  
#' 
#' @author Trent McDonald
#' 
#' @references 
#' 
#' Robertson, B.L., J. A. Brown, T. L. McDonald, and P. Jaksons
#' (2013) "BAS: Balanced Acceptance Sampling of Natural Resources", Biometrics,
#' v69, p. 776-784.
#' 
#' @seealso \code{\link{bas.line}}, \code{\link{bas.point}}, \code{\link{bas.polygon}}
#' @examples
#' # A 2D random-start Halton sequence, length 10, bases c(2,3).
#' u <- c( floor((maxU()+1)*runif(1)), floor((maxU()+1)*runif(1)))
#' halt.pts <- halton(10,dim=2,start=u,bases=c(2,3))
#' 
#' 
maxU <- function(){
  10e7
}