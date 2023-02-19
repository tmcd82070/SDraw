#' @title hal.point - Halton Lattice sampling for point resources
#' 
#' @description Draw a sample of point resources by 
#' allocating them to Halton boxes, separating those in the same 
#' box by at least one cycle of the Halton sequence, 
#' order them, then draw the sample.  
#' 
#' This can be used in place of 
#' BAS samples of point resource that have a large extent and small 
#' minimum distance between points.  When minimum distance between points
#' is small relative to the study area, \code{bas.point} takes forever to run
#' and in some cases fails because R cannot allocate enough memory to hold 
#' the entire sample.   
#' 
#' @param x A 'sf' or 'SpatialPoints' object containing points to sample. 
#' 
#' @param n Sample size. This is the number of points to select from \code{x}. 
#' 
#' @param J A length 2 vector containing the level of the Halton sequence 
#' to implement.  This determines the size and shape of the smallest Halton
#' boxes in the sampling scheme. 
#' 
#' @details The HAL algorithm utilizes larger 
#' Halton boxes than \code{bas.point} that potentially contain multiple points.
#' Multiple points in the same box are separated in the "halton frame" and then 
#' a sequence of contigous points is drawn.  See example in \code{halton.indices}.
#' 
#' @return A 'SpatialPoints' object containing sample points in spatially balanced order.  
#' Attributes of the sample points are: 
#' \itemize{
#'   \item \code{sampleID}: A unique identifier for every sample point.  This 
#'   encodes the spatial balance order.   If order is lost, \code{return[} \code{order(} 
#'   \code{return$sampleID} \code{),]} will re-sort the 
#'   returned object into proper order.
#'   \item \code{HaltonOrder}: The original Halton order in the frame. Generally, 
#'   users safely ignore this.
#'   \item \code{HaltonIndex}: The index in the Halton sequence that puts a 
#'   point in the box containing this point. Generally, users can safely ignore 
#'   this.
#'   \item All attributes of the original points in \code{x}. 
#' }
#'
#' 
#' @author Trent McDonald
#' @seealso \code{\link{bas.point}}
#' @keywords design survey
#' @examples
#' 
#' \dontrun{
#' hal.point( WA.cities, 100)
#' }
#' 
#' @export
hal.point <- function(x, n = nrow(x), J = c(6,4), bases = c(2,3)){
  
  # this is an implmentation of the HAL sampling method
  if( inherits(x, "sf")){
    x <- sf::as_Spatial(x, "sf")
  }
  
  # Compute Halton indices
  frame <- SDraw::halton.indices( x, J = J, bases = bases )
  
  # Construct Halton frame
  frame <- SDraw::halton.frame( frame )
  
  # Draw HAL sample
  N.frame <- nrow(frame)
  m <- floor(runif(1, 0, N.frame)) # Integer 0,...,N.frame-1
  ind <- (((0:(n-1))+m) %% N.frame ) + 1  # Cycle around frame if necessary
  samp <- frame[ind,]  # draw sample
  
  samp@data <- cbind(data.frame(sampleID = 1:n), samp@data)
  row.names(samp@data) <- NULL
  
  samp
}