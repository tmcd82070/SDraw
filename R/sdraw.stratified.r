#' @name sdraw.stratified
#' 
#' @title Stratified sample draws from spatial objects.
#' 
#' @description Use \code{sdraw} to draw stratified samples (point locations) from \code{SpatialPoints}, \code{SpatialLines}, 
#' \code{SpatialPolygons}, and the \code{*DataFrame} varieties of each.
#' 
#' @param x A spatial object.  Methods are implemented for \code{SpatialPoints}, 
#' \code{Spatial}\code{Points} \code{DataFrame}, \code{SpatialLines}, \code{Spatial}\code{Lines}\code{DataFrame}, 
#' \code{SpatialPolygons}, and 
#' \code{Spatial} \code{Polygons}\code{DataFrame} objects. 
#' 
#' @param n Desired sample size per strata. Can be a scalar indicating constant sample size per strata, a vector with the 
#' sample size in each strata ordered the same as in \code{x}, or a data frame with columns containing strata names and 
#' sample sizes per strata.   
#' 
#' Some \code{type}'s of samples are fixed-size (see DETAILS), in which case 
#' exactly \code{n} points are returned.  Other \code{type}'s are variable-size, 
#' and this number is the expected sample size (i.e., average over many repetitions).
#' 
#' @param type Character, naming the type of sample to draw. Valid \code{type}'s are:
#' \itemize{
#' \item \code{"HAL"}  : Halton Lattice sampling (Robertson et al., (Forthcoming)) 
#' \item \code{"BAS"}  : Balanced Acceptance Sampling (Robertson et al., 2013) 
#' \item \code{"SSS"}  : Simple Systematic (grid) Sampling, with random start and orientation 
#' \item \code{"GRTS"} : Generalized Random Tessellation Stratified sampling 
#'      (Stevens and Olsen, 2004) 
#' \item \code{"SRS"}  : Simple Random Sampling 
#' }
#' 
#' @param stratified.by String indicating variable storing strata names. Must be the name of a column in \code{x}. Wll be 
#' treated as a factor with number of levels equal to the number of unique values in the column in \code{x}.
#' 
#' @param ... Optional arguments passed to underlying sample type method.
#' 
#' @details This is a generic method for types \code{SpatialPoints*}, \code{SpatialLines*}, 
#' and \code{SpatialPolygons*} objects.  
#'  
#' \code{HAL, BAS, GRTS, SRS} are fixed-size designs (return exactly \code{n} points).
#' The \code{SSS} algorithm applied to Line and Point is fixed-sized.  The \code{SSS} method
#' applied to Polygon frames is variable-sized. 
#'
#' Options which determine characteristics of each 
#' sample time are passed via \code{...}.  For example, 
#' spacing and "shape" of the grid in \code{sss.*} are controlled via
#' \code{spacing=} and \code{triangular=}, while the
#' \code{J} and \code{eta} parameters (which determine box sizes) 
#' are passed to \code{hal.*}.  See documentation for 
#' \code{hal.*}, \code{bas.*}, \code{sss.*}, \code{grts.*}, and \code{sss.*} 
#' for the full list of  
#' parameters which determine sample characteristics.  
#'  
#'
#' @return A \code{SpatialPointsDataFrame} object.  At a minimum, the data frame 
#' embedded in the \code{SpatialPoints} object contains a column named \code{siteID} which 
#' numbers the points, and \code{geometryID} which contains the ID of the
#' spatial object from which the point was drawn. 
#' If \code{x} is a \code{Spatial*DataFrame}, the return's  data
#' frame contains all attributes of \code{x} evaluated at the locations of the sample points.
#' 
#' Certain sampling routine add attributes 
#' that are pertinent to the design. For example, the \code{grts.*} routines add
#' a \code{pointType} attribute.  See documentation for the underlying sampling routine
#' to interpret extra output point attributes.  
#' 
#' @author Aidan McDonald, WEST, Inc.
#'
#' @references 
#'  Robertson, B.L., J. A. Brown,  T. L. McDonald, and P. Jaksons (2013) "BAS: 
#'  Balanced Acceptance Sampling of Natural Resources", Biometrics, v69, p. 776-784.
#'  
#'  Stevens D. L. Jr. and A. R. Olsen (2004) "Spatially Balanced Sampling of Natural Resources", 
#'  Journal of the American Statistical Association, v99, p. 262-278.
#'  
#' @seealso 
#'  \code{\link{sdraw}}


sdraw.stratified <- function(x, n, type = "BAS", stratified.by = NULL, ...){
  
  if((class(x) != "SpatialPolygonsDataFrame" | is.data.frame(n)) & is.null(stratified.by)){
    stop("stratified.by is required for SpatialPoints or SpatialLines or if n is a data frame")
  }
  
  if(!is.null(stratified.by)){
    strat.vals <- eval(parse(text = paste0("x@data$", stratified.by)))
    
    # Sort rows into strata based on entries in strat.vals
    unique.strat.vals <- unique(strat.vals)
    strata <- vector("list", length = length(unique.strat.vals))
    for(i in 1:length(unique.strat.vals)){
      for(j in 1:length(strat.vals)){
        if(strat.vals[j] == unique.strat.vals[i]){
          if(is.null(strata[[i]])){
            strata[[i]] <- x[j,]
          }else{
            strata[[i]] <- rbind(strata[[i]], x[j,])
          }
        }
      }
    }
    
    # Rearrange into the correct order if n is a data frame
    if(is.data.frame(n)){
      if(!(any(colnames(n) == "n") & any(colnames(n) == stratified.by))){
        stop(paste("n must have columns named 'n' and", stratified.by))
      }
      n.names <- eval(parse(text = paste0("n$", stratified.by)))
      
      if(length(unique.strat.vals) != nrow(n)){
        stop("Number of rows in n does not equal the number of strata")
      }
      
      new.n <- c()
      for(i in 1:nrow(n)){
        for(j in 1:length(unique.strat.vals))
        {
          if(n.names[i] == unique.strat.vals[j]){
            new.n[j] <- n$n[i]
            break
          }
        }
      }
      n <- new.n
      if(length(unique.strat.vals) != length(n)){
        stop("Error in rearranging n. Possibly strata name in n not found in spatial data frame")
      }
    }
    else{
      if(length(n) == 1){
        n <- rep(n, length(x))
      }
      else if(!(length(n) == length(x))){
        stop("Invalid input for n")
      }
    }
  }
  else{
    if(length(n) == 1){
      n <- rep(n, times = length(x))
    }
    strata <- vector("list", length = length(x))
    for(j in 1:length(x)){
      strata[[j]] <- x[j,]
    }
  }
  
  # Run sdraw function (with stratified = F) length(x) times
  raw.samples <- list()
  for(i in 1:length(strata)){
    raw.samples[[i]] <- sdraw(strata[[i]], n[i], type, stratified = F, ...)
  }
  
  # Merge the length(x) individual SpatialPointsDataFrames into 1 SpatialPointsDataFrame
  ans <- raw.samples[[1]]
  if(length(raw.samples)>1){
    for(i in 2:length(strata)){
      ans <- rbind(ans, raw.samples[[i]])
    }
  }
  ans
}