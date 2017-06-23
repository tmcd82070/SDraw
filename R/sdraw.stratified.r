sdraw.stratified <- function(x, n, type = "BAS", stratified.by = NULL, ...){
  
  if(class(x) != "SpatialPolygonsDataFrame" & is.null(stratified.by)){
    stop("stratified.by is required for SpatialPoints or SpatialLines")
  }
  
  if(length(n) == 1){
    n <- rep(n, length(x))
  }
  else if(!(length(n) == length(x))){
    stop("Invalid input for n")
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
  }
  else{
    for(j in 1:length(x)){
      strata <- vector("list", length = length(x))
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
  for(i in 2:length(strata)){
    ans <- rbind(ans, raw.samples[[i]])
  }
  ans
}