sdraw.stratified <- function(x, n, type = "BAS", stratified.by = NULL, ...){
  
  if(!is.null(stratified.by)){
    strat.col.vals <- eval(parse(text = paste0("x@data$", stratified.by)))
  }
  print(strat.col.vals)
  
  strata <- list()
  # SORT ROWS INTO STRATA BASED ON ENTRIES IN STRAT.COL.VARS
  
  if(length(n) == 1){
    n <- rep(n, length(x))
  }
  else if(!(length(n) == length(x))){
    stop("Invalid input for n")
  }
  
  # Run sdraw function (with stratified = F) length(x) times
  raw.samples <- list()
  for(i in 1:length(x)){
    raw.samples[[i]] <- sdraw(x[i,], n[i], type, stratified = F, ...)
  }
  
  # Merge the length(x) individual SpatialPointsDataFrames into 1 SpatialPointsDataFrame
  ans <- raw.samples[[1]]
  for(i in 2:length(x)){
    ans <- rbind(ans, raw.samples[[i]])
  }
  ans
}