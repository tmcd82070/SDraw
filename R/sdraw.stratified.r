sdraw.stratified <- function(x, n, type = "BAS", ...){
  for(i in 1:length(x)){
    ans <- rbind(ans,sdraw(x[i,], n[i], type, stratified = F, ...))
  }
  ans
}