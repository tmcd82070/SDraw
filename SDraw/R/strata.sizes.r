 strata.sizes <- function(alloc.type,n.user.defined=NULL){
		sizes <- NULL
		n.strata <- length(unique(strat.var))
		if(alloc.type=="proportional"){
		sizes <- round((as.vector(table(strat.var))/length(strat.var))*n,0)
		} else 
		if(alloc.type=="constant"){
		sizes <- rep(round(n/n.strata,0),n.strata)
		} else {
		sizes <- n.user.defined
		}
		return(sizes)
   }