# Merge lines function ==========================================
merge.lines <- function(x){
  # "merge" all the lines into one big matrix of coordinates with NA's between
  
  # keep in mind that x@lines[[1]] could be 2+ Lines. 
  # This unlists the Lines objects to produce one list of coordinates
  # which is different than coordinates(x)
  tmp <- lapply( unlist(x@lines), slot, "Lines")
  l.id <- sapply( x@lines, slot, "ID")
  nline <- sapply(tmp,length) # num Line objects in each Lines object
  tmp <- lapply( unlist(tmp), slot, "coords")
  
  # Construct matrix of all coordinates, with duplicate 
  # indicies and lengths between 
  # lines. 
  merged.line <- matrix(NA, length(unlist(tmp))/2, 4)
  merged.ids <- rep(NA, nrow(merged.line))
  l.id <- l.id[rep(1:length(l.id), nline)]  # reps out ID's so loop works
  strt <- 1
  strt.tt <- 1
  strt.len <- 0
  for(i in 1:length(tmp)){
    l1 <- tmp[[i]]
    l1.id <- l.id[i]
    l1.seg.lengths = cumsum(LineLength(l1,sum=FALSE))
    l1.seg.lengths = strt.len + c(0,l1.seg.lengths)
    
    end <- strt + nrow(l1) - 1
    tt <- seq(strt.tt, length=nrow(l1))
    
    merged.line[strt:end,1] <- tt
    merged.line[strt:end,2] <- l1.seg.lengths
    merged.line[strt:end,3:4]<-l1
    merged.ids[strt:end] <- l1.id
    
    strt.tt <- end + 1 #tt[length(tt)] 
    strt <- end + 1
    strt.len <- l1.seg.lengths[length(l1.seg.lengths)]
  }
  
  if( is.null(cnms <- coordnames(x))) cnms <- c("x", "y")
  dimnames(merged.line)<- list(NULL, c("t","l",cnms))
  list(geometry=merged.line, IDs=merged.ids)
}
