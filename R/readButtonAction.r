readButtonAction<-function(x,dat){
  
  # Read the shape file, and display in the dialog any variables found.
  
  fn <- dat$shape.in.entry$getText()
  in.dir <- dat$shape.in.dir$getText()
  outobj <- dat$out.r.entry$getText()
  
  spframe <- getSpFrame(fn,in.dir)

  nms <- names(spframe)
  typ <- unlist(lapply(names(spframe), function(x){class(data.frame(spframe)[,x])}))
  if( length(grep("SpatialPoints", class(spframe))) > 0 ){
    ftyp <- "points"
  } else if( length(grep("SpatialLines", class(spframe))) > 0 ){
    ftyp <- "lines"
  } else if( length(grep("SpatialPolygons", class(spframe))) > 0 ){
    ftyp <- "polygons"
  } 
  
  
  # Set frame info title
  dat$finfo.title$setText(paste("Frame Contents:    \n", length(spframe), toupper(ftyp)))
  
  # Add the variable names to the dialog box, first clear any existing labels
  lapply(2:length(dat$name.labs), function(x,lablist){lablist[[x]]$hide()}, lablist=dat$name.labs)
  lapply(2:length(dat$type.labs), function(x,lablist){lablist[[x]]$hide()}, lablist=dat$type.labs)
  
  f.labtxt <- function(x,lablist,txt){
    lablist[[x+1]]$setText(txt[x])
    lablist[[x+1]]$show()
  }
  lapply(1:length(nms), f.labtxt, lablist=dat$name.labs, txt=nms )
  lapply(1:length(typ), f.labtxt, lablist=dat$type.labs, txt=typ )
  
  
}