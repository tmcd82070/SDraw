readButtonAction<-function(x,dat){
  
  # Read the shape file, and display in the dialog any variables found.
  
  fn <- dat$shape.in.entry$getText()
  in.dir <- dat$shape.in.dir$getText()
  
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
  nn <- min(length(nms),length(dat$name.labs)-1)
  lapply(1:nn, f.labtxt, lablist=dat$name.labs, txt=nms )
  lapply(1:nn, f.labtxt, lablist=dat$type.labs, txt=typ )
  
  if( length(nms) > (length(dat$name.labs)-1) ){
    dat$name.labs[[nn+1]]$setText(paste("<list truncated>"))
    dat$type.labs[[nn+1]]$setText(paste("<first", nn-1, "displayed>"))
  }
  
  # Plot the object
  plotSample(x,dat)
  
}