readButtonAction<-function(x,dat){
  
  # Read the shape file, and display in the dialog any variables found.
  
  fn <- dat$shape.in.entry$getText()
  in.dir <- dat$shape.in.dir$getText()
  outobj <- dat$out.r.entry$getText()
  
  spframe <- getSpFrame(fn,in.dir)
  
  print(names(spframe))
  
}