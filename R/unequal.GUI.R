unequal.GUI <- function()   {
  #
  #   Setup and run a GUI to take a BAS sample 
  #
  
  design <- "unequal"
  
  #   ---- Define the main window
  win <- gtkWindowNew("toplevel")
  win$setBorderWidth(8) 
  win$setTitle("S-Draw : Unequal probability sample drawing interface")
  #gtkWindowSetIconFromFile(win, filename = "s-draw.ico")  # need path to be correct here, or does not work, obviously
  
  vbox1 <- gtkVBoxNew(FALSE, 8)
  vbox1$setBorderWidth(8)
  win$add(vbox1)
  
  # ================= Sample type frame ============================
  samp.types <- c("HAL - Halton Lattice Sampling", 
                  "BAS - Balanced Acceptance Sampling", 
                  "GRTS - Generalized Random Tessellation Stratified", 
                  "SSS - Simple Systematic Sampling")
  
  #this adds different sampling frames
  #I don't forsee adding anything other than BAS, GRTS, or SSS  -- HAL!!!
  samp.type.combo <- gtkComboBoxNewText()
  samp.type.combo$show()
  for( i in samp.types ){
    samp.type.combo$appendText( i )
  }
  samp.type.combo$setActive(0)
  
  #    print(gtkComboBoxGetActive(samp.type.combo))
  #    print(gtkComboBoxGetWrapWidth(samp.type.combo))
  
  samp.type.frame <- gtkFrameNew("Sample Type")
  samp.type.frame$setBorderWidth(8)
  #this adds a label to the combo box
  
  combo.box <- gtkHBoxNew(FALSE, 8)
  combo.box$setBorderWidth(8)
  combo.box$packStart( samp.type.combo )
  samp.type.frame$add( combo.box )
  
  hbox2 <- gtkHBoxNew(FALSE, 8)
  #hbox2$setBorderWidth(8)
  hbox2$packStart(samp.type.frame)
  
  #    logo <- gtkImageNewFromFile("s_draw_banner.png")
  #    hbox2$packStart(logo)
  
  
  vbox1$packStart(hbox2)
  
  #   Handler for change in sample type
  f.samp.type.change <- function(x,dat){
    stype <- samp.type.combo$getActive()
    
    #  Carefull, don't get the numbers out of order with the options
    if( stype == 0 ){
      # Halton samples
      over.entry$hide()
      over.size.label$hide()
    } else if( stype == 1 ){
      # BAS samples
      over.entry$hide()
      over.size.label$hide()        
    } else if( stype == 2 ){
      # GRTS samples
      over.entry$show()
      over.size.label$show()
    } else {
      # sss samples
      over.entry$hide()
      over.size.label$hide()
    }
    
    
  }
  gSignalConnect(samp.type.combo, "changed", f.samp.type.change )
  
  
  
  # ------ Optional inputs box
  opt.hbox <- gtkHBoxNew(TRUE, 2)
  opt.hbox$setBorderWidth(8)
  hbox2$packStart(opt.hbox)
  
  opt.frame <- gtkFrameNew("Optional Inputs")
  opt.hbox$packStart(opt.frame)
  
  #    opt.blank.box <- gtkHBoxNew(TRUE,2)
  #    opt.hbox$packStart(opt.blank.box)
  
  opt.vbox <- gtkVBoxNew(FALSE, 8)
  opt.vbox$setBorderWidth(8)
  opt.frame$add(opt.vbox)
  
  
  #   ---- Define table of boxes so everything aligns
  opt.tbl <- gtkTable(7,5,FALSE)
  gtkTableSetRowSpacings(opt.tbl,1)
  gtkTableSetColSpacings(opt.tbl,5)
  
  opt.vbox$add(opt.tbl)
  
  
  
  #   ---- Seed text box
  seed.entry <- gtkEntryNew()
  seed.entry$setText( "" )
  seed.label <- gtkLabel("Random number seed:")
  
  gtkTableAttach(opt.tbl,seed.label, 0, 1, 0, 1, xpadding=5, ypadding=5)
  gtkTableAttach(opt.tbl,seed.entry, 1, 2, 0, 1, xpadding=5, ypadding=5)
  
  
  #   ---- Over sample size text boxes
  over.entry <- gtkEntry()
  over.entry$setText( "0" )
  over.size.label <- gtkLabel("Over sample, total over all categories:")
  
  # Hide initially because Halton Latice is initial sample type
  over.entry$hide()
  over.size.label$hide()
   
  gtkTableAttach(opt.tbl,over.size.label, 0, 1, 1, 2, xpadding=5, ypadding=5)
  gtkTableAttach(opt.tbl,over.entry, 1, 2, 1, 2, xpadding=5, ypadding=5)
  
  
  # --------------------------- Middle horizontal box ---------------
  req.frame <- gtkFrameNew("Required Inputs")
  vbox1$packStart(req.frame)
  
  hbox1 <- gtkHBoxNew(FALSE, 8) #sets up middle horizontal box, FALSE means things not evenly spaced, 8 is for 8 pixels between things
  hbox1$setBorderWidth(8)
  req.frame$add(hbox1) #this adds the new horizontal box to the frame which is in the overall vertical box.  we are building the window vertically
  
  
  
  # ================= Required Inputs frame ============================
  frame.frame <- gtkFrameNew("Frame Information")
  hbox1$add(frame.frame)  # Adds the frame to the horizontal box
  
  #   ---- Define a verticle box
  req.vbox <- gtkVBoxNew(FALSE, 8)
  req.vbox$setBorderWidth(8)
  frame.frame$add(req.vbox)
  
  
  #   ---- Define table of boxes so everything aligns
  tbl <- gtkTable(7,4,FALSE) #3 rows, 2 columns, FALSE for nonhomogeneous
  gtkTableSetRowSpacings(tbl,1) #1 pixel between rows
  gtkTableSetColSpacings(tbl,5) #5 pixels between columns
  
  req.vbox$packStart(tbl)
  
  
  #   ---- Input shape file box
  shape.in.entry <- gtkEntry()
  shape.in.entry$setText( "" )
  shape.file.label <- gtkLabel("Shape file OR 'sp' Object:")
  
  shape.in.dir <- gtkEntry()  # this entry box is hidden/not displayed
  shape.in.dir$setText( getwd() )
  
  #out.r.entry <- gtkEntry()
  #out.r.entry$setText( "" )
  
  #   ---- Output R object box
  out.r.entry <- gtkEntry()
  out.r.entry$setText("")#paste("sdraw.", format(Sys.time(), "%Y.%m.%d.%H%M%S"), sep=""))
  out.r.label <- gtkLabel("Sample's R name:")
  
  gtkTableAttach(tbl,out.r.label, 0, 1, 3, 4, xpadding=5, ypadding=5)
  gtkTableAttach(tbl,out.r.entry, 1, 2, 3, 4, xpadding=5, ypadding=5)
  
  
  shape.file.box <- gtkHBox(FALSE, 10)
  browse.b <- gtkButton("Browse")
  gSignalConnect(browse.b, "clicked", browse.for.shapefile,data=list(
    shape.in.entry = shape.in.entry, 
    shape.in.dir = shape.in.dir,
    out.r.entry = out.r.entry,
    parent.window = win
  ))
  
  
  shape.file.box$packEnd(browse.b)
  shape.file.box$packStart(shape.in.entry)
  
  gtkTableAttach(tbl,shape.file.label, 0, 1, 1, 2, xpadding=5, ypadding=5)
  gtkTableAttach(tbl,shape.file.box, 1, 2, 1, 2, xpadding=5, ypadding=5)
  
  # #   ---- Stratum Names
  unequal.var.entry <- gtkEntry()
  unequal.var.entry$setText( "" )
  
  
  # jason add 6/16/2015
  f.write.var.label <- function(x,dat){
    prop.active <- cont.rb$getActive()
    const.active <- const.rb$getActive()
    
    if(prop.active){
      unequal.var.label$setText("Name of Continuous Variable:")
    } else if( const.active ){
      unequal.var.label$setText("Name of Categorical Variable:")
    } else {
      # Note, because the three buttons are in a group, you don't need signal for the last one
      unequal.var.label$setText("Name of Categorical Variable:")
    }
  }
  
  unequal.var.label <- gtkLabel("Name of Continuous Variable:")   # basically the default display when first opened

  
  gtkTableAttach(tbl,unequal.var.label, 0, 1, 2, 3, xpadding=5, ypadding=5)
  gtkTableAttach(tbl,unequal.var.entry, 1, 2, 2, 3, xpadding=5, ypadding=5)
  
  
  
  
  # ============================ Sample Allocation frame ===========
  #hbox1 <- gtkHBoxNew(FALSE, 8) #sets up another horizontal box, FALSE means things not evenly spaced, 8 is for 8 pixels between things
  #hbox1$setBorderWidth(8)
  #vbox1$add(hbox1)
  
  samp.alloc.frame <- gtkFrameNew("Sample Allocation")
  hbox1$add(samp.alloc.frame)
  
  #  Radio Buttons to Specify Sample Allocation 
  stype.box <- gtkVBoxNew(TRUE, 2)
  stype.box$setBorderWidth(8)
  samp.alloc.frame$add( stype.box )
  
  cont.rb <- gtkRadioButtonNewWithLabel(label="Continuous")
  const.rb <- gtkRadioButtonNewWithLabelFromWidget(cont.rb,"Constant")
  uneqprop.rb <- gtkRadioButtonNewWithLabelFromWidget(cont.rb,"Unequal Proportion")
  #user.entry <-gtkEntry()
  #user.entry$setText( "" ) #keep box blank
  
  stype.box$packStart(cont.rb, TRUE, TRUE, 2)
  stype.box$packStart(const.rb, TRUE, TRUE, 2)
  stype.box$packStart(uneqprop.rb, TRUE, TRUE, 2)
  #stype.box$packStart(user.entry, TRUE, TRUE, 2) 
  #this creates a box next to the user-specified button
  #it would be nice to only have this box pop up if the user-specified button is clicked
  
  f.write.sample.label <- function(x,dat){
    prop.active <- cont.rb$getActive()
    const.active <- const.rb$getActive()
    
    if(prop.active){
      n.label$setText("Specify: total n across\n\tcontinous variable range")
    } else if( const.active ){
      n.label$setText("Specify: total n across\n\tall categories")
    } else {
      # Note, because the three buttons are in a group, you don't need signal for the last one
      n.label$setText("Specify: a comma delimited\n\tlist of n, in alphabetized\n\tcategorical order")
    }
  }
  
  
  gSignalConnect(cont.rb, "toggled", f.write.sample.label )
  gSignalConnect(const.rb, "toggled", f.write.sample.label )
  
  # jason 6/16/2015 - do stuff in the left box when radio buttons toggled.
  gSignalConnect(cont.rb, "toggled", f.write.var.label )
  gSignalConnect(const.rb, "toggled", f.write.var.label )
  
  #   ---- Sample sizes
  
  n.frame <- gtkFrameNew( "Sample Size")
  
  # n.tbl <- gtkTableNew(7,4,homogeneous=FALSE) #Bigger than we need. FALSE for nonhomogeneous
  # gtkTableSetRowSpacings(n.tbl,1) #1 pixel between rows
  # gtkTableSetColSpacings(n.tbl,5) #5 pixels between column
  
  hbox1$add(n.frame)
  
  n.vbox <- gtkVBoxNew(TRUE, 2)
  n.vbox$setBorderWidth(8)
  n.frame$add( n.vbox )
  
  n.entry <- gtkEntry()
  n.entry$setText( "" )
  
  n.label <- gtkLabel("Specify: total n across all\n\tcategories") 
  n.label2 <- gtkLabel(" ")
  
  n.vbox$packStart(n.label)
  n.vbox$packStart(n.entry)
  n.vbox$packStart(n.label2)
  
  # gtkTableAttach(n.tbl,tot.size.label, 0, 1, 0, 1, xpadding=5, ypadding=5)
  # gtkTableAttach(n.tbl,n.entry, 1, 2, 0, 1, xpadding=5, ypadding=5)
  
  
  # =========================== Frame information area ==================================
  
  #   ---- Separator
  vbox1$packStart(gtkHSeparatorNew(), expand=FALSE)
  
  
  finfo.vbox <- gtkHBoxNew(FALSE,2)
  finfo.vbox$setBorderWidth(8)
  vbox1$packStart(finfo.vbox)
  
  finfo.title <- gtkLabel("Frame Type:    \n<pending>")
  finfo.vbox$packStart(finfo.title, expand=FALSE, fill=FALSE)
  
  finfo.vbox$packStart(gtkVSeparatorNew(), expand=FALSE)
  
  max.vars <- 20  # maximum number of variables to display
  n.blank.cols <- 4  # must be even, half place on left and half on right
  
  finfo.tbl <- gtkTable(max.vars+1,n.blank.cols+2,FALSE) #FALSE for nonhomogeneous
  gtkTableSetRowSpacings(finfo.tbl,1) #1 pixel between rows
  gtkTableSetColSpacings(finfo.tbl,5) #5 pixels between columns
  finfo.vbox$packStart(finfo.tbl)
  
  # Allocate the labels
  names.labs <- lapply(1:(max.vars+1), gtkLabel, str="")
  vtypes.labs <- lapply(1:(max.vars+1), gtkLabel, str="")
  
  # Place column header labels
  names.labs[[1]]$setText("VARIABLE")
  vtypes.labs[[1]]$setText("CLASS")
  
  gtkTableAttach(finfo.tbl, names.labs[[1]], (n.blank.cols/2), (n.blank.cols/2)+1, 0,1 )
  gtkTableAttach(finfo.tbl, vtypes.labs[[1]], (n.blank.cols/2)+1, (n.blank.cols/2)+2, 0,1 )
  
  # Place separators
  gtkTableAttach(finfo.tbl, gtkHSeparatorNew(), (n.blank.cols/2), (n.blank.cols/2)+1, 1,2)
  gtkTableAttach(finfo.tbl, gtkHSeparatorNew(), (n.blank.cols/2)+1, (n.blank.cols/2)+2, 1,2)
  
  # Set thier length
  #     f.setlablen <-function(x,lablist){
  #         lablist[[x]]$setWidthChars(25)
  #         #lablist[[x]]$setJustify(GTK_JUSTIFY_LEFT)
  #         #lablist[[x]]$setAlignment(0,.5)
  #     }
  #     names.labs <- lapply(1:(max.vars+1), f.setlablen, lablist=names.labs)
  #     vtypes.labs <- lapply(1:(max.vars+1), f.setlablen, lablist=vtypes.labs)
  
  # place them
  placelabs <-  function(x, lablist, obj, labcol, bcols){
    gtkTableAttach( obj, lablist[[x+1]], bcols+labcol-1, bcols+labcol, x-1+2, x+2) # + 2 for header
  }
  
  lapply(1:max.vars, placelabs, lablist=names.labs, obj=finfo.tbl, labcol=1, bcols=n.blank.cols/2)
  lapply(1:max.vars, placelabs, lablist=vtypes.labs, obj=finfo.tbl, labcol=2, bcols=n.blank.cols/2 )
  
  
  #     blank.labs <- lapply(1:(n.blank.cols+2), gtkLabel, str=" ")
  #     placeblanklabs <-  function(x, lablist, obj, side){
  #       gtkTableAttach( obj, lablist[[side+x]], side+x-1, side+x, 0, 1)
  #     }
  #     lapply(1:(n.blank.cols/2), placeblanklabs, lablist=blank.labs, obj=finfo.tbl, side=0)
  #     lapply(1:(n.blank.cols/2), placeblanklabs, lablist=blank.labs, obj=finfo.tbl, side=(n.blank.cols/2)+1)
  
  # Initial values in columns, and hide all but first
  names.labs[[2]]$setText("<pending>")
  vtypes.labs[[2]]$setText("<pending>")
  lapply(2:max.vars, function(x,lablist){lablist[[x+1]]$hide()}, lablist=names.labs)
  lapply(2:max.vars, function(x,lablist){lablist[[x+1]]$hide()}, lablist=vtypes.labs)
  
  
  
  
  
  # Bottom row of buttons ---------------------------------------------------
  # =========================== Bottom row of buttons ==================================
  
  
  #   ---- Separator
  vbox1$packStart(gtkHSeparatorNew(), expand=FALSE)
  
  
  #   ---- Define box for row of buttons at bottom
  bbox <- gtkHButtonBox()
  bbox$SetLayout("Spread")                   # Layout can be c("Start", "Edge", "Spread", "End")
  bbox$SetBorderWidth(10)
  
  #   ---- Read frame button, but do not draw sample, this displays variables in shapefile
  read.b <- gtkButton("Inspect\n Frame ")
  gSignalConnect(read.b, "clicked", readButtonAction, 
                 data=list(
                   shape.in.entry=shape.in.entry,
                   shape.in.dir=shape.in.dir,
                   out.r.entry=out.r.entry,
                   name.labs=names.labs,
                   type.labs=vtypes.labs, 
                   finfo.title=finfo.title
                 )
  )
  bbox$packEnd(read.b, expand=FALSE)
  
  #   ---- Run button
  run.b <- gtkButton("Run")
  gSignalConnect(run.b, "clicked", run.unequal.sample, data=list( 
    samp.type.combo=samp.type.combo,
    n.entry=n.entry,
    shape.in.entry=shape.in.entry,
    shape.in.dir=shape.in.dir,
    unequal.var.entry=unequal.var.entry,
    out.r.entry=out.r.entry,
    over.entry=over.entry,
    seed.entry=seed.entry, 
    cont.rb=cont.rb,
    const.rb=const.rb, 
    uneqprop.rb=uneqprop.rb
  )
  ) 
  bbox$packEnd(run.b, expand=FALSE)
  
  #   ---- Read frame button, but do not draw sample, this displays variables in shapefile
  plot.b <- gtkButton("  Plot\nSample")
  gSignalConnect(plot.b, "clicked", readButtonAction, 
                 data=list(
                   shape.in.entry=shape.in.entry,
                   shape.in.dir=shape.in.dir,
                   out.r.entry=out.r.entry,
                   name.labs=names.labs,
                   type.labs=vtypes.labs, 
                   finfo.title=finfo.title
                 )
  )
  bbox$packEnd(plot.b, expand=FALSE)  
  
  #   ---- View button
  view.b <- gtkButton("Tabulate\n Sample")
  gSignalConnect(view.b, "clicked", view.sample, data=list(
    out.r.entry = out.r.entry
  ))
  bbox$packEnd( view.b, expand=FALSE)
  
  
  # ???   #   ---- Write to csv button
  #    write.csv.b <- gtkButton("Write CSV")
  #    gSignalConnect(write.csv.b, "clicked", SDraw::my.write.csv, data=list(
  #            out.r.entry = out.r.entry
  #    ))
  #    bbox$packEnd( write.csv.b, expand=FALSE)
  
  #   ---- Write to Shapefile button
  write.shp.b <- gtkButton("Export")
  gSignalConnect(write.shp.b, "clicked", my.write.shp, data=list(
    out.r.entry = out.r.entry, 
    parent.window = win            
  ))
  bbox$packEnd( write.shp.b, expand=FALSE)
  
  
  #   ---- Done button
  cancel.b <- gtkButton("Done")
  gSignalConnect(cancel.b, "clicked", function(x){
    win$Hide();
    win$Destroy()
  })
  bbox$packEnd( cancel.b, expand=FALSE)
  
  
  #   ---- Pack the rows of buttons into the vertical box
  vbox1$packEnd( bbox, expand=FALSE)
  
  
  #   ---- Finally, show the window
  win$Show()
  
}
