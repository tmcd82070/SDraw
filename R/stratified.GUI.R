stratified.GUI <- function()   {
#
#   Setup and run a GUI to take a BAS sample 
#

    design <- "stratified"

    #   ---- Define the main window
    win <- gtkWindowNew("toplevel")
    win$setBorderWidth(8) 
    win$setTitle("S-Draw : A sample drawing interface--Stratified design")
    #gtkWindowSetIconFromFile(win, filename = "s-draw.ico")  # need path to be correct here, or does not work, obviously

    vbox1 <- gtkVBoxNew(FALSE, 8)
    vbox1$setBorderWidth(8)
    win$add(vbox1)

    # ================= Sample type frame ============================
    samp.types <- c("HAL - Halton Lattice Sampling", "BAS - Balanced Acceptance Sampling", "GRTS - Generalized Random Tesselation Stratified", 
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
    shape.file.label <- gtkLabel("Shape file:")

    shape.file.box <- gtkHBox(FALSE, 10)
    browse.b <- gtkButton("Browse")
    gSignalConnect(browse.b, "clicked", browse.for.shapefile,data=list(
        shape.in.entry = shape.in.entry, 
        parent.window = win
    ))
    
   
    shape.file.box$packEnd(browse.b)
    shape.file.box$packStart(shape.in.entry)

    gtkTableAttach(tbl,shape.file.label, 0, 1, 1, 2, xpadding=5, ypadding=5)
    gtkTableAttach(tbl,shape.file.box, 1, 2, 1, 2, xpadding=5, ypadding=5)

  	# #   ---- Input stratifying variable information Guy added this section 12/19
  	# #   ---- Stratum Names
  	strata.var.entry <- gtkEntry()
  	strata.var.entry$setText( "" )
  	strata.var.label <- gtkLabel("Name of Stratifying Variable:")
	
    gtkTableAttach(tbl,strata.var.label, 0, 1, 2, 3, xpadding=5, ypadding=5)
    gtkTableAttach(tbl,strata.var.entry, 1, 2, 2, 3, xpadding=5, ypadding=5)
	
	
    #   ---- Output R object box
    out.r.entry <- gtkEntry()
    out.r.entry$setText(paste("sdraw.", format(Sys.time(), "%Y.%m.%d.%H%M%S"), sep=""))
    out.r.label <- gtkLabel("Sample's R name:")

    gtkTableAttach(tbl,out.r.label, 0, 1, 3, 4, xpadding=5, ypadding=5)
    gtkTableAttach(tbl,out.r.entry, 1, 2, 3, 4, xpadding=5, ypadding=5)


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
    
    prop.rb <- gtkRadioButton(label="Proportional to Size")
    const.rb <- gtkRadioButtonNewWithLabelFromWidget(prop.rb,"Constant")
    user.rb <- gtkRadioButtonNewWithLabelFromWidget(prop.rb,"User-specified")
    #user.entry <-gtkEntry()
    #user.entry$setText( "" ) #keep box blank
    
    stype.box$packStart(prop.rb, TRUE, TRUE, 2)
    stype.box$packStart(const.rb, TRUE, TRUE, 2)
    stype.box$packStart(user.rb, TRUE, TRUE, 2)
    #stype.box$packStart(user.entry, TRUE, TRUE, 2) 
    #this creates a box next to the user-specified button
    #it would be nice to only have this box pop up if the user-specified button is clicked
    
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
    
    n.label <- gtkLabel("Specify total sample size (N):") 
    n.label2 <- gtkLabel(" ")

    n.vbox$packStart(n.entry)
    n.vbox$packStart(n.label)
    n.vbox$packStart(n.label2)

    # gtkTableAttach(n.tbl,tot.size.label, 0, 1, 0, 1, xpadding=5, ypadding=5)
    # gtkTableAttach(n.tbl,n.entry, 1, 2, 0, 1, xpadding=5, ypadding=5)


    # =========================== Optional inputs frame ================================
    opt.hbox <- gtkHBoxNew(TRUE, 2)
    opt.hbox$setBorderWidth(8)
    vbox1$add(opt.hbox)

    opt.frame <- gtkFrameNew("Optional Inputs")
    opt.hbox$packStart(opt.frame)

    opt.blank.box <- gtkHBoxNew(TRUE,2)
    opt.hbox$packStart(opt.blank.box)

    opt.vbox <- gtkVBoxNew(FALSE, 8)
    opt.vbox$setBorderWidth(8)
    opt.frame$add(opt.vbox)


    #   ---- Define table of boxes so everything aligns
    opt.tbl <- gtkTable(7,5,FALSE)
    gtkTableSetRowSpacings(tbl,1)
    gtkTableSetColSpacings(tbl,5)

    opt.vbox$add(opt.tbl)

    #   ---- Over sample size text boxes
    over.entry <- gtkEntry()
    over.entry$setText( "0" )
    over.size.label <- gtkLabel("Over sample, each strata:")

    gtkTableAttach(opt.tbl,over.size.label, 0, 1, 0, 1, xpadding=5, ypadding=5)
    gtkTableAttach(opt.tbl,over.entry, 1, 2, 0, 1, xpadding=5, ypadding=5)

		
    #   ---- Seed text box
    seed.entry <- gtkEntry()
    seed.entry$setText( "" )
    seed.label <- gtkLabel("Random number seed:")

    gtkTableAttach(opt.tbl,seed.label, 0, 1, 1, 2, xpadding=5, ypadding=5)
    gtkTableAttach(opt.tbl,seed.entry, 1, 2, 1, 2, xpadding=5, ypadding=5)

    #   ---- Put in some blank space to pretty it up
    blank.lab <- gtkLabel("                          ")
    blank.lab2 <- gtkLabel(" ")

    gtkTableAttach(opt.tbl,blank.lab, 3, 4, 0, 1, xpadding=5, ypadding=5)
    gtkTableAttach(opt.tbl,blank.lab2, 4, 5, 0, 1, xpadding=5, ypadding=5)


    # =========================== Bottom row of buttons ==================================

    #   ---- Separator
    vbox1$packStart(gtkHSeparatorNew(), expand=FALSE)


    #   ---- Define box for row of buttons at bottom
    bbox <- gtkHButtonBox()
    bbox$SetLayout("Spread")                   # Layout can be c("Start", "Edge", "Spread", "End")
    bbox$SetBorderWidth(10)

    #   ---- Run button
    run.b <- gtkButton("Run")
    gSignalConnect(run.b, "clicked", run.strat.sample, data=list( 
            samp.type.combo=samp.type.combo,
            n.entry=n.entry,
            shape.in.entry=shape.in.entry,
			strata.var.entry=strata.var.entry,
            out.r.entry=out.r.entry,
            over.entry=over.entry,
			seed.entry=seed.entry
            )
    ) 
    bbox$packEnd(run.b, expand=FALSE)


    # #   ---- Plot button
    # plot.b <- gtkButton("Map")
    # gSignalConnect(plot.b, "clicked", #function(x,dat){print(dat$input.dir)}, 
    # plotSample, 
    # data=list(
            # shape.in.entry=shape.in.entry,
            # out.r.entry=out.r.entry
            # )
    # )
    # bbox$packEnd(plot.b, expand=FALSE)


    # #   ---- View button
    # view.b <- gtkButton("View Sample")
    # gSignalConnect(view.b, "clicked", view.sample, data=list(
            # out.r.entry = out.r.entry
    # ))
    # bbox$packEnd( view.b, expand=FALSE)


#    #   ---- Write to csv button
#    write.csv.b <- gtkButton("Write CSV")
#    gSignalConnect(write.csv.b, "clicked", SDraw::my.write.csv, data=list(
#            out.r.entry = out.r.entry
#    ))
#    bbox$packEnd( write.csv.b, expand=FALSE)

    # #   ---- Write to Shapefile button
    # write.shp.b <- gtkButton("Export")
    # gSignalConnect(write.shp.b, "clicked", my.write.shp, data=list(
            # out.r.entry = out.r.entry, 
            # parent.window = win            
    # ))
    # bbox$packEnd( write.shp.b, expand=FALSE)


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
