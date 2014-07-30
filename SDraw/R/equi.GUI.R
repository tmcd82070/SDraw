equi.GUI <- function()   {
#
#   Setup and run a GUI to take a BAS sample 
#


    INPUT.DIR <- getwd()


    #   ---- Define the main window
    win <- gtkWindowNew("toplevel")
    win$setBorderWidth(8)
    win$setTitle("S-Draw : A sample drawing interface")
    #gtkWindowSetIconFromFile(win, filename = "s-draw.ico")  # need path to be correct here, or does not work, obviously

    vbox1 <- gtkVBoxNew(FALSE, 8)
    vbox1$setBorderWidth(8)
    win$add(vbox1)

    # ================= Sample type frame ============================
    samp.types <- c("BAS - Balanced Acceptance Sampling", "GRTS - Generalized Random Tesselation Stratified", 
            "SSS - Simple Systematic Sampling")
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
    hbox1 <- gtkHBoxNew(FALSE, 8)
    hbox1$setBorderWidth(8)
    vbox1$add(hbox1)



    # ================= Required Inputs frame ============================
    req.frame <- gtkFrameNew("Required Inputs")
    hbox1$add(req.frame)

    #   ---- Define a verticle box
    req.vbox <- gtkVBoxNew(FALSE, 8)
    req.vbox$setBorderWidth(8)
    req.frame$add(req.vbox)


    #   ---- Define table of boxes so everything aligns
    tbl <- gtkTable(3,2,FALSE)
    gtkTableSetRowSpacings(tbl,1)
    gtkTableSetColSpacings(tbl,5)

    gtkBoxPackStart(req.vbox,tbl)

    #   ---- Sample size text box
    #samp.size.box <- gtkHBox(FALSE, 10)
    n.entry <- gtkEntry()
    n.entry$setText( "" )
    #gtkEntrySetText( n.entry, "")

    samp.size.label <- gtkLabel("Sample size (n):")

    gtkTableAttach(tbl,samp.size.label, 0, 1, 0, 1, xpadding=5, ypadding=5)
    gtkTableAttach(tbl,n.entry, 1, 2, 0, 1, xpadding=5, ypadding=5)

    #   ---- Input shape file box
    shape.in.entry <- gtkEntry()
    shape.in.entry$setText( "" )
    shape.file.label <- gtkLabel("Shape file:")

    shape.file.box <- gtkHBox(FALSE, 10)
    browse.b <- gtkButton("Browse")
    gSignalConnect(browse.b, "clicked", function(x,dat){
        INPUT.DIR <- browse.for.shapefile(INPUT.DIR,dat)
    }, data=list(
        shape.in.entry = shape.in.entry
    ))
    shape.file.box$packEnd(browse.b)
    shape.file.box$packStart(shape.in.entry)

    gtkTableAttach(tbl,shape.file.label, 0, 1, 1, 2, xpadding=5, ypadding=5)
    gtkTableAttach(tbl,shape.file.box, 1, 2, 1, 2, xpadding=5, ypadding=5)

    #   ---- Output R object box
    out.r.entry <- gtkEntry()
    out.r.entry$setText(paste("sdraw.", format(Sys.time(), "%Y.%m.%d.%H%M%S"), sep=""))
    out.r.label <- gtkLabel("Output R object:")

    gtkTableAttach(tbl,out.r.label, 0, 1, 2, 3, xpadding=5, ypadding=5)
    gtkTableAttach(tbl,out.r.entry, 1, 2, 2, 3, xpadding=5, ypadding=5)


    # =========================== Optional inputs frame ================================
    opt.frame <- gtkFrameNew("Optional Inputs")
    hbox1$add(opt.frame)

    opt.vbox <- gtkVBoxNew(FALSE, 8)
    opt.vbox$setBorderWidth(8)
    opt.frame$add(opt.vbox)

    #   ---- Define table of boxes so everything aligns
    opt.tbl <- gtkTable(3,2,FALSE)
    gtkTableSetRowSpacings(tbl,1)
    gtkTableSetColSpacings(tbl,5)

    opt.vbox$add(opt.tbl)

    #   ---- Over sample size text box
    over.entry <- gtkEntry()
    over.entry$setText( "0" )
    over.size.label <- gtkLabel("Over sample:")

    gtkTableAttach(opt.tbl,over.size.label, 0, 1, 0, 1, xpadding=5, ypadding=5)
    gtkTableAttach(opt.tbl,over.entry, 1, 2, 0, 1, xpadding=5, ypadding=5)

    #   ---- Seed text box
    seed.entry <- gtkEntry()
    seed.entry$setText( "" )
    seed.label <- gtkLabel("Random number seed:")

    gtkTableAttach(opt.tbl,seed.label, 0, 1, 1, 2, xpadding=5, ypadding=5)
    gtkTableAttach(opt.tbl,seed.entry, 1, 2, 1, 2, xpadding=5, ypadding=5)

    # =========================== Frame Type ================================
    samp.type.frame <- gtkFrameNew("Frame Type")
    hbox1$add(samp.type.frame)

    stype.box <- gtkVBoxNew(TRUE, 2)
    samp.type.frame$add( stype.box )

    area.rb <- gtkRadioButton(label="Area\n(polygon shapefile)")
    line.rb <- gtkRadioButtonNewWithLabelFromWidget(area.rb,"Linear\n(line shapefile)")
    disc.rb <- gtkRadioButtonNewWithLabelFromWidget(area.rb,"Finite\n(point shapefile)")


    stype.box$packStart(area.rb, TRUE, TRUE, 2)
    stype.box$packStart(line.rb, TRUE, TRUE, 2)
    stype.box$packStart(disc.rb, TRUE, TRUE, 2)

    # =========================== Bottom row of buttons ==================================

    #   ---- Separator
    gtkBoxPackStart(vbox1, gtkHSeparatorNew(), expand=FALSE)


    #   ---- Define box for row of buttons at bottom
    bbox <- gtkHButtonBox()
    bbox$SetLayout("Spread")                   # Layout can be c("Start", "Edge", "Spread", "End")
    bbox$SetBorderWidth(10)

    #   ---- Run button
    run.b <- gtkButton("Run")
    gSignalConnect(run.b, "clicked", run.sample, data=list( 
            samp.type.combo=samp.type.combo,
            n.entry=n.entry,
            shape.in.entry=shape.in.entry,
            out.r.entry=out.r.entry,
            over.entry=over.entry,
            seed.entry=seed.entry, 
            area.rb=area.rb,
            line.rb=line.rb,
            disc.rb=disc.rb,
            input.dir=INPUT.DIR)
    ) 
    bbox$packEnd(run.b, expand=FALSE)


    #   ---- Plot button
    plot.b <- gtkButton("Map")
    gSignalConnect(plot.b, "clicked", plotSample, data=list(
            shape.in.entry=shape.in.entry,
            out.r.entry=out.r.entry,
            input.dir=INPUT.DIR)
    )
    bbox$packEnd(plot.b, expand=FALSE)

#   Here!!! 
#   Take out frame type radio buttons.  Determine frame type from shape file.
#   Add radio buttons or dialog to set input and output format to a subset 
#       of formats in ogrDrivers()$name (KML, CSV, GPX, ESRI Shapefile)
#   Change to rgdal's readOGR and writeOGR (from spsurvey's readshape())

    #   ---- View button
    view.b <- gtkButton("View Sample")
    gSignalConnect(view.b, "clicked", view.sample, data=list(
            out.r.entry = out.r.entry
    ))
    bbox$packEnd( view.b, expand=FALSE)


    #   ---- Write to csv button
    write.csv.b <- gtkButton("Write CSV")
    gSignalConnect(write.csv.b, "clicked", my.write.csv, data=list(
            out.r.entry = out.r.entry
    ))
    bbox$packEnd( write.csv.b, expand=FALSE)

    #   ---- Write to Shapefile button
    write.shp.b <- gtkButton("Write Shapefile")
    gSignalConnect(write.shp.b, "clicked", my.write.shp, data=list(
            out.r.entry = out.r.entry
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
