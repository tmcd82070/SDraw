equi.GUI <- function()   {
#
#   Setup and run a GUI to take a BAS sample 
#

    require( RGtk2 )

    assign( ".INPUT.DIR", getwd(), pos=.GlobalEnv )


    #   ---- Define the main window
    .win <<- gtkWindowNew("toplevel")
    .win$setBorderWidth(8)
    .win$setTitle("S-Draw : A sample drawing interface")
    #gtkWindowSetIconFromFile(.win, filename = "s-draw.ico")  # need path to be correct here, or does not work, obviously

    vbox1 <- gtkVBoxNew(FALSE, 8)
    vbox1$setBorderWidth(8)
    .win$add(vbox1)

    # ================= Sample type frame ============================
    samp.types <- c("BAS - Balanced Acceptance Sampling", "GRTS - Generalized Random Tesselation Stratified", 
            "SSS - Simple Systematic Sampling")
    .samp.type.combo <<- gtkComboBoxNewText()
    .samp.type.combo$show()
    for( i in samp.types ){
        .samp.type.combo$appendText( i )
    }
    .samp.type.combo$setActive(0)
    
#    print(gtkComboBoxGetActive(.samp.type.combo))
#    print(gtkComboBoxGetWrapWidth(.samp.type.combo))

    samp.type.frame <- gtkFrameNew("Sample Type")
    samp.type.frame$setBorderWidth(8)
    combo.box <- gtkHBoxNew(FALSE, 8)
    combo.box$setBorderWidth(8)
    gtkBoxPackStart(combo.box, .samp.type.combo )
    samp.type.frame$add( combo.box )

    hbox2 <- gtkHBoxNew(FALSE, 8)
    #hbox2$setBorderWidth(8)
    gtkBoxPackStart(hbox2,samp.type.frame)
    #hbox2$add(samp.type.frame)

#    logo <- gtkImageNewFromFile("s_draw_banner.png")
#    gtkBoxPackStart(hbox2,logo)

    gtkBoxPackStart(vbox1,hbox2)

    #vbox1$add(hbox2)



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
    .n.entry <<- gtkEntry()
    .n.entry$setText( "" )
    #gtkEntrySetText( n.entry, "")

    samp.size.label <- gtkLabel("Sample size (n):")

    gtkTableAttach(tbl,samp.size.label, 0, 1, 0, 1, xpadding=5, ypadding=5)
    gtkTableAttach(tbl,.n.entry, 1, 2, 0, 1, xpadding=5, ypadding=5)

    #   ---- Input shape file box
    .shape.in.entry <<- gtkEntry()
    .shape.in.entry$setText( "" )
    #gtkEntrySetText( .shape.in.entry, "")
    shape.file.label <- gtkLabel("Shape file:")

    shape.file.box <- gtkHBox(FALSE, 10)
    browse.b <- gtkButton("Browse")
    gtkAddCallback(browse.b, "clicked", function(x){browse.for.shapefile()} )
    gtkBoxPackEnd(shape.file.box,browse.b)
    gtkBoxPackStart(shape.file.box,.shape.in.entry)

    gtkTableAttach(tbl,shape.file.label, 0, 1, 1, 2, xpadding=5, ypadding=5)
    gtkTableAttach(tbl,shape.file.box, 1, 2, 1, 2, xpadding=5, ypadding=5)

    #   ---- Output R object box
    .out.r.entry <<- gtkEntry()
    .out.r.entry$setText(paste("sdraw.", format(Sys.time(), "%Y.%m.%d.%H%M%S"), sep=""))
    out.r.label <- gtkLabel("Output R object:")

    gtkTableAttach(tbl,out.r.label, 0, 1, 2, 3, xpadding=5, ypadding=5)
    gtkTableAttach(tbl,.out.r.entry, 1, 2, 2, 3, xpadding=5, ypadding=5)


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
    .over.entry <<- gtkEntry()
    .over.entry$setText( "0" )
    over.size.label <- gtkLabel("Over sample:")

    gtkTableAttach(opt.tbl,over.size.label, 0, 1, 0, 1, xpadding=5, ypadding=5)
    gtkTableAttach(opt.tbl,.over.entry, 1, 2, 0, 1, xpadding=5, ypadding=5)

    #   ---- Seed text box
    .seed.entry <<- gtkEntry()
    .seed.entry$setText( "" )
    seed.label <- gtkLabel("Random number seed:")

    gtkTableAttach(opt.tbl,seed.label, 0, 1, 1, 2, xpadding=5, ypadding=5)
    gtkTableAttach(opt.tbl,.seed.entry, 1, 2, 1, 2, xpadding=5, ypadding=5)

    # =========================== Frame Type ================================
    samp.type.frame <- gtkFrameNew("Frame Type")
    hbox1$add(samp.type.frame)

    stype.box <- gtkVBoxNew(TRUE, 2)
    samp.type.frame$add( stype.box )

    .area.rb <<- gtkRadioButton(label="Area\n(polygon shapefile)")
    .line.rb <<- gtkRadioButtonNewWithLabelFromWidget(.area.rb,"Linear\n(line shapefile)")
    disc.rb <- gtkRadioButtonNewWithLabelFromWidget(.area.rb,"Finite\n(point shapefile)")


    stype.box$packStart(.area.rb, TRUE, TRUE, 2)
    stype.box$packStart(.line.rb, TRUE, TRUE, 2)
    stype.box$packStart(disc.rb, TRUE, TRUE, 2)

    # =========================== Bottom row of buttons ==================================

    #   ---- Separator
    gtkBoxPackStart(vbox1, gtkHSeparatorNew(), expand=FALSE)


    #   ---- Define box for row of buttons at bottom
    bbox <- gtkHButtonBox()
    #gtkHButtonBoxSetSpacingDefault(10)
    bbox$SetLayout("Spread")                   # Layout can be c("Start", "Edge", "Spread", "End")
    bbox$SetBorderWidth(10)

    #   ---- Run button
    run.b <- gtkButton("Run")
    gtkObjectAddCallback(run.b, "clicked", run.sample)
    gtkBoxPackEnd(bbox, run.b, expand=FALSE)

    #   ---- Plot button
    plot.b <- gtkButton("Map")
    gtkObjectAddCallback(plot.b, "clicked", plot.sample)
    gtkBoxPackEnd(bbox, plot.b, expand=FALSE)

    #   ---- View button
    view.b <- gtkButton("View Sample")
    gtkObjectAddCallback(view.b, "clicked", view.sample)
    gtkBoxPackEnd(bbox, view.b, expand=FALSE)


    #   ---- Write to csv button
    write.csv.b <- gtkButton("Write CSV")
    gtkObjectAddCallback(write.csv.b, "clicked", my.write.csv)
    gtkBoxPackEnd(bbox, write.csv.b, expand=FALSE)

    #   ---- Write to Shapefile button
    write.shp.b <- gtkButton("Write Shapefile")
    gtkObjectAddCallback(write.shp.b, "clicked", my.write.shp)
    gtkBoxPackEnd(bbox, write.shp.b, expand=FALSE)


    #   ---- Cancel button
    cancel.b <- gtkButton("Done")
    gtkObjectAddCallback(cancel.b, "clicked", cleanup)
    gtkBoxPackEnd(bbox, cancel.b, expand=FALSE)


    #   ---- Pack the rows of buttons into the vertical box
    gtkBoxPackEnd(vbox1, bbox, expand=FALSE)


    #   ---- Finally, show the window
    .win$Show()

}
