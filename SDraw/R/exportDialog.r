exportDialog <- function(x,dat){

        #   ==================== Export dialog box
        export.win <- gtkWindowNew()
        export.win$setBorderWidth(8)
        export.win$setTitle("Export sample draw")
        #export.win$setModal(TRUE)
    
        vbox1 <- gtkVBoxNew(FALSE, 8)
        vbox1$setBorderWidth(8)
        export.win$add(vbox1)

        # ---- export format combo box
        export.formats <- c("ESRI Shapefile (.SHP)", "Comma Separated (.CSV)", "Google Earth (.KML)", "Garmin format(.GPX)" )
        export.format.combo <- gtkComboBoxNewText()
        export.format.combo$show()
        for( i in export.formats ){
            export.format.combo$appendText( i )
        }
        export.format.combo$setActive(0)


        export.format.frame <- gtkFrameNew("Format:")
        export.format.frame$setBorderWidth(8)

        combo.box <- gtkHBoxNew(FALSE, 8)
        combo.box$setBorderWidth(8)
        combo.box$packStart( export.format.combo )

        export.format.frame$add( combo.box )

        hbox2 <- gtkHBoxNew(FALSE, 8)
        hbox2$packStart(export.format.frame)
        vbox1$packStart(hbox2)

        # ------ File name entry box

        hbox1 <- gtkHBoxNew(FALSE, 8)
        hbox1$setBorderWidth(8)
        vbox1$add(hbox1)

        filename.frame <- gtkFrameNew("Export File Name:")
        hbox1$add(filename.frame)

        vbox3 <- gtkVBoxNew(FALSE, 8)
        filename.frame$add(vbox3)
        

        filename.hbox <- gtkHBoxNew(FALSE, 8)
        filename.hbox$setBorderWidth(8)

        filename.entry <- gtkEntry()
        filename.entry$setText(samp.nm2)
        filename.hbox$packStart(filename.entry)

        choose.b <- gtkButton("Choose file name")
        gSignalConnect(choose.b, "clicked", SDraw:::choose.export.filename,data=list(
            filename.entry = filename.entry
        ))
        filename.hbox$packStart(choose.b)

        vbox3$packStart(filename.hbox)
        
        export.dir.label <- gtkLabel("Directory:")
        export.dir.entry <- gtkEntry( get(".INPUT.DIR") )

        dir.hbox <- gtkHBoxNew(FALSE, 8)
        dir.hbox$packStart( export.dir.label )
        dir.hbox$packStart( export.dir.entry )
        
        vbox3$packStart(export.dir.entry)

        #   ------ Ok and cancel buttons
        ok.b <- gtkButton( "Ok" )
        cancel.b <- gtkButton( "Cancel" )

        gSignalConnect( ok.b, "clicked", function(x,dat){
            ex.format=export.format.combo$getActiveText()
            ex.file = filename.entry$getText()
            ex.dir = export.dir.entry$getText()
            assign(".LAST.EXPORT", list(
                ex.format=ex.format, 
                ex.file=ex.file,
                ex.dir = ex.dir), envir=.GlobalEnv )
            
            export.win$Hide()
            export.win$Destroy()
        }, data=list(
            export.format.combo = export.format.combo,
            filename.entry = filename.entry,
            export.dir = export.dir
        ))

        gSignalConnect(cancel.b, "clicked", function(x){
            assign(".LAST.EXPORT", list(
                ex.format=NA, 
                ex.file=NA,
                ex.dir = NA), envir=.GlobalEnv )

            export.win$Hide();
            export.win$Destroy()
        })

        vbox1$add(ok.b)
        vbox1$add{cancel.b)

        export.win$Show()
}
