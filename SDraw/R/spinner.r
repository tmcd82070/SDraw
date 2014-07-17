start.spinner <- function(){

.spin.win <<- gtkDialogNew()

.spin.win$setResizable(FALSE)
.spin.win$setTitle("")
gtkWindowSetIcon(.spin.win, NULL)
    
gSignalConnect(.spin.win, "response", gtkWidgetDestroy)
gSignalConnect(.spin.win, "destroy", gtkWidgetDestroy)

content_area <- .spin.win$getContentArea()

vbox <- gtkVBox(FALSE, 5)
content_area$packStart(vbox)
vbox$setBorderWidth(8)

spinner <- gtkSpinner()
vbox$add(spinner)

spinner$start()

}


# ---------------------------------------

stop.spinner <- function(){

if( exists( ".spin.win" )){
    .spin.win$Hide()
    .spin.win$Destroy()
}

}

