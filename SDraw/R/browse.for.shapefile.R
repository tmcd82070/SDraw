browse.for.shapefile <- function(x, dat){
#
#

    filt <- rbind( c( "All files (*.*)", "*.*"), c("Shapefiles (*.shp)", "*.shp") )

    #   Prompt the user for file name
    dialog <- gtkFileChooserDialog("Open file...", dat$parent.window, "open",
                                   "gtk-open", GtkResponseType["accept"],
                                   "gtk-cancel", GtkResponseType["cancel"])

    #   run dialog
    if (dialog$run() == GtkResponseType["accept"]) {

        path.n.filename <- extractPathFilename( dialog$getFilename() )
        dialog$destroy()

        filename <- path.n.filename$file
        path <- path.n.filename$path

        dat$shape.in.entry$setText(filename)
        assign(".INPUT.DIR", path, env=.GlobalEnv )
    } else {
        dialog$destroy()
    }

}
