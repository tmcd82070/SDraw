readShape <- function(input.dir, layername){
#
#   This is a wrapper for readOGR
#


    cat("Reading shapefile...This takes a while if the file is big...\n")

    #   Read the shape file, if it does not exist, readOGR says so.
    shp <- readOGR( input.dir, layername )  # This requires library rgdal

    cat("Success reading shapefile\n")

    shp
}
