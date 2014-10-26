getSpFrame <- function( filename ){

    if( exists(filename) ){
        #   The shapefile has already been read, and it is laying around.  No need to re-read.
        shp <- get(filename)
    } else {
        #   The shapefile is not laying around.  Read it.
        input.dir <- get(".INPUT.DIR")
        
        #   Write the command to console and histry file
        timestamp( paste( filename, "<- readOGR(", input.dir, ",", filename, ")"), prefix="", suffix=" ## SDraw")
        
        shp <- readShape(input.dir, filename)  # a wrapper for readOGR

        assign(filename, shp, pos=.GlobalEnv)  # save a copy for future use
    }
	#this is another comment

    shp
}
