getSpFrame <- function( filename, dir ){

    if( exists(filename) ){
        #   The shapefile has already been read, and it is laying around.  No need to re-read.
        shp <- get(filename)
    } else {
        #   The shapefile is not laying around.  Read it.

        #   Write the command to console and histry file
        timestamp( paste( filename, "<- readOGR(", dir, ",", filename, ")"), prefix="", suffix=" ## SDraw")
        
        shp <- readShape(dir, filename)  # a wrapper for readOGR

        #   Save a copy for future use, but save in the Package space because 
        #   Packages cannot alter the .GlobalEnv
        SDrawPackageSpace <- as.environment( "package:SDraw" )
        assign(filename, shp, pos=SDrawPackageSpace)  
    }
	#this is another comment

    shp
}
