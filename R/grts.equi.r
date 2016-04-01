grts.equi <- function( shp, n, over.n=0 ){

  if( over.n == 0 ){
    Equaldsgn <- list(None = list(panel = c(Main = n),
                                  seltype = "Equal"))
    
  } else {
    Equaldsgn <- list(None = list(panel = c(Main = n),
                                seltype = "Equal",
                                over = over.n))
  }
  
  add.ID <- function(xx){
    # Make sure xx is a Spatial*DataFrame, and has at least one 
    # attribute. 
    geom <- geometry(xx)  # these are just the points, lines or polygons.  No data frame
    id <- row.names(xx)  # these are ID's of geometry unlist(lapply(xx@lines, slot, "ID"))
    if( length(id) == 0 ) id <- 1:length(geom) # pretty sure xx must have row.names, but just in case...
    if( "data" %in% slotNames(xx) ){
      # xx is a SpatialXDataFrame
      df <- xx@data
      if( nrow(df) == length(id) & ncol(df) > 0 ){
        df <- data.frame(geometryID=id, df, row.names = id)
      } else if( ncol(df) == 0){
        df <- data.frame(geometryID=id, row.names = id)
      } else {
        stop( "Number of rows in data frame does not equal number of 
              spatial objects in geometry. Check your input Spatial*DataFrame.")
      }
    } else {
      # xx is a SpatialX object (no data frame)
      df <- data.frame(geometryID=id, row.names = id)
    }
    if( inherits(geom, "SpatialLines")){
      xx <- SpatialLinesDataFrame( geom, data=df)
    } else if( inherits(geom, "SpatialPoints")){
      xx <- SpatialPointsDataFrame( geom, data=df)
    } else if( inherits(geom, "SpatialPolygons")){
      xx <- SpatialPolygonsDataFrame( geom, data=df)
    } else {
      stop(paste("Unknown spatial object class =", class(geom)))
    }
    xx
  }
  shp <- add.ID( shp )
  # At this point, shp has to be a SpatialXDataFrame with at least one attribute
  
  if( inherits(shp, "SpatialPoints") ){
      sframe.type = "finite"
      sframe.type.out = "point"
  } else if( inherits(shp, "SpatialLines") ){
      sframe.type = "linear"
      sframe.type.out = "line"
  } else if( inherits(shp, "SpatialPolygons") ){
      sframe.type = "area"
      sframe.type.out = "polygon"
  }


  Equalsites <- spsurvey::grts(design=Equaldsgn,
          DesignID='Site',
          type.frame=sframe.type,
          src.frame="sp.object",
          sp.object=shp,
          shapefile=FALSE)



  #   Convert to SpatialPointsDataFrame
  Equalsites <- as(Equalsites, "SpatialPointsDataFrame")

    #   Copy over the projection from the input spatial object
  proj4string(Equalsites) <- CRS(proj4string(shp))

  #   Add a column of sample/oversample for convieneince
  Equalsites$pointType <- c(rep("Sample",n), rep("OverSample",over.n))
  
  #   Drop all columns that spsurvey::grts added, except 'sampleID'.  None 
  #   of the dropped ones are important for equal probable designs
  keep.cols <- c("siteID", "pointType", names(shp))
  Equalsites <- Equalsites[, keep.cols ]
  
  #   Rename "siteID" to "sampleID" to be compatible with the rest of SDraw
  names(Equalsites)[ names(Equalsites) == "siteID" ] <- "sampleID"

  #   Store some extra attributes
  attr(Equalsites, "sp.object") <- deparse(substitute(shp))
  attr(Equalsites, "frame.type") <- sframe.type.out
  attr(Equalsites, "sample.type") <- "GRTS"
  attr(Equalsites, "n") <- n
  attr(Equalsites, "over.n") <- over.n
  
  Equalsites
}
