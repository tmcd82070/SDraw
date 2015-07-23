draw.hal <- function(n,over.n,fn,dir){
#
#   draw a HAL sample.
#

#   Check whether the frame has been read already, and the sp object is laying around. 
    shp <- getSpFrame( fn, dir )

#   Draw the HAL sample.  hal() handles points, lines, and polygons
    samp <- hal(n+over.n,shp)

    
    #   Add a column of sample/oversample for convieneince
    samp$pointType <- c(rep("Sample",n), rep("OverSample",over.n))



    #   Store some attributes
    attr(samp, "sample.type") <- "HAL"
    attr(samp, "n") <- n
    attr(samp, "over.n") <- over.n
    attr(samp, "frame.type") <- attr(shp,"type")
    attr(samp, "sp.object") <- fn


    samp
}
