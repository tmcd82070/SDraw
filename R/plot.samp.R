#' @export plot.samp
#'  
#' @title Plot sample and frame
#'  
#' @description Plot the sample and optionally the frame, background image (terrain), and lattice 
#'  (if HAL sample). 
#'  
#' @param x A \code{SpatialPointsDataFrame} produced by an SDraw sampling function.  For example, 
#'  as produced by \code{sdraw(frame,n)}.  This object is a standard \code{SpatialPointsDataFrame} object
#'  with additional attributes that record the sampling design. 
#'  
#' @param frame The sample frame used to draw the sample contained in \code{x}. This is 
#' either a \code{SpatialPoints*}, \code{SpatialLines*}, or \code{SpatialPolygons*} object. 
#' 
#' @param lattice Boolean. Whether to plot the Halton Lattice if \code{x} is a HAL sample. 
#' 
#' @param hl.bbox Boolean. Whether to plot the Halton bounding box used to construct the 
#' Halton Lattice.  Assumes \code{x} is a HAL sample.  \code{lattice==TRUE} sets \code{hl.bbox == TRUE}.
#' 
#' @param background Either the background image (from a previous call to this routine) or 
#' the type of background image to plot.  If NULL (the default), no background image is plotted. 
#' If \code{background} is a string, it names the type of background image 
#' to retrieve from the Open Maps project.  Background types are 
#' any allowed by \code{\link{openmap}}  in the OpenStreetMap package.  Popular options are "osm", 
#' "esri", and "esri-topo".  When a background is retrieved, it slows execution and requires 
#' an internet connection.  When \code{background} is the returned value from a previous call 
#' to this function, it is plotted without retrieving a new image.  This will speed multiple 
#' iterations of plotting when the sample frame does not change (see Examples).
#' 
#'    
#' @return The set of tiles used in the background image is silently returned. If 
#' \code{background==NULL}, NULL is silently returned. 
#' 
#' @author Trent McDonald
#' 
#' @seealso \code{\link{sdraw}}
#' 
#' @examples 
#' data(WY)
#' samp <- sdraw(wY, 100, type="HAL", J=c(4,3))
#' plot.samp( samp, WY )
#'
#'


plot.samp <- function(x, frame, lattice=T, hl.bbox=F, background=NULL){

  #hl.bbox = whether to plot bounding box

  stype <- attr(x, "sample.type")         

  # Plot background if called for
  if( !is.null(background) ){
    
    require(OpenStreetMap)
    
    if(missing(frame)){
      fr <- x
    }
    
    if(length( grep("proj=longlat", proj4string(fr)) ) == 0){
      # Need to project frame to lat long to get bounding box
      tmp <- spTransform(fr, CRS("+init=epsg:4326"))
      bb <- bbox(tmp)
    } else {
      bb <- bbox(fr)
    }
    ULcoords <- c(bb[2,2], bb[1,1])
    BRcoords <- c(bb[2,1], bb[1,2])

    cat("Fetching background image...")
    
    # Read in desired background.  The coordinates to openmap must be lat long.
    #openMap <- openmap(ULcoords, BRcoords, type = "esri-topo")
    openMap <- openmap(ULcoords, BRcoords, type = background)
  
    # See the below link for a good site that gives visuals for the different backgrounds
    # and their corresponding 'type' arguments
    # http://www.r-bloggers.com/the-openstreetmap-package-opens-up/
    
    cat("Projecting background image...")
    # Transform to desired projection
    openMap <- openproj(openMap, projection = CRS(proj4string(fr))) 
    
    print(class(openMap))
  
    cat("\n")
    
    # Plot new layer (can be used with sp class objects)
    plot(openMap)
  }

    
  #   plot shape file
  if( !missing(frame) ){
    if( !is.null(background) ){
      if( inherits(frame, "SpatialPolygons") ){
        plot(frame, add=T, border="black")
      } else if (inherits(x, "SpatialLines") ){
        plot(frame, add=T, col="black", lwd=3)
      } else if (inherits(x, "SpatialPoints") ){
        plot(frame, add=T, col="black", pch=16)
      }
      
    } else {
      # What happens when no background
      my.cols <- rainbow(length(frame),start=3/6,end=4/6,alpha=0.5)
      if( inherits(frame, "SpatialPolygons") ){
          plot(frame, col=my.cols, border="white")#rainbow(length(x@polygons),start=3/6,end=4/6,alpha=0.5),border="white")                   # traditional R plot
      } else if (inherits(x, "SpatialLines") ){
          plot(frame, col=my.cols, lwd=3)
      } else if (inherits(x, "SpatialPoints") ){
          plot(frame, col=my.cols, pch=16)
      }
    }
  }


  
  if(stype == "HAL" & lattice){
    bb <- attr(x,"hl.bbox")
    bases <- attr(x,"bases")
    J <- attr(x,"J")
    
    for(d in 1:2){
      tmp2 <- bb[d,1] + (0:(bases[d]^J[d]))*(diff(bb[d,]))/(bases[d]^J[d])
      if( d == 1){
        for(i in 1:length(tmp2)){
          lines(rep(tmp2[i],2), bb[2,], col="blue")
        }
      } else{
        for(i in 1:length(tmp2)){
          lines(bb[1,], rep(tmp2[i],2), col="blue")
        }
      }
    }    
  }

  if( hl.bbox | lattice ){
    bb.col <- heat.colors(10)[4]
    bb <- attr(x,"hl.bbox")
    lines( rep(bb[1,1],2), bb[2,], col=bb.col )
    lines( rep(bb[1,2],2), bb[2,], col=bb.col )
    lines( bb[1,], rep(bb[2,1],2), col=bb.col )
    lines( bb[1,], rep(bb[2,2],2), col=bb.col )
  }

  if( !is.null(background) | !missing(frame) ){
    # plot is on screen, use points
    points(x, col="#FF0000FF", pch=16)
  } else {
    # No previous plot
    plot( x, col="#FF0000FF", pch=16)
  }

  
  
    
#     #   If the sample object exists, plot points on the map
#     if( !missing(samp) ){
#         stype <- attr(samp, "sample.type")        
#         
#         # Is this a stratified sample -> different legend
#         strat.var <- attr(samp, "strata.var")
#         
#         # Is this an unequal prob sample -> different legend
#         unequal.var <- attr(samp, "unequal.var")
#         
#         # Determine if this sample has an oversample
#         has.oversamp <- "pointType" %in% names(data.frame(samp))
#         if( has.oversamp )  has.oversamp <- length(unique(data.frame(samp)[,"pointType"])) > 1   # ?
#         
#         if( !is.null( strat.var )){
#           
#           # We have stratified sample
#           strat.ind <- data.frame(samp)[,strat.var]
#           strat.vals <- levels(factor(strat.ind))
#           strat.cols <- rainbow(length(strat.vals))
#           for(h in strat.vals){
#             points( samp[strat.ind == h,], pch=which(h==strat.vals)+14,cex=1.5,col=strat.cols[which(h==strat.vals)] )
#           }
#           legend("bottomleft", legend=strat.vals, pch=1:length(strat.vals)+14, col=strat.cols, title="Strata:")
#           # Note. oversample points in stratified samples, if they exist, are not plotted.
#         } else if( !is.null( unequal.var )){
#           
#           if( attr(samp, "alloc.type") != "Continuous" ){    # so, unequal prob or constant
#             
#             # We have categorical sample
#             unequal.ind <- data.frame(samp)[,c("mdcaty")] 
#             unequal.vals <- levels(factor(unequal.ind))
#             unequal.cols <- rainbow(length(unequal.vals))
#             for(h in unequal.vals){
#               points( samp[unequal.ind == h,], pch=which(h==unequal.vals)+14,cex=1.5,col=unequal.cols[which(h==unequal.vals)] )
#             }
#             legend("bottomleft", legend=unequal.vals, pch=1:length(unequal.vals)+14, col=unequal.cols, title="Categories:")
# 
#           } else if( attr(samp, "alloc.type") == "Continuous" ){     # continuous sample
#             samp <- samp[order(samp$mdcaty),]                        # make sure legend plots in range order
#             NS <- nrow(samp@data)                                    # get NS - the N sampled
#             if(NS <= 5){                                             # get one range.
#               ranges <- paste0("(",round(min(samp@data$mdcaty),0),",",round(max(samp@data$mdcaty),0),"]")
#               points(samp,pch=15,cex=1.5,col='blue')
#               legend("bottomleft",ranges,pch=19,col='blue')
#             } else if(NS >= 6 & NS <= 25){                           # get three ranges.    
#               ranges <- cut(samp@data$mdcaty,3,dig.lab=0)
#               unequal.ind <- as.numeric(ranges)
#               unequal.cols <- rainbow(3)
#               unequal.pch <- c(15,16,17)
#               for(h in unique(unequal.ind)){
#                 points(samp[unequal.ind == h,],pch=h+14,cex=1.5,col=unequal.cols[h])
#               }
#               legend("bottomleft",as.character(droplevels(unique(ranges))),pch=unequal.pch,col=unequal.cols, title="Ranges:")                            
#             } else {                                                 # get five ranges.  
#               ranges <- cut(samp@data$mdcaty,5,dig.lab=0)
#               unequal.ind <- as.numeric(ranges)
#               unequal.cols <- rainbow(5)
#               unequal.pch <- c(15,16,17,18,19)
#               for(h in unique(unequal.ind)){
#                 points(samp[unequal.ind == h,],pch=h+14,cex=1.5,col=unequal.cols[h])
#               }
#               legend("bottomleft",as.character(droplevels(unique(ranges))),pch=unequal.pch,col=unequal.cols, title="Ranges:")  
#             }
#           }
#         } else if( has.oversamp ){
#           #   There is some oversample
#           samp.ind <- data.frame(samp)[,"pointType"]
#           points( samp[samp.ind=="Sample",], pch=16 )
#           points( samp[samp.ind=="OverSample",], pch=1 )
#           legend("bottomleft", legend=paste(stype, c("sample", "over sample")), pch=c(16,1))
#         } else {
#           #   No oversample
#           points( samp, pch=16 )
#           legend("bottomleft", legend=paste(stype, "sample points"), pch=c(16))
#         }
# 
#     }
    

}


#plot.samp(samp, WY)