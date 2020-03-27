#' @export plotSample
#' 
#' @title Plot sample and frame
#'  
#' @description Plot the sample and optionally the frame, background image (terrain), and lattice 
#'  (if HAL sample). 
#'  
#' @param x A \code{SpatialPointsDataFrame} produced by an SDraw sampling function.  For example, 
#'  as produced by \code{sdraw(frame,n)}.  This object is a standard \code{Spatial} \code{Polygons}\code{DataFrame} object
#'  with additional attributes that record the sampling design. 
#'  
#' @param frame The sample frame used to draw the sample contained in \code{x}. This is 
#' either a \code{SpatialPoints*}, \code{SpatialLines*}, or \code{SpatialPolygons*} object. 
#' 
#' @param lattice Logical. Whether to plot the Halton Lattice if \code{x} is a HAL sample. 
#' 
#' @param bbox Logical. Whether to plot the bounding box if the sample 
#' has a bounding box attribute. This generally means \code{x} 
#' is a HAL or BAS sample.  \code{lattice==TRUE} sets \code{bbox == TRUE}.
#' 
#' @param add Logical. Whether to add to an existing plot.  See Examples.
#' 
#' @param poly.fill Logical. Whether to fill polygons (TRUE) or leave them transparent (FALSE). 
#' Only applies to \code{SpatialPolygon*} frames. 
#' 
#'    
#' @return Nothing (NULL is invisibly returned)
#' 
#' @author Trent McDonald
#' 
#' @seealso \code{\link{sdraw}}
#' 
#' @examples
#' data(WY)
#' samp <- sdraw(WY, 100, type="HIP", J=c(4,3))
#' plotSample( samp, WY )
#' plotSample( samp, WY, lattice=TRUE )
#'
#' # A map-like background under frame and sample ----
#' # Requires 'OpenStreetMap' package and internet connection
#' \dontrun{
#' library(OpenStreetMap)
#' # 1:convert to Lat-Long
#' WY.ll <- spTransform(WY, CRS("+init=epsg:4326"))
#' # 2:Specify bounding box for OpenStreetMap
#' bb.openmap <- bbox(WY.ll)
#' ULcoords <- c(bb.openmap[2,2], bb.openmap[1,1])
#' BRcoords <- c(bb.openmap[2,1], bb.openmap[1,2])
#' # 3:Fetch image (see 'openmap' help for 'type' parameter)
#' openMap <- OpenStreetMap::openmap(ULcoords, BRcoords, type = "esri")
#' # 4:Project background image to original coordinate system
#' openMap <- OpenStreetMap::openproj(openMap, projection = CRS(proj4string(WY))) 
#' # 5:plot background
#' plot(openMap)
#' # 6:plot frame and sample
#' plotSample(samp, WY, add=TRUE, poly.fill=FALSE)
#' }


plotSample <- function(x, frame, lattice=FALSE, bbox=FALSE, add=FALSE, poly.fill=TRUE){


  stype <- attr(x, "sample.type")         
  wps <- proj4string(x)
  baltype <- attr(x,"balance")
  if(is.null(baltype)){
    baltype = "2d"
  }
  ftype <- attr(x,"frame.type")
  
  
  if( (bbox | lattice) & (stype == "HAL") & !((ftype=="line") & (baltype=="1d"))){
    bb <- attr(x,"hl.bbox")
  } else if ((bbox | lattice) & (stype == "BAS") & !((ftype=="line") & (baltype=="1d"))){
    bb <- attr(x,"bas.bbox")
  } else {
    # use bb tight around x or frame
    
    if(missing(frame)){
      bb <- bbox(x)
    } else {
      bb <- bbox(frame)
    }
  }
  bb.sp <- SpatialPoints(t(bb), proj4string = CRS(wps))
  plot(bb.sp, col=0, add=add) # invisible points to extablish plot extent
  
  #   plot frame
  if( !missing(frame) ){
      my.cols <- rainbow(length(frame),start=3/6,end=4/6,alpha=0.5)
      if( inherits(frame, "SpatialPolygons") ){
        if(poly.fill){
          plot(frame, add=TRUE, col=my.cols, border=my.cols)
        } else {
          plot(frame, add=TRUE, border="black")
        }
      } else if (inherits(x, "SpatialLines") ){
          plot(frame, add=TRUE, col=my.cols, lwd=3)
      } else if (inherits(x, "SpatialPoints") ){
          plot(frame, add=TRUE, col=my.cols, pch=16)
      }
  }
  
  if(stype == "HAL" & lattice ){
    if( !(ftype =="line" & baltype == "1d")){
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
  }

  if( (bbox | lattice) & (stype=="HAL" | stype=="BAS") ){
    if( !(ftype =="line" & baltype == "1d")){
      bb.col <- "#FF6D00FF"
      lines( rep(bb[1,1],2), bb[2,], col=bb.col )
      lines( rep(bb[1,2],2), bb[2,], col=bb.col )
      lines( bb[1,], rep(bb[2,1],2), col=bb.col )
      lines( bb[1,], rep(bb[2,2],2), col=bb.col )
    } else {
      warning("bbox not plotted. bbox is not 2-dimensional for 1D balanced line samples")
    }
  }

  points(x, col="#FF0000FF", pch=16)

  invisible(NULL)
  
}

