halton.frame <- function( hl.points, order.name="frame.order" ){
  #
  # Make a Halton frame, from a set of Halton points that have 
  # their Halton indicies attached. This function takes the halton.index and 
  # adds random Halton cycles to points in same Halton box, then sorts by these
  # adjusted indicies. 
  #
  # Input: 
  #   hl.points = the output of halton.indicies.  This is either a data frame or a 
  #     SpatialPointsDataFrame object.  The data frame, or data frame of the SpatialPointsDataFrame, 
  #     must contain a column with the Halton indicies, which is assumed to be named 
  #     attr(hl.points, "index.name").  By default this column names is "halton.index".  
  #     Each row of the data frame is a point 
  #     in the frame, and column attr(hl.points, "index.name") is the Halton ID of the box the point falls in.
  #     We cannot have a SpatialPoints object here.  If SpatialPoints are passed in, there must
  #     be at least one column in the data slot (i.e., the halton.index column).
  #   order.name = name of the column in the output object by which the frame should be sorted
  #     to produce the proper sampling order. This is saved as an attribute of the output object.
  #
  # Value: 
  #   A data frame like hl.points, but sorted by modified halton order, and ready to sample
  
  # Extract halton index (should probably check for presence first, and exit nicely)
  if( regexpr("SpatialPointsDataFrame", class(hl.points)) > 0 ){
    df <- data.frame(hl.points)  # use data.frame here because we want the coordinates in df
    is.points <- TRUE
  } else {
    df <- hl.points
    is.points <- FALSE
  }  

  halton.index.name <- attr(hl.points, "index.name")
  
  if( !(halton.index.name %in% names(df)) ) stop(paste(halton.index.name, "undefined or column not found in data frame."))
  hl.index <- df[,halton.index.name]

  # Find order of indicies and save for later
  ord.hl <- order(hl.index)  

  # Sort data frame by halton index.  This has to happen regardless whether theres 1 or multiple 
  # points per box.  If multiple points per box, this puts all points in same box side-by-side in frame.
  df <- df[ord.hl,]
  
  # Compute maximum number of points in any Halton box.  This is well defined because halton indecies are integers.
  # It would be interesting to inspect the distribution of points per box here. e.g., hist(hl.index)
  mx.count <- max( table( hl.index ))
  
  
  if( mx.count > 1 ){
    # Find points that have more than one other point in the same box, create cycle variable 
    hl.index <- hl.index[ ord.hl ]   # sort indicies
    cycle <- unlist(tapply(rep(1,length(hl.index)), hl.index, 
                           function(x,mx){
                             sample(1:mx,size=length(x),replace=F)
                           }, 
                           mx=mx.count))

    # Add cycle to hl.index to make it easy to sort outside this routine
    hl.digits <- floor(log10(max(hl.index))) + 1
    cycle.hl.index <- cycle * 10^hl.digits + hl.index
    df$zzzzframe.order <- cycle.hl.index 
  
    # Sort data frame by cycle, hl.index.  This puts points in same box at least one cycle away from each other in frame.
    ord <- order( cycle.hl.index )
    df <- df[ord,]
  } else {
    df$zzzzframe.order <- hl.index[ ord.hl ]  # a duplicate copy of halton.index when one point per box
  }
  names(df)[names(df)=="zzzzframe.order"] <- order.name
  
  # Convert to SpatialPOintsDataFrame if input was same
  if( is.points ){
    # Return a SpatialPoints* object
    cnames <- coordnames(hl.points)
    df.nocoords <- df[, -which(names(df) %in% cnames)]
    df <- SpatialPointsDataFrame(df[,cnames], data=df.nocoords, 
                                 proj4string=CRS(proj4string(hl.points)))
  } 
  
  # Add attributes
  attr(df,"J") <- attr(hl.points, "J")
  attr(df,"eta") <- attr(hl.points, "eta")
  attr(df,"bases") <- attr(hl.points, "bases")
  attr(df,"hl.bbox") <- attr(hl.points, "hl.bbox")
  attr(df,"triangular") <- attr(hl.points, "trianglular")
  attr(df,"index.name") <- halton.index.name
  attr(df,"order.name") <- order.name
  
  df
}


# -----------------------------
# Examples

##  Test with SpatialPolygon* objects
# tmp <- halton.lattice.polygon(WA.utm, J=c(7,4), eta=c(2,2))
# tmp <- halton.lattice.polygon(WA.utm.nodata, J=c(6,3), eta=c(1,1))
# 
# 
# tmp3 <- halton.frame(tmp2)
# #
##  Test with multiple points in each box for SpatialPolygon* objects (i.e., eta > 1)
# tmp <- halton.lattice.polygon(WA.utm, J=c(6,3), eta=c(2,2))
# tmp <- halton.lattice.polygon(WA.utm.nodata, j=c(6,3), eta=c(2,2))
# tmp2 <- halton.indicies(tmp)
# 
# tmp3 <- halton.frame(tmp2)
#
# Some interesting plots
# plot(WA.utm)
# points(tmp)
# points(tmp3[tmp3$halton.index==2,], pch=16, col=1:4)
# points(tmp3[tmp3$halton.index==3,], pch=16, col=1:4)
#
# points(tmp3[20000 <= tmp3$frame.order & tmp3$frame.order<=29999,], pch=16, col=2)
# points(tmp3[30000 <= tmp3$frame.order & tmp3$frame.order<=39999,], pch=16, col=3)
# points(tmp3[40000 <= tmp3$frame.order & tmp3$frame.order<=49999,], pch=16, col=4)
# 
#
## Test with data frames
# tmp <- halton.lattice(bbox(WA.utm[3,]), J=c(6,3), eta=c(2,1))
# tmp2 <- halton.indicies(tmp)
# tmp3 <- halton.frame(tmp2)

# #plot(WA.utm[3,], xlim=c(480118.3, 515610.1), ylim=c(5230959 ,5265726))
# plot(WA.utm[3,])
# 
# points(tmp3[1:4, ], pch=16,col=rainbow(max(tmp2$halton.index))[i])

## Test on unit box to see indicies
# J <- c(3,2)
# eta <- c(1,1)
# b <- c(2,3)
# tmp <- halton.lattice(J=J, eta=eta, triangular=F )
# tmp2 <- halton.indicies(tmp)
# # 
# # 
# plot(c(0,1),c(0,1),type="n")
# 
# for( i in J[1]:1){
#   abline(v=(0:b[1]^i)/b[1]^i, lwd=J[1]+1-i, col=i)
# }
# for( i in J[2]:1){
#   abline(h=(0:b[2]^i)/b[2]^i, lwd=J[2]+1-i, col=i)
# }
# points(tmp[,1],tmp[,2],  col=6, pch=16)
# 
# for( i in 1:nrow(tmp2)){
#   text(tmp2$d1[i],tmp2$d2[i], tmp2$halton.index[i], adj=1, col="black")  
# }

