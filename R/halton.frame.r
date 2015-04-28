halton.frame <- function( hl.points ){
  #
  # Make a Halton frame, from a set of Halton points that have 
  # their Halton indicies attached. This function takes the halton.index and 
  # adds random Halton cycles to points in same Halton box, then sorts by these
  # adjusted indicies. 
  #
  # Input: 
  #   hl.points = the output of halton.indicies.  This is either a data frame or a 
  #     SpatialPointsDataFrame object.  The data frame, or data frame of the SpatialPointsDataFrame, 
  #     must contain a column named "halton.index".  Each row of the data frame is a point 
  #     in the frame, and "halton.index" is the Halton ID of the box the point falls in.
  #
  # Value: 
  #   A data frame like hl.points, but sorted by modified halton order, and ready to sample
  
  # Extract halton index (should probably check for presence first, and exit nicely)
  hl.index <- data.frame(hl.points)[,"halton.index"]
  
  # Compute maximum number of points in any Halton box.  This is well defined because halton indecies are integers.
  mx.count <- max( table( hl.index ))
  
  # Find points that have more than one other point in the same box. 
  here!!!
  
  
  
}