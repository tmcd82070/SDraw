#' @title bas.point - BAS of points
#' 
#' @description Draws a Balanced Acceptance Sample (BAS) 
#' from a 'sf' data frame containing POINT objects.
#' 
#' @details The BAS method for points computes the 
#' minimum distance between any two points in
#' \code{x} and places a small square (pixel) around each.  
#' Size of the square around each point is d/sqrt(2) 
#' on a side, where d is the minimum
#' distance between points. The method then selects 
#' a BAS sample from the set of polygons (i.e., squares) 
#' surrounding each point (see \code{\link{bas.polygon}}).  
#' The BAS method for polygons selects Halton
#' points until \code{n} are located inside the 
#' squares surrounding each point.  
#' When a square contains a Halton point, 
#' the official sample location
#' is the original point (center of the square), 
#' not the Halton point. 
#' 
#' @param x A 'sf' object containing POINT geometries
#' only. MULTIPOINT geometries are first converted to individual
#' points (silently) and sampled as individuals.
#' 
#' @param n Sample size.  Number of points to select 
#' from \code{x}.
#'
#' @return A 'sf' object containing POINT geometries, 
#' the selected points from \code{x}, in BAS order, with
#' additional columns and attributes. The additional attributes are:
#' \itemize{
#'   \item \code{sampleID}: A unique identifier for every 
#'   point in the sample.  This is
#'   the BAS order.   If BAS order is lost, 
#'   \code{return[} \code{order(} 
#'   \code{return$sampleID} \code{),]} will resort the 
#'   returned object into BAS order.
#'   
#'   \item \code{geometryID}: The ID of the original point 
#'   in \code{x}. The 
#'   ID of original points are \code{row.names(x)}. 
#' }
#'
#' Additional attributes of the output object, beyond those which 
#' make it a 'sf' object, are:
#' \itemize{
#'    \item \code{frame}: Name of the input sampling frame.
#'    \item \code{frame.type}: Type of resource in sampling frame. (i.e., "point").
#'    \item \code{sample.type}: Type of sample drawn. (i.e., "BAS").
#'    \item \code{random.start}: The random seed of the random-start Halton sequence 
#'    that produced the sample.  This is a vector of length 2 whose elements are 
#'    random integers between 0 and \code{\link{maxU}}. 
#'    The Halton point
#'    associated with \code{random.start}   
#'    falls inside one of the boxes (pixels) surrounding 
#'    points in \code{x}. (i.e., 
#'    \code{halton(1,2,random.start)} scaled by \code{x}'s square bounding box
#'    falls inside one of the pixels constructed for \code{x}). 
#'    The point associated with \code{random.start} is the 
#'    first in BAS order. 
#'    
#'    Note that subsequent points in the Halton sequence
#'    are  not guaranteed to fall inside a pixel surrounding
#'    a point in \code{x} (after scaling by bounding box).
#'    The BAS sample consists of points 
#'    associated with \code{random.start} and the 
#'    next \code{n-1} Halton points that fall inside a pixel
#'    of \code{x}. 
#'    
#'
#'    \item \code{bas.bbox}: The square bounding box surrounding
#'     \code{x} used to scale Halton points.  A scaled Halton 
#'     sequence of n points is \code{bas.bbox[,"min"] +} 
#'     \code{t(halton(n,2,random.start)) *} 
#'    \code{rep( max(diff(t(bas.bbox))), 2)}.
#'    
#'    
#' }
#' 
#' @author Trent McDonald
#' @seealso \code{\link{bas.polygon}}, \code{\link{bas.line}}, \code{\link{spsample}}
#' @keywords design survey
#' @examples
#' 
#' \dontrun{
#' bas.point( WA.cities, 100)
#' }
#'
#' @export 
#'  
bas.point <- function(x, n, verbose = FALSE){

  if( !(inherits(x, "sf"))  ) stop("Must call bas.point with a sf object.")
  
  geomType <- unique(sf::st_geometry_type(x))
  if( !all(geomType %in% c("POINT", "MULTIPOINT")) ){
    stop(paste0("Input object must contain only POINT or MULTIPOINT "
              , "geometries.  Found geometries "
              , paste(geomType, collapse = ", ")))
  }
  
  if( any(geomType %in% "MULTIPOINT") ){
    x <- sf::st_cast(x 
                   , to = "POINT")
  }

  N <- nrow(x)

  if( n > N ){
    warning("Sample size greater than frame size. Census will be taken.")
    n <- N
  }

  #   Find minimum distance between two points.  
  #   This is expensive, but necessary. 
  #   Use dist from stats package. Keep in mind this 
  #   is decimal degrees if x is lat long.
  D <- sf::st_distance( x ) # N X N
  diag(D) <- Inf
  d <- min(D)  # has units if projection has units (usually 'm')
  
  
  # error trap: check for duplicate coordinates 
  # if duplicates exist: pixels won't draw correctly
  unitsD <- units(d)
  if(d < units::set_units(.Machine$double.eps * 10
                        , value = unitsD
                        , mode = "standard")) {
    whichMin <- which.min(D)  # col major order
    colLoc <- whichMin %/% nrow(D)
    rowLoc <- whichMin - nrow(D) * (colLoc - 1)
    stop(paste0("Points on rows "
              , rowLoc
              , " and "
              , colLoc
              , " of 'x' appear to be duplicates. "
              , "Remove duplicates and try again."))
  }

  # Apply <some dude's> algorithm ----
  # This dude's algoritm moves points to the centroid 
  # of their Voronoi polygons to spread them out. 
  # It does this repeatedly until the distances 
  # between points is uniform-ish. This is expensive. 

  x <- x |> 
    tibble::rownames_to_column(var = ".rowname")
  xMod <- x
  loopNum <- 0
  repeat{
    loopNum <- loopNum + 1
    
    bb <- sf::st_bbox(xMod)
    dx <- max((bb["xmax"] - bb["xmin"]), (bb["ymax"] - bb["ymin"]))
    dx <- units::set_units(dx, value = unitsD, mode = "standard")
    dx <- dx + 2*(d / (2*sqrt(2)))  # b/c pixels go outside point bbox
    pixArea <-   N*(2*d / (2*sqrt(2)))^2
    p <- min(1, pixArea/(dx^2))
    
    if( verbose ){
      cat(paste0("Min distance = "
               , format(d)
               , "; Proportion of bbox covered = "
               , format(p)
               , "\n"))
    }
    if (p < 0.2){
      xMod <- spreadPts(xMod)
      D <- sf::st_distance( xMod ) # N X N
      diag(D) <- Inf
      d <- min(D)  
    } else {
      break
    }
    
    if(loopNum >= 5){
      break
    }
  }
  
  #   Make pixels around points 
  d <- d / (2*sqrt(2))   # reduce minimum so no pixels over lap
  if(verbose){
    cat(paste0("Creating pixels of size "
             , format(d)
             , "\n"))
  }
  pts <- sf::st_coordinates( xMod )
  pts <- units::set_units(pts
                        , value = unitsD
                        , mode = "standard")
  ll.x <- pts[,1] - d
  ll.y <- pts[,2] - d
  ur.x <- pts[,1] + d
  ur.y <- pts[,2] + d
  pid <- 1:length(ll.x)

  #   Make sf POLYGON object
  pixels <- data.frame(
      x = c(ll.x, ur.x, ur.x, ll.x, ll.x)
    , y = c(ll.y, ll.y, ur.y, ur.y, ll.y)
    , geometryID = c(pid, pid, pid, pid, pid)
  ) 
  pixels <- pixels[order(pixels$geometryID), ]
  pixels <- pixels |> 
    sfheaders::sf_polygon(
        x = "x"
      , y = "y"
      , polygon_id = "geometryID"
    ) 
  sf::st_crs(pixels) <- sf::st_crs(x)
  # So can get back to original points later.
  pixels <- pixels |> 
    dplyr::mutate(.rowname = xMod$.rowname)

  #   Now that we have polygons around points, call bas.polygon. 
  #   But, points can be sampled more than once when the Halton sequence comes 
  #   back around.  To deal with this, take an oversample and keep going 
  #   until we get n distinct points. 
  samp <- NULL
  n.iter <- n
  ran.start <- NULL
  repeat{
    # Geometry ID returned by bas.polygon is correct because we assigned row.names when we made pixels
    nAsked <- min(N, n.iter * (1 + n.iter/N))
    samp2 <- bas_polygon( x = pixels
                        , n = round(nAsked)
                        , halton.start = ran.start)
    ran.start <- attr(samp2, "random.start") # in case we loop again
    samp <- samp |> 
      dplyr::bind_rows(samp2)  

    dups <- duplicated(samp$geometryRowID)
    
    if(verbose){
      cat(paste0(
        "Asked for "
        , nAsked
        , " points from "
        , nrow(pixels) 
        , ". Got "
        , nrow(samp2)
        , ". Now have "
        , nrow(samp)
        , " points, of which "
        , sum(dups)
        , " are duplicates.\n"
      ))
    }
    
    if( sum(!dups) >= n ){
      # done
      samp <- samp |>
        dplyr::filter(!dups) |> 
        dplyr::slice(1:n)
      break
    }

    # Prep for next iteration, cut down sample size and frame.
    n.iter <- n - sum(!dups)
    pixels <- pixels |> 
      dplyr::filter( !(.rowname %in% samp$.rowname) )
  }

  # Final manipulations: merge in original points ----

  samp <- samp |>
    sf::st_drop_geometry() |> 
    dplyr::select(.rowname, geometryID, sampleID) |> 
    dplyr::left_join(x, by = ".rowname") |> 
    sf::st_set_geometry(attr(x,"sf_column")) |> 
    tibble::column_to_rownames(var = ".rowname")
  
  attr(samp, "frame") <- deparse(substitute(x))
  attr(samp, "frame.type") <- "point"
  attr(samp, "sample.type") <- "BAS"
  attr(samp, "random.start") <- ran.start
  # attr(samp, "bas.bbox") <- bas.bbox
  
  samp

}
