#' @export sss.line
#' 
#' @title Draw a Simple Systematic Sample (SSS) from a linear resource. 
#' 
#' @description Draws a systematic sample from a \code{LINESTRING} or 
#' \code{MULTILINESTRING} objects.  
#' 
#' @details If \code{x} contains multiple lines, the lines are amalgamated before
#' sampling.   Conceptually, the amalgamated line contains all lines in \code{x} 
#' after "stretching" straight and laying them end-to-end in order 
#' of appearance in \code{x}.
#' The simple systematic sample is drawn from the amalgamated line
#' and sample points are mapped back to the original 2-dimensional line. 
#' 
#' Note that spacing between sample points is enforced on the amalgamated 
#' line, and may not look correct if the lines loop back on themselves. 
#' For example, consider a line tracing a circle.  The spacing between 
#' the first and last sample point along the circle will not be the 
#' prescribed \code{spacing} because the circle starts between them. 
#' Spacing of all other points (2 to n-1) will be as requested. 
#'
#' @section Mixed Geometries:
#' Mixed geometries of MULTILINESTRING and LINESTRING are allowed; 
#' however, all MULTILINESTRING geometries are laid at the end of the 
#' amalgamated line and hence are out of order relative to the 
#' order of \code{x}. If you desire the same order of lines in the 
#' amalgamated line as in \code{x}, convert all MULTILINESTRING geometries
#' to LINESTRING \emph{a priori}, prior to calling this routine.
#' 
#' @param n Sample size.  Number of points to draw from the set of all lines
#' contained in \code{x}.  Specification of \code{n} takes precedence 
#' over specification of \code{spacing}.
#' 
#' @param x A \code{sf} object containing \code{LINESTRING} or 
#' \code{MULTILINESTRING} geometries, or both. 
#' 
#' @param spacing Assuming \code{n} is not given, this is the distance 
#' between sample points on the amalgamated line 
#' in \code{x}. For example, if \code{x} is projected 
#' in UTM coordinates and \code{spacing=100}, the returned sample has one point 
#' every 100 meters along the amalgamated line in \code{x}. Keep in mind that the start
#' of line i+1 may not coincide with the end of line i.  2-dimensional 
#' distances between
#' sample points will not, in general, equal \code{spacing}.   
#' 
#' @param random.start Whether to start the sequence of points at a 
#' random place.  If \code{TRUE}, a random uniform variate is selected 
#' between 0 and either \code{spacing} or (length/\code{n}) and the first 
#' location is placed at that location along the line.  Subsequent points occur
#' every \code{spacing} units along the lines.  If \code{random.start==FALSE}, 
#' the first sample point occurs at 0 (first vertex of the lines).
#' 
#' @return A \code{sf} data frame containing POINT geometries. 
#' The \code{sf} POINT geometries are equally spaced along the 
#' amalgamated line, except as noted above.  Points on line 1 
#' appear first, those on line 2 
#' second, etc. Attributes of the sample points are 
#' \itemize{
#'   \item \code{sampleID}: A unique identifier for every sample point.  
#'   \code{sampleID} starts with 1 at the first point and 
#'   increments by one for each.  \code{sampleID} orders 
#'   sample points along the amalgamated line.
#'   \item \code{geometryID}: The ID of the geometries in \code{x} on which each 
#'   sample point falls.  The geometry
#'   IDs in \code{x} are \code{rownames(x)}. 
#'   \item Any attributes of the original lines (in \code{x}) on which each sample 
#'   point falls.
#' }
#'
#' 
#' The return has additional attributes that makes it 
#' an \code{SDraw} sample.  The additional attributes are:
#' \itemize{
#'    \item \code{frame}: Name of the input sampling frame.
#'    \item \code{frame.type}: Type of resource in sampling frame. (i.e., "line").
#'    \item \code{sample.type}: Type of sample drawn. (i.e., "SSS").
#'    \item \code{sample.spacing}: The spacing between sample points along the 
#'    amalgamated line. This is the input \code{spacing} parameter if specified,
#'    or is computed as (length/n) if \code{n} is specified.
#'    \item \code{random.start}: The random start of the systematic sample.  NA 
#'    corresponds to no random start.
#' }
#' 
#' @author Trent McDonald
#' @seealso \code{\link{sss.polygon}}, \code{\link{sss.point}}, \code{\link{sdraw}}
#' @keywords design survey
#' @examples
#' 
#' # Draw fixed number of equi-distant points
#' HI.samp <- sss.line( HI.coast, 100 )   
#' plot( HI.coast, col=rainbow(length(HI.coast)) )
#' points( HI.samp, col="red", pch=16 )
#' 
#' # Draw points every 20 km along Hawaii's coastline
#' HI.samp <- sss.line( HI.coast, spacing=20000 )   
#' plot( HI.coast, col=rainbow(length(HI.coast)) )
#' points( HI.samp, col="red", pch=16 )
#' 
#' # Inspect attributes of points with HI.samp@data
#' 
sss.line <- function(x, n, spacing, random.start=TRUE){

  if( !(inherits(x, "sf")) ) stop("Must call sss.line with a sf object.")

  if(!missing(n) & !missing(spacing)) warning("n and spacing were both specified in sss.line.  n is being used.")

  # check input parameters
  if( !missing(n)){
    if (!is.finite(n) || n < 1) return(NULL)
  }
  

  
  # Main code ========================================================
  # Add rownames as a column so that the stay constant throughout the rest of this
  x <- tibble::rownames_to_column(x, var = "geometryID")

  # Check for non-LINESTRING geometries (e.g., point or polygon)  
  geoTypes <- sf::st_geometry_type(x)
  if( !all(geoTypes %in% c("MULTILINESTRING", "LINESTRING")) ){
    warnings(paste("Dropping"
       , sum(!(geoTypes %in% c("MULTILINESTRING", "LINESTRING")))
       , "non-LINESTRING geometries."))
    x <- x %>% 
      dplyr::filter(geoTypes %in% c("MULTILINESTRING", "LINESTRING"))
    geoTypes <- sf::st_geometry_type(x)
  }
  
  # Check for MULTILINESTRINGS interspersed within LINESTRINGS
  if( any(geoTypes == "MULTILINESTRING") ){
    # cast multilines to lines; do this because st_coordinates does 
    # not accept multi-geometries and casting straight 
    # to LINESTRING of a data frame containing MULTILINESTRING and LINESTRING 
    # keeps only the first line in the MULTILINESTRING. Weird behavior.
    multiLines <- suppressWarnings(  # warning about constant attributes
        x %>% 
        dplyr::filter( geoTypes == "MULTILINESTRING") %>% 
        sf::st_cast( to = "LINESTRING" ) %>% 
        dplyr::mutate( geometryID = paste0("multiline_"
                                           , geometryID
                                           , "_"
                                           , dplyr::row_number()
                                           ))
    )
    x <- x %>%
      dplyr::filter( geoTypes == "LINESTRING" ) %>% 
      dplyr::bind_rows(multiLines)
  }
  
  # Prep ----
  
  # Make matrix of coordinates; columns are X, Y, L1; L1 = row number of line in x
  # This amalgamates the line by listing coordinates in order of x
  mline <- sf::st_coordinates(x) %>% 
    tibble::as_tibble()
  
  # We need the length index, i.e., "t" in parameterized line [x(t), y(t)].
  # Get "t" by making individual line segments, then computing cumulative length.
  points1 <- mline %>%  
    dplyr::group_by(L1) %>% 
    dplyr::slice(-dplyr::n()) %>% 
    tibble::rowid_to_column(var = "rowID")
  points2 <- mline %>% 
    dplyr::group_by(L1) %>% 
    dplyr::slice(-1) %>% 
    tibble::rowid_to_column(var = "rowID")
  segmentedLine <- points1 %>% 
    dplyr::bind_rows( points2 ) %>% 
    dplyr::arrange(rowID) %>% 
    sfheaders::sf_linestring(
        x = "X"
      , y = "Y"
      , linestring_id = "rowID"
      , keep = TRUE
    ) 
  sf::st_crs(segmentedLine) <- sf::st_crs(x)
  # Here: nrow(segmentedLine) = nrow(mline) - (num L1 groups)
  
  segmentedLine <- segmentedLine %>%  
    dplyr::mutate(segLen = sf::st_length(.)) %>% 
    dplyr::group_by(L1) %>%
    dplyr::mutate(ord = seq(from = 2, to = dplyr::n() + 1, by = 1)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(cumLength = cumsum(segLen)) 
  
  # We need to duplicate a segment at the beginning of every line, 
  # which really duplicates the first vertex of every L1 group.
  # The "lag" of cumlength is the key here.
  
  zero <- units::set_units(0, units(segmentedLine$segLen), mode = "standard")
  firstSeg <- segmentedLine %>% 
    dplyr::mutate(cumLength = dplyr::lag(cumLength
                                         , n = 1
                                         , default = zero)
                  , ord = 1) %>% 
    dplyr::group_by(L1) %>% 
    dplyr::slice(1)
  # Here: nrow(firstSeg) = (num L1 groups)
  

  segmentedLine <- segmentedLine %>% 
    bind_rows(firstSeg) %>% 
    dplyr::arrange(L1, ord)
  # Now: nrow(segmentedLine) = nrow(mline)
  
  # We can rbind the cumLength column onto vertices now
  mline <- mline %>% 
    dplyr::mutate(cumLength = segmentedLine$cumLength) %>% 
    tibble::rowid_to_column(var = "rowID")
  
  # Sample ----
  # Figure out l.out  = a sequence of lengths along parameterized line ("l","x","y")
  tot.len <- segmentedLine$cumLength[nrow(segmentedLine)] # also last line of mline; also sum(lengths(x))
  if(!missing(n)){
    # figure out spacing. Want n equally space points along 
    # entire length of line.
    spacing <- tot.len / n
  } 
  l.out <-  seq(from = zero
                , to = tot.len-spacing
                , by = spacing)

  if( random.start ){
    m <- runif(1, 0, spacing)
    m <- units::set_units(m, units(spacing), mode = "standard")
    l.out <- l.out + m
  } else {
    m <- NA
  }

  # Extract or compute points on the parameterized line, and indices (tt)
  aprox <- function(x,y,xout){ 
    xout <- xout[min(x) <= xout & xout <= max(x)]
    yout <- approx(x = x, y = y, xout = xout)$y 
    yout
  }
  x.out <- mline %>% 
    group_by(L1) %>% 
    summarise(x = aprox(cumLength, X, l.out)
              , y = aprox(cumLength, Y, l.out)
              , vertexWithinLine = ceiling(aprox(cumLength, rowID, l.out)))

  # Project back to 2-D ----
  samp <- sfheaders::sf_point(x.out
                            , x = "x"
                            , y = "y"
                            , keep = TRUE
                        )
  sf::st_crs(samp) <- sf::st_crs(x)

  # Add sampleID = order ----
  samp <- samp %>% 
    dplyr::mutate(sampleID = dplyr::row_number()) %>% 
    dplyr::select(sampleID, dplyr::everything())
  
  # Attach any attributes that came in with X ----
  samp <- samp %>% 
    dplyr::bind_cols(sf::st_drop_geometry(x)[samp$L1,])
  
  #   Add additional attributes
  attr(samp, "frame") <- deparse(substitute(x))
  attr(samp, "frame.type") <- "line"
  attr(samp, "sample.type") <- "SSS"
  attr(samp, "sample.spacing") <- spacing
  attr(samp, "random.start") <- m
  
  samp
}
