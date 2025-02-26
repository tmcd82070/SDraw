#' @title bas_polygon
#'
#' @description Select a BAS sample from a data frame
#' containing 'sf' POLYGON objects.
#'
#' @param x A 'sf' object containing POLYGON geometries
#'
#' @param n Desired sample size
#' 
#' @param halton.start A 1XD matrix of random numbers between 
#' 1 and \code{SDraw::maxU()}, where D is the number of dimensions.
#' This parameter controls where the random Halton sequence starts.
#' If NULL, random uniform numbers are used and the Halton sequence
#' starts in a random place, which is returned as an attribute
#' (\code{attr(return, "random.start")}.  Otherwise, sampling
#' starts at the Halton point specified by \code{halton.start}.
#'
#' @return A 'sf' object containing POINT geometries in BAS 
#' order.
#' Points are sample locations inside the polygons of the input 
#' polygons.
#'
#'
#' @export
bas_polygon <- function (x, n, halton.start = NULL){
    if (n < 1) {
        n <- 1
        warning("Sample size less than one has been reset to 1")
    }
    bb <- sf::st_bbox(x)
    area <- sf::st_area(x)
    dx <- (bb["xmax"] - bb["xmin"])
    dy <- (bb["ymax"] - bb["ymin"])
    p <- min(1, sum(area)/max( dx, dy )^2)
    my.dim <- 2
    x$geometryRowID <- 1:nrow(x)
    crs.obj <- sf::st_crs(x)
    q <- 1 - p
    z <- qnorm(0.9)
    n.init <- (1/p) + (q * z * z/(2 * p)) + (z/p) * sqrt(z *
        z * q * q/4 + q * 1)
    n.init <- ceiling(n.init)
    if (exists("maxU", envir = .GlobalEnv, mode = "function")) {
        max.u <- get("maxU", envir = .GlobalEnv, mode = "function")
        max.u <- max.u()
    }
    else {
        max.u <- SDraw::maxU()
    }
    d.box <- rep(max(dx,dy), 2)
    xl <- bb["xmin"]
    xr <- bb["xmin"] + d.box[1]
    yl <- bb["ymin"]
    yu <- bb["ymin"] + d.box[2]
    bas.bbox <- matrix(c(xl, yl, xr, yu), 2)
    dimnames(bas.bbox) <- list(c("x","y"), c("min", "max"))
    curLoop <- 0
    repeat {
        curLoop <- curLoop + 1
        if( is.null(halton.start) ){
          m <- floor(runif(n.init * my.dim, min = 0, max = max.u +
              1))
          m <- matrix(m, n.init, my.dim)
          halt.samp <- matrix(NA, n.init, my.dim)
          for (i in 1:n.init) {
              halt.samp[i, ] <- SDraw::halton(1, dim = my.dim, start = m[i,
                  ])
          }
        } else {
          halt.samp <- SDraw::halton(curLoop*n.init, dim = my.dim, start = halton.start)
          halt.samp <- halt.samp[((curLoop-1)*n.init + 1):(curLoop*n.init),]
        }
        halt.samp <- bas.bbox[, "min"] + t(halt.samp) * d.box
        halt.samp <- t(halt.samp)
        halt.pts <- sf::st_sfc(
            lapply(1:nrow(halt.samp), function(x,h)
                { sf::st_point(h[x,]) }, h=halt.samp),
            crs = sf::st_crs(x))
        in.poly <- sf::st_intersects(x, halt.pts)
        in.poly <- do.call(c,in.poly)
        if (length(in.poly) > 0){
            break
        }
    }
    which.pt.in <- in.poly[1]
    m <- m[which.pt.in, ]
    halt.pts <- halt.pts[which.pt.in, ]
    q <- 1 - p
    z <- qnorm(0.99)
    halt.start <- m
    n.cur <- n
    repeat {
        n.init <- (n.cur/p) + (q * z * z/(2 * p)) + (z/p) * sqrt(z *
            z * q * q/4 + q * n.cur)
        n.init <- ceiling(n.init)
        halt.samp <- SDraw::halton(n.init, dim = my.dim, start = m +
            1)
        halt.samp <- bas.bbox[, "min"] + t(halt.samp) * d.box
        halt.samp <- t(halt.samp)
        halt.samp <- sf::st_sfc(
            lapply(1:nrow(halt.samp), function(x,h)
            { sf::st_point(h[x,]) }, h=halt.samp),
            crs = sf::st_crs(x))
        halt.pts <- c(halt.pts, halt.samp)

        in.poly <- sf::st_intersects(x, halt.pts)
        in.poly <- do.call(rbind,lapply(1:length(in.poly),
                                    function(x,h){
                                        if(length(h[[x]]) > 0){
                                            data.frame(xInd = x,hInd = h[[x]])
                                        } else {
                                            NULL
                                        }
                                    },
                                    h = in.poly))
        in.poly <- in.poly[order(in.poly$hInd), ]
        in.poly <- in.poly[!duplicated(in.poly$xInd), ]

        cat(crayon::green(paste("Samples found:", nrow(in.poly), "of", n, n.init,"\n")))
        if (nrow(in.poly) >= n) {
            break
        }
        else {
            n.cur <- n - nrow(in.poly)
            m <- m + n.init
            p <- min(1, (sum(area) - area[1]*nrow(in.poly))/max( dx, dy )^2)
        }
    }
    halt.pts <- x[ in.poly$xInd[1:n], ]
    halt.pts$sampleID <- 1:nrow(halt.pts)

    attr(halt.pts, "frame") <- deparse(substitute(x))
    attr(halt.pts, "frame.type") <- "polygon"
    attr(halt.pts, "sample.type") <- "BAS"
    attr(halt.pts, "random.start") <- m
    halt.pts
}
