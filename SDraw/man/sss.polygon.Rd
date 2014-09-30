\name{sss.polygon}
\alias{sss.polygon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sss.polygon(n, shp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{shp}{
%%     ~~Describe \code{shp} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (n, shp) 
{
    bb <- bbox(shp)
    lx <- diff(bb["x", ])
    ly <- diff(bb["y", ])
    A <- sum(unlist(lapply(shp@polygons, function(x) {
        x@area
    })))
    delta <- sqrt(A/n)
    m.x <- runif(1, 0, delta)
    m.y <- runif(1, 0, delta)
    seq.x <- seq(bb["x", "min"], bb["x", "max"], by = delta) + 
        m.x
    seq.y <- seq(bb["y", "min"], bb["y", "max"], by = delta) + 
        m.y
    grd <- expand.grid(x = seq.x, y = seq.y)
    df <- data.frame(row = rep(1:length(seq.y), each = length(seq.x)), 
        col = rep(1:length(seq.x), length(seq.y)), pointType = rep("Sample", 
            length(seq.x) * length(seq.y)))
    grd <- SpatialPointsDataFrame(grd, proj4string = CRS(proj4string(shp)), 
        data = df)
    shp@data <- data.frame(data.frame(shp), zzz = 1)
    tmp <- over(grd, shp)
    keep <- !is.na(tmp$zzz)
    tmp <- tmp[, !(names(tmp) \%in\% c("zzz"))]
    tmp2 <- data.frame(grd)
    tmp2 <- tmp2[, !(names(tmp2) \%in\% c("x", "y"))]
    grd@data <- data.frame(tmp2, tmp)
    grd <- grd[keep, ]
    attr(grd, "spacing") <- delta
    grd
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
