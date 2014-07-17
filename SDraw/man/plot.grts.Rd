\name{plot.grts}
\alias{plot.grts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a GRTS sample}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.grts(button)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{button}{
%%     ~~Describe \code{button} here~~
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
function (button) 
{
    fn <- shape.in.entry$getText()
    fn <- file.path(.GRTS.INPUT.DIR, fn)
    outobj <- out.r.entry$getText()
    shp <- read.shape(fn)
    if ("SpatialPolygonsDataFrame" \%in\% class(shp)) {
        plot(shp, col = rainbow(length(shp@polygons)))
    }
    else {
        plot(shp)
    }
    if (exists(outobj)) {
        samp <- get(outobj, pos = .GlobalEnv)
        points(samp[samp$panel != "OverSamp", ], pch = 16)
        points(samp[samp$panel == "OverSamp", ], pch = 1)
        if (sum(samp$panel == "OverSamp") > 0) {
            legend("bottomleft", legend = c("Sample", "Over sample"), 
                pch = c(16, 1))
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
