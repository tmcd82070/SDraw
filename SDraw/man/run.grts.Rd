\name{run.grts}
\alias{run.grts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Run a GRTS sample
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
run.grts(button, dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{button}{
%%     ~~Describe \code{button} here~~
}
  \item{dat}{
%%     ~~Describe \code{dat} here~~
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
function (button, dat) 
{
    n <- n.entry$getText()
    fn <- shape.in.entry$getText()
    outobj <- out.r.entry$getText()
    over.n <- over.entry$getText()
    seed <- seed.entry$getText()
    if (nchar(seed) > 0) {
        seed <- as.numeric(seed)
        set.seed(seed)
    }
    if (area.rb$getActive()) {
        sframe.type <- "area"
    }
    else if (line.rb$getActive()) {
        sframe.type <- "linear"
    }
    else {
        sframe.type <- "finite"
    }
    if (length(n) == 0 | (n <= 0)) {
        cat("ERROR: Sample size not specified or is less than 0.\n")
        return()
    }
    if (nchar(fn) == 0 | (fn == " ")) {
        cat("ERROR: Input shape file not specified. \n")
        return()
    }
    if (exists(outobj, where = .GlobalEnv)) {
        assign(paste(outobj, ".previous", sep = ""), get(outobj, 
            pos = .GlobalEnv), pos = .GlobalEnv)
        cat(paste("Old version of", outobj, "copied to", paste(outobj, 
            ".previous", sep = ""), "\n"))
    }
    fn <- file.path(.GRTS.INPUT.DIR, fn)
    n <- as.numeric(n)
    if (nchar(over.n) == 0) {
        over.n <- 0
    }
    else {
        over.n <- as.numeric(over.n)
    }
    if (is.na(over.n)) {
        warning("Oversample set to 0.")
        over.n <- 0
    }
    Equaldsgn <- list(None = list(panel = c(sample = n), seltype = "Equal", 
        over = over.n))
    Equalsites <- grts(design = Equaldsgn, DesignID = "EQUAL", 
        type.frame = sframe.type, src.frame = "shapefile", in.shape = fn, 
        shapefile = FALSE)
    assign(outobj, Equalsites, pos = .GlobalEnv)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
