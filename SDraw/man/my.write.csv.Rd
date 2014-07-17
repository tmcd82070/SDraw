\name{my.write.csv}
\alias{my.write.csv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write a CSV file to disk}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
my.write.csv(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function (x) 
{
    samp.nm <- out.r.entry$getText()
    if (exists(samp.nm)) {
        samp <- get(samp.nm, pos = .GlobalEnv)
        samp.nm2 <- gsub(".", "_", samp.nm, fixed = TRUE)
        out.fn <- paste(samp.nm2, ".csv", sep = "")
        write.table(samp, file = out.fn, sep = ",", col.names = TRUE, 
            row.names = FALSE)
        cat(paste("GRTS object", samp.nm, "written to", out.fn, 
            "in current directory.\n"))
    }
    else {
        cat(paste("GRTS sample object", samp.nm, "does not exist.\n"))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
