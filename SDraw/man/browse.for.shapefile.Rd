\name{browse.for.shapefile}
\alias{browse.for.shapefile}

\title{Browse for a shapefile}
\description{
Browse for a shapefile}
\usage{
browse.for.shapefile()
}

\details{
Uses \code{choose.files()} to prompts the user for the name of a shapefile.  The hidden
variable \code{.INPUT.DIR} in the \code{.GlobalEnv} is set to directory containing the choosen file. }
\value{
A text string containing the name of the choosen file, with full path.}
\author{
Trent McDonald
}

\seealso{
\code{\link{choose.files}}
}
\examples{
browse.for.shapefile()
}
\keyword{ modeling }
