\name{baseb2dec}
\alias{baseb2dec}

\title{
Arbitrary base to decimal conversion.}
\description{
Converts a number represented in a general base (base b) to decimal (base 10)}
\usage{
baseb2dec(x, b = 2)
}

\arguments{
  \item{x}{ A nXc matrix. Each row contains a number to convert.  The 
  columns of each row represent digits in base b.  
  For example, x=matrix( c(1,1,0), 1) represents the decimal number 6 in base 2, 
  x=matrix( c(4,0,3), 1) represents the decimal number 103 = 4*5^2 + 0*5^1 + 3*5^0 in base 5. 
  Digits in all numbers must be integers. }
  \item{b}{ Integer base of the numbers in x}
}

\value{
A vector of length \code{nrow(x)} containing decimal representations of the numbers 
in the rows of \code{x}.}
\author{Trent McDonald}

\section{Warning}{The digits in \code{x} are not checked for validity. To be 
valid, all entries in \code{x}
must be integers and strictly less than the base (\code{b}).  
}

\examples{

x<- matrix( c(4,0,1), 1)
baseb2dec( x, 5)   # 101

x<- matrix( c(1,1), 1)
baseb2dec( x, 16 )  # 17 in base 16 (hex)

}
\keyword{ modeling }

