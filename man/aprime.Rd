\name{aprime}
\alias{aprime}
\title{A prime}
\description{
Computes the A' sensitivity measure of the signal detection theory}
\usage{
aprime(hit,fa)
}
\arguments{
  \item{hit}{hit rates}
  \item{fa}{false alarm rates}
}
\details{
  Return a vector containing A' values for each hit and fa rate pairs.
}
\note{References: Wayne Donaldson (1992) Measuring Recognition
  Memory. Journal of Experimental Psychology: General, 121, 3, 275--277.

  Wayne Donaldson (1993) Accuracy of d' and A' as estimates of
  sensitivity.  Bulletin of the Psychonomics Society, 31(4), 271--274.  
}
\seealso{
  \code{\link{beta,dprime,bppd}}.
}
\examples{
aprime(.8,.2)
}
\keyword{maths}

