\name{bppd}
\alias{bppd}
\title{bppd - bias}
\description{
Computes the bppd bias measure of the signal detection theory}
\usage{
beta(hit,fa)
}
\arguments{
  \item{hit}{hit rates}
  \item{fa}{false alarm rates}
}
\details{
  Return a vector containing bppd values for each hit and fa rate pairs.
}

\note{Reference: Don Wayne Donaldson (1992) Measuring Recognition
    Memory. Journal of Experimental Psychology: General, 121, 3, 275--277
}
\seealso{
  \code{\link{dprime,aprime,beta}}.
}
\examples{
bppd(.8,.2)
}
\keyword{SDT, bias}

