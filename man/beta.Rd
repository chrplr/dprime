\name{beta}
\alias{beta}
\title{beta - bias}
\description{
Computes the beta bias measure of the signal detection theory}
\usage{
beta(hit,fa)
}
\arguments{
  \item{hit}{hit rates}
  \item{fa}{false alarm rates}
}
\details{
  Return a vector containing beta values for each hit and fa rate pairs.
}

\note{}
\seealso{
  \code{\link{dprime,aprime,bppd}}.
}
\examples{
beta(.8,.2)
}
\keyword{SDT, bias}

