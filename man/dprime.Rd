\name{dprime}
\alias{dprime}
\title{D prime}
\description{
Computes the d' sensitivity measure of the signal detection theory}
\usage{
dprime(hit,fa,design="yes.no")
}
\arguments{
  \item{hit}{hit rates}
  \item{fa}{false alarm rates}
}
\details{
  Return a vector containing d' values for each hit and fa rate pairs.

  The optional argument "design" can take 4 values: "yes.no" (default),
"forced.choice", "same.different.ind.obs", "same.different.diff",
refering to the different SDT models descirbe in MacMillan and Creelman
(1991).  }

\note{Reference: MacMillan and Creelman (1991) Signal Detection Theory:
  a user guide. Cambridge University Press}
\seealso{
  \code{\link{beta,aprime,bppd}}.
}
\examples{
dprime(.8,.2)
dprime(.7,.1)
dprime(.7,.1,design="forced.choice")
dprime(.7,.1,design="same.diff.ind.obs")
dprime(.7,.1,design="same.diff.diff")
}
\keyword{SDT, sensitivity}

