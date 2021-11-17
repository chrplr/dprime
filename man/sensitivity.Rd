\name{sensitivity}
\alias{sensitivity}
\title{Sensitivity}
\description{
Computes the sensitivity measure of the signal detection theory}
\usage{
sensitivity(hit,fa,type="yes.no")
}
\arguments{
  \item{hit}{hit rates}
  \item{fa}{false alarm rates}
}
\details{
  Return a vector containing d' values for each hit and fa rate pairs.

  The optional argument "design" can take one of the following values: "yes.no" (default),
"2AFC" (2 alternative forced-choice), "same.different.fixed",
"same.different.roving", "ABX.fixed", "ABX.roving", "reminder.fixed",
"reminder.roving", "aprime",
refering to the different SDT models described in Macmillan and Creelman
(1991).  }

\note{Reference: MacMillan and Creelman (1991) Signal Detection Theory:
  a user guide. Cambridge University Press}
\seealso{
  \code{\link{bias}}.
}
\examples{
sensitivity(.8,.2) # return d' for a Yes.No experiment
sensitivity(.8,.2,type="aprime") # return A'
sensitivity(.7,.1) 
sensitivity(.7,.1,type="2AFC")
sensitivity(.7,.1,type="same.diff.ind.obs")
sensitivity(.7,.1,type="same.diff")
}
\keyword{SDT, sensitivity}

