\name{is.nested}
\alias{is.nested}
\title{is.nested}
\description{
test if factor1 is nested within factor2.  that is, if each level of
factor 1 Computes the A' sensitivity measure of the signal detection theory}
\usage{
is.nested(factor1,factor2)
}
\arguments{
  \item{factor1,factor2}{factors}
}
\details{
  Return true iff all levels of factor1 correspond to at most one level
  of factor2.
}
\note{
}
\seealso{
  \code{\link{are.crossed}}.
}
\examples{
group <- gl(3,10) # 3 groups
subject <- gl(30,1) # 30 subjects
is.nested(subject,group)

subject<- gl(10,1,30) # 10 subjects
is.nested(subject,group)
}
\keyword{design}

