\name{are.crossed}
\alias{are.crossed}
\title{are.crossed}
\description{
  test if two factors are crossed, that is, if all combinations of
  levels occur.}
\usage{
are.crossed(factor1,factor2)
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
  \code{\link{is.nested}}.
}
\examples{
group <- gl(3,10) # 3 groups
subject <- gl(30,1) # 30 subjects
are.crossed(subject,group)

subject<- gl(10,1,30) # 10 subjects
are.crossed(subject,group)
}
\keyword{design}

