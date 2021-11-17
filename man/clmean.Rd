\name{clmean}
\alias{clmean}
\title{"Clean" mean}
\description{
  Compute the mean after removing or replacing outliers. Outliers are
  values above or under certain fixed cutoffs, and values which are more
  than a certain number of standard deviations away from the mean.
  The best way to understand the details is to read the function's
  source code (any verbal explanation will much longer). 
}
\usage{
clmean(x, max.sd = 2, cutoff.min = NULL, cutoff.max = NULL, replace = F)
}
\arguments{
  \item{x}{a numeric vector.}
  \item{max.sd}{number of stddev from the mean above which an item is considered an outlier.}
  \item{cutoff.min}{a numeric value: items lower than that are
    considered outlier}
  \item{cutoff.max}{a numeric value beyond which items are considered
    outliers.}
  \item{replace}{should outliers be excluded or replaced by the cutoff value?}
}
\seealso{
  \code{\link{mean}}.
}
\examples{
x=c(1,2,3,4,5,20)
clmean(x)
clmean(x,max.sd=3)
clmean(x,replace=TRUE)
}

\keyword{mean, sd}

