\name{minf}
\alias{minf}
\title{Min F'}
\description{
Computes MinF' given F1 and F2 values obtained from by-subject and
by-item analyses.}
\usage{
minf(df11,df12,f1,df21,df22,f2)
}
\arguments{
  \item{df11,df12,df21,df22}{the degrees of freedom associated to f1 and
    f2.}
  \item{f1,f2}{the F1 and F2 values.}
}
\details{
  Return a vector containing (df1, df2, minf)
}

\note{Reference: Herbert H.  Clark (1973) "The Language-as-fixed-Effect Fallacy: A critique of
Language Statistics in Psychological Research" in Journal of Verbal
Learning and Verbal Behavior, 12, 335-359.}
\seealso{
  \code{\link{pf}}.
}
\examples{
minf(1,23,4,1,45,3.5)
}
\keyword{F-test}

