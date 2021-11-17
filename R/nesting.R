nesting <- function (factor1, factor2)
  {
    stopifnot(is.nested(factor1,factor2))
    aggregate(factor1, list(factor1,factor2), length)
  }
