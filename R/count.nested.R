count.nested <- function (factor1, factor2)
  {
    stopifnot(is.nested(factor1,factor2))
    table(nesting(factor1, factor2)[[2]])
  }
