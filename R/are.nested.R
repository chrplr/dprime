are.crossed <- function (factor1,factor2)
  { all(table(factor1,factor2) > 0 ) }
