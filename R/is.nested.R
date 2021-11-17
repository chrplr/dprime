is.nested <- function (factor1,factor2)
  {
    # only one positive number per line in the f1 * f2 crosstable
    all(apply(table(factor1,factor2)>0,1,sum) == 1) 
  }
