nb.levels.within <- function (factor,grouping)
  { tapply(factor,grouping,nb.levels) }
