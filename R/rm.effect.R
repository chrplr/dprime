rm.effect <- function (x,effect,FUN=mean) {
  m=tapply(x,effect,FUN)
  x-m[as.character(effect)]
}

