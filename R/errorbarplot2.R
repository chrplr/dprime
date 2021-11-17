errorbarplot2 <- function (x,listoffactors,...)
{
  a<-tapply(x,listoffactors,mean)
  b<-tapply(x,listoffactors,se)
  errorbarplot(a,b,...)
}
