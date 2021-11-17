# create a barplot with error bars 
# the first argument is a matrix containing bars' heights
# the second argument is a matrix containing the half-size of errors bars 
# optional arguments of `barplot' are accepted
errorbarplot <- function (a,b,...) {
  mp<-barplot(a,beside=T,xpd=F,...)

  arrows(mp,a,mp,a+b,angle=90)
#  segments(mp,a,mp,a+b)
#  segments(mp-.1,a+b,mp+.1,a+b)
}
