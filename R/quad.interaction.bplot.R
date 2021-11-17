quad.interaction.bplot <- function (f1,f2,f3,f4,x,...)
  {
    par(mfcol=c(nlevels(f3),nlevels(f4)))
    for (i in levels(f4))
      for (j in levels(f3))
        { s <-  f4==i & f3==j
          double.interaction.bplot(f1[s],f2[s],x[s],xlab=paste(i,j),...)
        }
  }
