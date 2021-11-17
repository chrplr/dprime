triple.interaction.bplot <- function (f1,f2,f3,x,...)
  {
    par(mfcol=c(1,nlevels(f3)))
    for (i in levels(f3))
      double.interaction.bplot(f1[f3==i],f2[f3==i],x[f3==i],xlab=i,...)      
  }
