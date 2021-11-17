describe.factors <- function (data) # take a data.frame as input
  # display the nb of levels of each factor
  # and the relationship (nesting or crossing) between each couple of factors
  {
    fnames<-names(data)[sapply(data,is.factor)]
    for (i in fnames) {
      x<-data[[i]];
      cat (i,"(",nb.levels(x),")\n",sep='');
      cat(" ",levels(x),"\n\n");
    }
    cat("\n\n");
    l<-length(fnames);
    rel=matrix(nrow=l,ncol=l,dimnames=list(fnames,fnames));
    diag(rel)="="
    for (i in 1:(l-1)) {
      x<-data[[fnames[i]]];
      for (j in (i+1):l) {
        y<-data[[fnames[j]]];
          if (is.nested(x,y)) { rel[i,j]<- "w"; rel[j,i]<-"g" }
            else if (is.nested(y,x)) { rel[j,i] <- "w"; rel[i,j]<-"g" }
              else if (are.crossed(x,y)) { rel[i,j] <- rel[j,i] <- "*"; } 
        }
    }
    print(rel,quote=F);

}
