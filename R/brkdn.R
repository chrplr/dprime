# brkdn is a function that attempts to calculate and display means, 
# variances and valid ns for the variable that appears on the left 
# side of the formula.
# It expects the variables on the right side of the formula to be
# factors, or at worst integers with a very small range.
# It returns a list of "dstat" objects (or a list of lists of "dstat"
# objects if there are two breakdown variables, and so on.)
# A "dstat" object is a matrix that looks like:
#                       vname1  vname2  ...
#       Mean            mean1   mean2   ...
#       Variance        var1    var2    ...
#       Valid n         n1      n2      ...

brkdn<-function(formula,dataframe,maxlevels=10) {
 if(!missing(dataframe) && !missing(formula)) {
  bn<-as.character(attr(terms(formula),"variables")[-1])
  nbn<-length(bn)
  cat("Breakdown of",bn[1],"by",bn[2],"\n")
  if(nbn > 2) {
   # get the factor for this level
   by.factor<-as.factor(dataframe[[bn[2]]])
   factor.levels<-levels(by.factor)
   nlevels<-length(factor.levels)
   if(nlevels > maxlevels) {
    nlevels<-maxlevels
    cat("Too many levels - only using first",maxlevels,"\n")
   }
   brkstats<-as.list(rep(0,nlevels))
   names(brkstats)<-factor.levels
   # calculate the mean for this level
   for(i in 1:nlevels) {
    currentdata<-subset(dataframe,by.factor == factor.levels[i])
    cat("\nMean for",bn[2],"- level",factor.levels[i],"=",
     mean(currentdata[bn[1]],na.rm=T),"\n\n")
    next.formula<-as.formula(paste(paste(bn[1],"~"),paste(bn[3:nbn],collapse="+")))
    # and call yourself for the next level down
    brkstats[[i]]<-brkdn(next.formula,currentdata)
   }
   class(brkstats)<-"dstat"
   invisible(brkstats)
  }
  else {
   by.factor<-as.factor(dataframe[[bn[2]]])
   factor.levels<-levels(by.factor)
   nlevels<-length(factor.levels)
   if(nlevels > maxlevels) {
    nlevels<-maxlevels
    cat("Too many levels - only using first",maxlevels,"\n")
   }
   gstats<-matrix(NA,ncol=nlevels,nrow=3)
   colnames(gstats)<-factor.levels[1:nlevels]
   rownames(gstats)<-c("Mean","Variance","n")
   # calculate the basic descriptive stats
   if(is.numeric(dataframe[[bn[1]]])) {
    for(i in 1:nlevels) {
     currentdata<-subset(dataframe[[bn[1]]],by.factor == factor.levels[i])
     if(length(currentdata)) gstats[,i]<-desc.stat(currentdata,na.rm=T)
    }
    class(gstats)<-"dstat"
    print(gstats)
   }
   invisible(gstats)
  }
 }
 else cat("Usage: brkdn(formula, dataframe, maxlevels=10)\n")
}

# desc.stat calculates the mean, variance and valid n for a numeric vector
# needed by brkdn()

desc.stat<-function(datavec,na.rm=T) {
 dstat<-c(0,0,0)
 dstat[1]<-mean(datavec,na.rm=na.rm)
 dstat[2]<-var(datavec,na.rm=na.rm)
 dstat[3]<-sum(!is.na(datavec))
 return(dstat)
}
