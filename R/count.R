count <- function(by,x,ok,FUN=mean)
# exemples d'utilisation:
# x=rnorm(100)
# hit=rbinom(100,size=1,prob=.9)
# a=gl(2,50)
# b=gl(2,25,100)
# count(list(a=a,b=b))
# count(list(a=a,b=b),ok=hit)
# count(list(a=a,b=b),x)
# count(list(a=a,b=b),ok=hit,x=x)
# count(list(a=a,b=b),ok=!hit,x=x)
# count(list(a=a,b=b),ok=hit,x=x,FUN=sd)
  {
    if (missing(ok)) {
      ok=!logical(length(by[[1]]))
    }
    if (mode(ok[1])=="numeric") ok=ok>0

    l=lapply(by,function (x) {as.factor(x[ok])})

    r=aggregate(list(N=by[[1]]),by,length)
    s=aggregate(list(ok=ok[ok]),l,length)    
    t=merge(r,s)
    
    if (missing(x)) { 
         return( t)
       }
    else {
        u=aggregate(x[ok],l,FUN)
        return (merge(t,u))
       }
  }
