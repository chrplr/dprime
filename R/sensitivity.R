# Computation of the sensitivity parameter of Signal Detection Theory

# Reference: Macmillan & Creelman (1991) Signal Detection Theory: A
# user's guide. Cambridge University Press

# Author: Christophe.Pallier@m4x.org

# Date: 18 Nov. 2003

sensitivity <- function(hit,fa,type="yes.no") {
   d <- qnorm(hit) - qnorm(fa)
   pc <-pnorm(d/2)
   sqr2 <- sqrt(2)
   switch(type,
           "yes.no" = d,
           "2AFC" = d/sqr2,
           "same.different.fixed" =  2*qnorm(0.5*(1+sqrt(2*pc-1))),
           "same.different.roving" = {
             cc=rep(0,length(hit))
             for (i in 1:length(hit)) {
               cc[i]=dprime.samediff.differencing.model(hit[i],fa[i])
             }
               cc},
          "ABX.fixed" =   {
             cc=rep(0,length(hit))
             for (i in 1:length(hit)) {
               cc[i]=dprime..abx.indepobs.model(hit[i],fa[i])
             }
             cc},
          "ABX.roving" = {
            cc=rep(0,length(hit))
             for (i in 1:length(hit)) {
               cc[i]=dprime.abx.differencing.model(hit[i],fa[i])
             }
            cc},
          "reminder.fixed" = d,
          "reminder.roving" = sqr2 * d,
          "aprime" = sensitivity.aprime(hit,fa)
           )
}

# same-different roving  (many pairs of stimuli) => differencing model
#  table A.5.4 of Macmillan & Creelman

# same-different fixed (2 stimuli) => independent observation model
# table A.5.3


sensitivity.samediff.differencing.model <- function(hit,fa) {
     if (hit==fa) return (0)
     sign = 1;
     if (hit<fa) { sign=-1; tmp<-fa; fa<-hit; hit<-tmp; }
     sqr2 <- sqrt(2)
     k    <- -sqr2*qnorm(fa/2)
     f    <- function (x) { pnorm((x-k)/sqr2)+pnorm((-x-k)/sqr2)-hit }
     sol <- uniroot(f,c(0,10))
     sign * sol$root
}

sensitivity.abx.indepobs.model <-  function(hit,fa) {
  if (hit==fa) return (0)
  sign = 1;
  if (hit<fa) { sign=-1; tmp<-fa; fa<-hit; hit<-tmp; }
  pc = pnorm((qnorm(hit)-qnorm(fa))/2)
  sqr2 <- sqrt(2)
  f <- function (x) { pnorm(x/sqr2)*pnorm(x/2)+pnorm(-x/sqr2)*pnorm(-x/2)-pc }
  sol <- uniroot(f,c(0,10))
  sign * sol$root
}

sensitivity.abx.differencing.model <-  function(hit,fa) {
  if (hit==fa) return (0)
  sign = 1;
  if (hit<fa) { sign=-1; tmp<-fa; fa<-hit; hit<-tmp; }
  pc = pnorm((qnorm(hit)-qnorm(fa))/2)  
  sqr2=sqrt(2)
  sqr6=sqrt(6)
  f <- function (x) { pnorm(x/sqr2)*pnorm(x/sqr6)+pnorm(-x/sqr2)*pnorm(-x/sqr6)-pc }
  sol <- uniroot(f,c(0,10))
  sign * sol$root
}

sensitivity.aprime <-function(hit,fa) {
   a<-1/2+((hit-fa)*(1+hit-fa) /
                 (4*hit*(1-fa)))
   b<-1/2-((fa-hit)*(1+fa-hit) /
                 (4*fa*(1-hit)))
   a[fa>hit]<-b[fa>hit]
   a[fa==hit]<-.5
   a
}
