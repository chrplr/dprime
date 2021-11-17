aprime <-function(hit,fa) {
   a<-1/2+((hit-fa)*(1+hit-fa) /
                 (4*hit*(1-fa)))
   b<-1/2-((fa-hit)*(1+fa-hit) /
                 (4*fa*(1-hit)))
   a[fa>hit]<-b[fa>hit]
   a[fa==hit]<-.5
   a
}
