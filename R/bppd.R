bppd <-function(hit,fa) {
    ((1-hit)*(1-fa)-hit*fa) /
         ((1-hit)*(1-fa)+hit*fa)
}
