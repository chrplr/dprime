se <- function(x) {
    nx <- length(x)
    if (nx < 2) 
        stop("not enough x observations")
    sqrt(var(x)/nx)
}
