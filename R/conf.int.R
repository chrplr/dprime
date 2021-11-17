# return the *half size* of confidence interval of the mean
#
conf.int <- function (x, conf.level=0.95) {
    nx <- length(x)
    if (nx < 2) 
        stop("not enough x observations")
    mx <- mean(x)
    vx <- var(x)
    df <- length(x) - 1
    se <- sqrt(vx/nx)
    tstat <- mx/se
    alpha <- 1 - conf.level
    cint <- qt(1 - alpha/2, df) # qt(x,df)=P(X<=x) if X~t(df)
    se * cint
}
