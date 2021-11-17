#Subject: [R] Summary: Partial correlation coefficients in R.
#From: Kaspar Pflugshaupt (pflugshaupt@geobot.umnw.ethz.ch)
#Date: Sat 26 Feb 2000 - 02:01:21 EST


f.parcor <-
function (x, test = F, p = 0.05)
{
    nvar <- ncol(x)
    ndata <- nrow(x)
    conc <- solve(cor(x))
    resid.sd <- 1/sqrt(diag(conc))
    pcc <- -sweep(sweep(conc, 1, resid.sd, "*"), 2, resid.sd,
        "*")
    colnames(pcc) <- rownames(pcc) <- colnames(x)
    if (test) {
        t.df <- ndata - nvar
        t <- pcc/sqrt((1 - pcc^2)/t.df)
        pcc <- list(coefs = pcc, significant = t > qt(1 - (p/2),
            df = t.df))
    }
    return(pcc)
} 
