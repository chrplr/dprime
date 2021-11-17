# correct a 'bug' in the 'subset.data.frame' function
# removing empty levels in factors
select<-function (x,cond,cols,...) {
    if (missing(cond))
        r <- TRUE
    else {
        e <- substitute(cond)
        r <- eval(e, x, parent.frame())
        r <- r & !is.na(r)
    }
    if (missing(cols))
        vars <- TRUE
    else {
        nl <- as.list(1:ncol(x))
        names(nl) <- names(x)
        vars <- eval(substitute(cols), nl, parent.frame())
    }
    a=x[r, vars, drop = FALSE]
    # suppress useless levels in factors:
    as.data.frame(lapply(a,function (x) {if (is.factor(x)) factor(x) else x}))
}
