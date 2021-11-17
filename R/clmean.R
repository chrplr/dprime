clmean <- function (x,
                    max.sd = 2,
                    cutoff.min = NULL,
                    cutoff.max = NULL,
                    replace = FALSE)
{
  low.cut <- high.cut <- rep(FALSE,length(x))
  if (!missing(cutoff.min)) low.cut = x<cutoff.min 
  if (!missing(cutoff.max)) high.cut = x>cutoff.max
  if (replace) {
   if (any(low.cut))  x[low.cut]=cutoff.min;
   if (any(high.cut)) x[high.cut]=cutoff.max;
  } else {
    x=x[!low.cut & !high.cut];
  }
  stopifnot(length(x)>1)
  m = mean(x)
  d = sd(x)
  low.sd = (m-x)/d>max.sd
  high.sd = (x-m)/d>max.sd
  if (replace) {
    x[low.sd] = m-max.sd*d
    x[high.sd] = m+max.sd*d
  } else {
    x=x[!low.sd & !high.sd];
  }
  mean(x)
}
