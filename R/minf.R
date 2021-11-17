# computation of MinF' (see Herbert H.  Clark (1973) "The
# Language-as-fixed-Effect Fallacy: A critique of Language Statistics
# in Psychological Research" in Journal of Verbal Learning and Verbal
# Behavior, 12, 335-359.)

minf <- function (df11,df12,f1,df21,df22,f2)
  {
    stopifnot(df11==df21)
    dfm1=df11;
    dfm2=floor((f1+f2)*(f1+f2)/(f1*f1/df22+f2*f2/df12)+0.5);
    mf=f1*f2/(f1+f2)
    c(mf,dfm1,dfm2)
  }
