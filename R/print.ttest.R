# Concise display for "htest" objects
print.ttest <- function(a) {
  cat(v$data.name,"(",v$method,")\n");
  options(digits=3)
  cat("  t(",a$parameter,")=",a$statistic," p=",a$p.value)
  if (!is.na(a$estimate[2])) 
    cat("m1=",a$estimate[1]," m2=",a$estimate[2],
        " conf.int=[",a$conf.int[1],",",a$conf.int[2],"]")
  else
    cat("m=",a$estimate[1],
        " conf.int=[",a$conf.int[1],",",a$conf.int[2],"]")
  cat("\n");
}
