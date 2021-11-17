#  from F. Harrel Jrl's Hmisc package
llist <- function(..., labels=TRUE)
{
  dotlist <- list(...)
  lname <- names(dotlist)
  name <- vname <- as.character(sys.call())[-1]
  for(i in 1:length(dotlist)) {
    vname[i] <-
      if(length(lname) && lname[i]!='')
        lname[i]
      else
        name[i]
    
    ## Was changed 21Mar01 - R barked at setting vname[i] to NULL
    lab <- vname[i]
    if(labels) {
      lab <- attr(dotlist[[i]],'label')
      if(length(lab) == 0)
        lab <- vname[i]
    }
    
    label(dotlist[[i]]) <- lab
  }
  
  names(dotlist) <- vname[1:length(dotlist)]
  dotlist
}
