"isCOP.RTI" <-
function(cop=NULL, para=NULL, wrtV=FALSE,
         verbose=FALSE, delt=0.005, ...) {
  T <- seq(0+delt, 1-delt, by=delt)
  if(verbose) cat(c("Checking: "))
  if(wrtV) {
    for(u in T) {
      if(verbose) cat(c(u,", "),sep="")
      derC  <- sapply(T, function(v) { return(derCOP2(u,v, cop=cop, para=para))})
      CdivT <- sapply(T, function(v) { return(u - cop(u,v, para=para)/(1 - v))})
      if(any(derC < CdivT)) {
        if(verbose) cat(c("done\n"))
        return(FALSE)
      }
    }
  } else {
    for(v in T) {
      if(verbose) cat(c(u,", "),sep="")
      derC  <- sapply(T, function(u) { return(derCOP(u,v, cop=cop, para=para))})
      CdivT <- sapply(T, function(u) { return(v - cop(u,v, para=para)/(1 - u))})
      if(any(derC < CdivT)) {
        if(verbose) cat(c("done\n"))
        return(FALSE)
      }
    }
  }
  if(verbose) cat(c("done\n"))
  return(TRUE)
}
