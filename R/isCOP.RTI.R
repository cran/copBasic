"isCOP.RTI" <-
function(cop=NULL, para=NULL, wrtV=FALSE, delta=0.005, ...) {
  TT <- seq(0+delta, 1-delta, by=delta)
  is.RTI <- TRUE
  if(wrtV) {
    for(u in TT) {
      derC  <- sapply(TT, function(v) { return(derCOP2(u,v, cop=cop, para=para, ...))})
      CdivT <- sapply(TT, function(v) { return(u - cop(u,v, para=para, ...)/(1 - v))})
      if(any(derC < CdivT)) { is.RTI <- FALSE; break }
    }
  } else {
    for(v in TT) {
      derC  <- sapply(TT, function(u) { return(derCOP(u,v, cop=cop, para=para, ...))})
      CdivT <- sapply(TT, function(u) { return(v - cop(u,v, para=para, ...)/(1 - u))})
      if(any(derC < CdivT)) { is.RTI <- FALSE; break }
    }
  }
  return(is.RTI)
}
