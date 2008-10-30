"sectionCOP" <-
function(f, cop=NULL,  para=NULL,    wrtV=FALSE,
         ratcop=FALSE, dercop=FALSE, ploton=TRUE,
         lines=TRUE,   delt=0.005, ...) {
  if(wrtV) {
    txtxlab <- "V, NONEXCEEDANCE PROBABILTIY"
    txtylab <- "H, NONEXCEEDANCE PROBABILITY"
    txt <- "with respect to V"
  } else {
    txtxlab <- "U, NONEXCEEDANCE PROBABILTIY"
    txtylab <- "H, NONEXCEEDANCE PROBABILITY"
    txt <- "with respect to U"
  }

  if(ploton) {
    plot(c(0,1), c(0,1), type="n", xlab=txtxlab,  ylab=txtylab)
  }
  T <- seq(0+delt,1-delt,delt)
  C <- vector(mode="numeric")
  if(dercop) {
    if(wrtV) {
      C <- sapply(T, function(x) {
                     return( derCOP2(x,f, cop=cop,
                                     para=para)) } )
    }
    else {
      C <- sapply(T, function(x) {
                     return( derCOP(f,x, cop=cop, para=para)) } )
    }
  } else {
    C <- sapply(T, function(x) { v <- x; u <- f
                      if(wrtV) { u <- x; v <- f }
                         return( cop(u,v, para=para)) } )
    if(ratcop) C <- C/T
  }
  if(lines & ! is.null(dev.list())) lines(T,C, ...)
  return(list(t=T, seccop=C,
              wrt=txt, fvalue=f, isderivative=dercop))
}

