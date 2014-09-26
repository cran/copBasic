"simCOP" <-
function(n=100, cop=NULL, para=NULL, na.rm=TRUE, keept=FALSE, ploton=TRUE, points=TRUE, ...) {
  if(ploton) {
    plot(c(0,1), c(0,1), type="n",
         xlab="U, NONEXCEEDANCE PROBABILITY", ylab="V, NONEXCEEDANCE PROBABILITY")
  }
  u <- runif(n); t <- runif(n);
  v <- sapply(1:n, function(i) { derCOPinv(cop=cop, u[i], t[i], para=para, ...) })

  if(points & ! is.null(dev.list())) points(u,v, ...)

  # Because z is a data.frame, it must be assigned within the ifelse()
  ifelse(keept, z <- data.frame(U=u, V=v, T=t), z <- data.frame(U=u, V=v))
  if(na.rm) {
     z <- z[complete.cases(z), ]
     m <- length(z[,1])
     if(m != n) {
        warning("user requested n=",n," simulations but only m=",m,
                " could be made without NA from derCOPinv (uniroot failure there)")
        row.names(z) <- NULL # reset the rows to "1:m"
     }
  }
  return(z)
}
