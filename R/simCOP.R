"simCOP" <-
function(n=100, cop=NULL, para=NULL,
         ploton=TRUE, points=TRUE, ...) {
  if(ploton) {
    plot(c(0,1), c(0,1),
         type="n",
         xlab="U, NONEXCEEDANCE PROBABILITY",
         ylab="V, NONEXCEEDANCE PROBABILITY")
  }
  U <- vector(mode="numeric"); V <- U
  for(i in 1:n) {
    u <- runif(1); t <- runif(1)
    v <- derCOPinv(cop=cop,u,t,para=para)
    if(is.na(v)) {
       warning("could not uniroot in derCOPinv, skipping sample")
       warning(paste(para, collapse=" "))
       next
    }
    U[i] <- u; V[i] <- v
  }
  if(points & ! is.null(dev.list())) points(U,V,...)
  z <- data.frame(U=U,V=V)
  return(z)
}
