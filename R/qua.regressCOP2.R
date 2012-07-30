"qua.regressCOP2" <-
function(F=0.5,
         V=seq(0.01, 0.99, by=0.01),
         cop=NULL, para=NULL, ...) {
  n <- length(V)
  U <- vector(mode="numeric", length=n)
  for(i in 1:n) {
    u <- derCOPinv2(cop=cop, V[i], F, para=para)
    if(is.na(u)) {
       txt <- paste(c("could not uniroot in derCOPinv2,",
                      " skipping sample for i=",i," having V=",
                      V[i]), collapse="")
       warning(txt)
       warning(para)
       U[i] <- NA
       next
    }
    U[i] <- u
  }
  z <- data.frame(U=U,V=V)
  return(z)
}
