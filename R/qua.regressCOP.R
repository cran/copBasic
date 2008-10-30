"qua.regressCOP" <-
function(F=0.5,
         U=seq(0.01,0.99, by=0.01),
         cop=NULL, para=NULL, ...) {
  n <- length(U)
  V <- vector(mode="numeric", length=n)
  for(i in 1:n) {
    v <- derCOPinv(cop=cop, U[i], F, para=para)
    if(is.na(v)) {
       txt <- paste(c("could not uniroot in derCOPinv,",
                      " skipping sample for i=",i," having U=",
                      U[i]),collapse="")
       warning(txt)
       warning(para)
       next
    }
    V[i] <- v
  }
  z <- data.frame(U=U,V=V)
  return(z)
}
