"simCOPmicro" <-
function(u, cop=NULL, para=NULL, ...) {
  n <- length(u); t <- runif(n); v <- vector(mode="numeric", length=n)
  v <- sapply(1:n, function(i) { derCOPinv(cop=cop, u[i], t[i], para=para, ...) })
  if(any(is.na(v))) warning("could not uniroot at least for one element in derCOPinv")
  return(v)
}
