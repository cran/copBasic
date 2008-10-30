"simCOPmicro" <-
function(u, cop=NULL, para=NULL, ...) {
  t <- runif(1)
  v <- derCOPinv(cop=cop,u,t,para=para)
  if(is.na(v)) {
    warning("could not uniroot in derCOPinv, v=NA")
  }
  return(v)
}
