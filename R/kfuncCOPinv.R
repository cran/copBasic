"kfuncCOPinv" <-
function(f, cop=NULL, para=NULL, subdivisions=100L,
            rel.tol=.Machine$double.eps^0.25, abs.tol=rel.tol, ...) {
   if(is.null(cop)) {
      warning("must have copula argument specified, returning NULL")
      return(NULL)
   }
   if(is.null(f)) {
      warning("must have at least on nonexceedance probability, returning NULL")
   }
   if(any(f < 0) || any(f > 1)) {
      warning("invalid nonexceedance probability, returning NULL")
      return(NULL)
   }
   ZinI <- c(0,1)
   "afunc" <- function(z, fk, ...) {
                          fk - kfuncCOP(z, subdivisions=subdivisions,
                                           rel.tol=rel.tol, abs.tol=abs.tol, ...) }
   Z <- sapply(1:length(f), function(i) {
           the.z <- uniroot(afunc, ZinI, fk=f[i], cop=cop, para=para, ...)$root
           return(the.z) })
   return(Z)
}
