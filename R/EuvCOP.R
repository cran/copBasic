"EuvCOP" <-
function(v=seq(0.01, 0.99, by=0.01), cop=NULL, para=NULL, asuv=FALSE, nsim=1E5,
         subdivisions=100L, rel.tol=.Machine$double.eps^0.25, abs.tol=rel.tol, ...) {

   u <- sapply(v, function(t) {
          E <- NULL
          try(E <- integrate(function(k) { 1 - derCOP2(cop=cop, para=para, k, t, ...) },
                    lower=0, upper=1, subdivisions=subdivisions,
                    rel.tol=rel.tol, abs.tol=abs.tol)$value, silent=TRUE)
          if(is.null(E)) {
            E <- mean( 1 - derCOP2(cop=cop, para=para, runif(nsim), t, ...) )
            names(E) <- "sim"
          }
          return(E)
        })
   u[u <= 0] <- 0
   u[u >= 1] <- 1
   if(asuv) return(data.frame(U=u, V=v))
   return(u)
}
