"LMRvuCOP" <-
function(u=seq(0.01, 0.99, by=0.01), cop=NULL, para=NULL, nsim=1E5,
         subdivisions=200L, rel.tol=.Machine$double.eps^0.25, abs.tol=rel.tol, ...) {
  # Jones (2004, eq. 15b) and Asquith (2011, 6.10 is wrong with 1/(1-r) and needs 1/(r-1))
  #                                  refer to lmomco R package inst/ERRATA_FOR_ISBN9781463508418.txt
  Lru <- function(u, r=2) {
    sapply(u, function(u) { sum(sapply(0:(r-2), function(j) {
                     (-1)^j * exp(lchoose(r-1,j)+lchoose(r-1,j+1))*u^(r-2-j)*(1-u)^j })) })/(r-1)
  }

  medvatu <- med.regressCOP(u, cop=COP, para=para)[,2]
  lam1atu <- EvuCOP(u, cop=COP, para=para, rel.tol=rel.tol, abs.tol=abs.tol,
                                           subdivisions=subdivisions, ...)
  lam2atu <- sapply(u, function(u) { E <- NULL
                                 try(E <- integrate(function(v, para=para) {
                t <- derCOP(cop=COP, u, v, para=para); return(t*(1-t)*Lru(t, r=2)) }, 0, +1,
                                           para=para, subdivisions=200)$value, silent=TRUE)
                          if(is.null(E)) { t <- derCOP(cop=COP, u, runif(nsim), para=para)
                                     E <- mean(t*(1-t)*Lru(t, r=2))
                               names(E) <- "sim"
                          }
                              return(E) })
  lam3atu <- sapply(u, function(u) { E <- NULL
                                 try(E <- integrate(function(v, para=para) {
                t <- derCOP(cop=COP, u, v, para=para); return(t*(1-t)*Lru(t, r=3)) }, 0, +1,
         rel.tol=rel.tol, abs.tol=abs.tol, para=para, subdivisions=200)$value, silent=TRUE)
                          if(is.null(E)) { t <- derCOP(cop=COP, u, runif(nsim), para=para)
                                     E <- mean(t*(1-t)*Lru(t, r=3))
                               names(E) <- "sim"
                          }
                              return(E) })
  lam4atu <- sapply(u, function(u) { E <- NULL
                                 try(E <- integrate(function(v, para=para) {
                 t <- derCOP(cop=COP, u, v, para=para); return(t*(1-t)*Lru(t, r=4)) }, 0, +1,
          rel.tol=rel.tol, abs.tol=abs.tol, para=para, subdivisions=200)$value, silent=TRUE)
                          if(is.null(E)) { t <- derCOP(cop=COP, u, runif(nsim), para=para)
                                     E <- mean(t*(1-t)*Lru(t, r=4))
                               names(E) <- "sim"
                          }
                              return(E) })
  zzz <- list(median=medvatu, L1=lam1atu, L2=lam2atu, T3=lam3atu/lam2atu, T4=lam4atu/lam2atu)
  return(zzz)
}
