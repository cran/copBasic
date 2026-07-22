"LMRuvCOP" <-
function(v=seq(0.01, 0.99, by=0.01), cop=NULL, para=NULL, nsim=1E5,
         subdivisions=200L, rel.tol=.Machine$double.eps^0.25, abs.tol=rel.tol, ...) {
  # Jones (2004, eq. 15b) and Asquith (2011, 6.10 is wrong with 1/(1-r) and needs 1/(r-1))
  #                                  refer to lmomco R package inst/ERRATA_FOR_ISBN9781463508418.txt
  Lrv <- function(v, r=2) {
    sapply(v, function(v) { sum(sapply(0:(r-2), function(j) {
                     (-1)^j * exp(lchoose(r-1,j)+lchoose(r-1,j+1))*v^(r-2-j)*(1-v)^j })) })/(r-1)
  }

  medvatv <- med.regressCOP2(v, cop=COP, para=para)[,1]
  lam1atv <- EuvCOP(v, cop=COP, para=para, rel.tol=rel.tol, abs.tol=abs.tol,
                                           subdivisions=subdivisions, ...)
  lam2atv <- sapply(v, function(v) { E <- NULL
                                 try(E <- integrate(function(u, para=para) {
                t <- derCOP2(cop=COP, u, v, para=para); return(t*(1-t)*Lrv(t, r=2)) }, 0, +1,
                                           para=para, subdivisions=200)$value, silent=TRUE)
                          if(is.null(E)) { t <- derCOP2(cop=COP, runif(nsim), v, para=para)
                                     E <- mean(t*(1-t)*Lrv(t, r=2))
                               names(E) <- "sim"
                          }
                              return(E) })
  lam3atv <- sapply(v, function(v) { E <- NULL
                                 try(E <- integrate(function(u, para=para) {
                t <- derCOP2(cop=COP, u, v, para=para); return(t*(1-t)*Lrv(t, r=3)) }, 0, +1,
         rel.tol=rel.tol, abs.tol=abs.tol, para=para, subdivisions=200)$value, silent=TRUE)
                          if(is.null(E)) { t <- derCOP2(cop=COP, runif(nsim), v, para=para)
                                     E <- mean(t*(1-t)*Lrv(t, r=3))
                               names(E) <- "sim"
                          }
                              return(E) })
  lam4atv <- sapply(v, function(v) { E <- NULL
                                 try(E <- integrate(function(u, para=para) {
                 t <- derCOP2(cop=COP, u, v, para=para); return(t*(1-t)*Lrv(t, r=4)) }, 0, +1,
          rel.tol=rel.tol, abs.tol=abs.tol, para=para, subdivisions=200)$value, silent=TRUE)
                          if(is.null(E)) { t <- derCOP2(cop=COP, runif(nsim), v, para=para)
                                     E <- mean(t*(1-t)*Lrv(t, r=4))
                               names(E) <- "sim"
                          }
                              return(E) })
  zzz <- list(median=medvatv, L1=lam1atv, L2=lam2atv, T3=lam3atv/lam2atv, T4=lam4atv/lam2atv)
  return(zzz)
}
