"isCOP.radsym" <-
function(cop=NULL, para=NULL, delta=0.005, tol=1e-4, ...) {
  TT <- seq(0+delta, 1-delta, by=delta)
  tmp <- sapply(TT, function(u) { sapply(TT, function(v) {
             return(u + v - 1 + COP(1-u,1-v, cop=cop, para=para, ...) -
                                COP(  u,  v, cop=cop, para=para, ...))
                               } ) })
  ifelse(any(abs(tmp) > tol), return(FALSE), return(TRUE))
}
