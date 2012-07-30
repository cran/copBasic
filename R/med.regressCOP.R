"med.regressCOP" <-
function(U=seq(0.01,0.99, by=0.01),
         cop=NULL, para=NULL, ...) {
  return(qua.regressCOP(F=0.5, U=U, cop=cop, para=para, ...))
}
