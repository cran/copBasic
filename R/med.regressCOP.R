"med.regressCOP" <-
function(U=seq(  .Machine$double.eps^0.5,
               1-.Machine$double.eps^0.5, by=0.01),
         cop=NULL, para=NULL, ...) {
  return(qua.regressCOP(F=0.5, U=U, cop=cop, para=para, ...))
}
