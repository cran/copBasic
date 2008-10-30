"med.regressCOP2" <-
function(V=seq(  .Machine$double.eps^0.5,
               1-.Machine$double.eps^0.5, by=0.01),
         cop=NULL, para=NULL, ...) {
  return(qua.regressCOP2(F=0.5, V=V, cop=cop, para=para, ...))
}
