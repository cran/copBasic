"med.regressCOP2" <-
function(V=seq(0.01,0.99, by=0.01),
         cop=NULL, para=NULL, ...) {
  return(qua.regressCOP2(F=0.5, V=V, cop=cop, para=para, ...))
}
