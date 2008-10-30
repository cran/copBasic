"rhoCOP" <-
function(cop=NULL, para=NULL,
         byloops=FALSE, delta=0.002, ...) {
   if(byloops) {
     Q <- tauCOP(cop=cop,   cop2=P, para=para,
                 delta=delta, byloops=byloops, ...);
     rho <- 3*Q;
     return(rho)
   }

   myint <- integrate(function(u) {
            sapply(u,function(u) {
               integrate(function(v) {
               COP( u, v, cop=cop, para=para, ...) - u*v},
               0, 1)$value
   })}, 0, 1)
   return(12*myint$value)
}

