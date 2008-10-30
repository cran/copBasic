"tauCOP" <-
function(cop=NULL,  para=NULL,
         cop2=NULL, para2=NULL, byloops=FALSE, delta=0.002, ...) {
   if(is.null(cop2)) {
     cop2  <- cop  # This is the expected opertion
     para2 <- para # as Kendall's Tau gets returned
   }

   if(byloops) {
     sum <- 0
     for(u in seq(  .Machine$double.eps,
                  1-.Machine$double.eps,delta)) {
       for(v in seq(  .Machine$double.eps,
                    1-.Machine$double.eps,delta)) {
         dCAu <- derCOP(u,v,  cop=cop2, para=para2)
         dCBv <- derCOP2(u,v, cop=cop,  para=para)
         sum  <- sum + dCAu*dCBv
       }
     }
     Q <- 4*(0.5 - sum*delta^2) - 1
     return(Q)
   }

   myint <- integrate(function(u) {
               sapply(u,function(u) {
                 integrate(function(v) {
                 derCOP( u, v, cop=cop2, para=para2, ...)*
                 derCOP2( u, v, cop=cop, para=para, ...)},
                         0, 1)$value
             })}, 0, 1)
   return(4*(0.5 - myint$value) - 1)
}

