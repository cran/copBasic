"rhoCOP" <-
function(cop=NULL, para=NULL, brute=FALSE, delta=0.002, ...) {
   if(brute) {
      # Following logic would implement the concordance function via tauCOP
      # if the previous if(brute) was not there, but her we only resort to if
      # brute is request.
      Q <- tauCOP(cop=cop, cop2=P, para=para, brute=brute, delta=delta, ...);
      rho <- 3*Q;
      return(rho)
   }
   myint <- NULL
   try(myint <- integrate(function(u) {
            sapply(u,function(u) {
                      integrate(function(v) {
                          COP( u, v, cop=cop, para=para, ...) - u*v},
                      0, 1)$value
            })}, 0, 1) )
   rho1 <- ifelse(is.null(myint), NA, 12*myint$value)
   return(rho1) # This version seems the most stable given M and W copulas
   #message("rho1 =",rho1)
   #print(myint)

   # Code below is for research purposes only. But any of
   # the solutions below seems to work as a degree of singularity in the
   # copula goes away.

   #myint <- NULL
   #try(myint <- integrate(function(u) {
   #         sapply(u,function(u) {
   #                   integrate(function(v) {
   #                      COP(u, v, cop=cop, para=para, ...)},
   #                   0, 1)$value
   #         })}, 0, 1) )
   #rho2 <-ifelse(is.null(myint), NA, 12*myint$value - 3)
   #message("rho2 =",rho2)
   #print(myint)
   #rho3 <- 3*tauCOP(cop=cop, para=para, cop2=P)
   #message("rho3 =",rho3)
   #return(c(rho1, rho2, rho3))
}

