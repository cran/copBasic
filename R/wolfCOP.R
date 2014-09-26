"wolfCOP" <-
function(cop=NULL,  para=NULL, brute=FALSE, delta=0.002, ...) {
   if(brute) {
      sum <- 0
      us <- vs <- seq(.Machine$double.eps, 1-.Machine$double.eps, delta)
      for(u in us) {
         tmp <- sum(sapply(vs, function(v) { abs(cop(u,v, para=para, ...) - u*v) }))
         sum <- sum + tmp
      }
      wolf <- 12*sum*delta^2
      return(wolf)
   }

   myint <- NULL
   try(myint <- integrate(function(u) {
            sapply(u,function(u) {
                      integrate(function(v) {
                          abs(COP( u, v, cop=cop, para=para, ...) - u*v)},
                      0, 1)$value
            })}, 0, 1) )
   wolf <- ifelse(is.null(myint), NA, 12*myint$value)
   return(wolf)
}

