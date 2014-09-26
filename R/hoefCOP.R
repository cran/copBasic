"hoefCOP" <-
function(cop=NULL,  para=NULL, brute=FALSE, delta=0.002, ...) {
   if(brute) {
      sum <- 0
      us <- vs <- seq(.Machine$double.eps, 1-.Machine$double.eps, delta)
      for(u in us) {
         tmp <- sum(sapply(vs, function(v) { (cop(u,v, para=para, ...) - u*v)^2 }))
         sum <- sum + tmp
      }
      hoef <- 3*sqrt(10*sum*delta^2)
      return(hoef)
   }

   myint <- NULL
   try(myint <- integrate(function(u) {
            sapply(u,function(u) {
                      integrate(function(v) {
                          (COP( u, v, cop=cop, para=para, ...) - u*v)^2},
                      0, 1)$value
            })}, 0, 1) )
   hoef <- ifelse(is.null(myint), NA, 3*sqrt(10*myint$value))
   return(hoef)
}

