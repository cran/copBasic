"hoefCOP" <-
function(cop=NULL, para=NULL, p=2, brute=FALSE, delta=0.002, ...) {
   if(p == 1) {
      kp <- 12
   } else if(p == 2) {
      kp <- 90
   } else if(p == 3) {
      kp <- 560
   } else if(p == 4) {
      kp <- 3150
   } else if(p == 5) {
      kp <- 16600
   } else {
      kp <- exp(lgamma(2*p+3) - 2*lgamma(p + 1))/2 # Nelsen (2006, p. 213)
   }
   if(brute) {
      sum <- 0
      us <- vs <- seq(.Machine$double.eps, 1-.Machine$double.eps, delta)
      for(u in us) {
         tmp <- sum(sapply(vs, function(v) {
                              abs(COP(u,v, cop=cop, para=para, ...) - u*v)^p }))
         sum <- sum + tmp
      }
      hoef <- (sum*delta^2*kp)^(1/p)
      return(hoef)
   }

   myint <- NULL
   try(myint <- integrate(function(u) {
            sapply(u,function(u) {
                      integrate(function(v) {
                          abs(COP( u, v, cop=cop, para=para, ...) - u*v)^p},
                      0, 1)$value
            })}, 0, 1) )
   if(is.null(myint)) {
      return(NA)
   } else if(myint$value == 0 & myint$abs.error == 0) {
      warning("integrate() returned zero with zero absolute error, ",
              "p is likely too large or the copula is Independence")
      return(NA)
   } else {
      return((myint$value*kp)^(1/p))
   }
}

"LpCOP" <-
function(cop=NULL, para=NULL, p=2, brute=FALSE, delta=0.002, ...) {
    return(hoefCOP(cop=cop, para=para, p=p, brute=brute, delta=delta, ...))
}


"LpCOPradsym" <-
function(cop=NULL, para=NULL, p=2, brute=FALSE, delta=0.002, ...) {
   if(brute) {
      sum <- 0
      us <- vs <- seq(.Machine$double.eps, 1-.Machine$double.eps, delta)
      for(u in us) {
         tmp <- sum(sapply(vs, function(v) {
                                 abs(COP(u,v, cop=cop, para=para, ...) -
                                  surCOP(u,v, cop=cop, para=para, ...))^p }))
         sum <- sum + tmp
      }
      kp <- 1
      Lp <- (sum*delta^2*kp)^(1/p)
      return(Lp)
   }

   myint <- NULL
   try(myint <- integrate(function(u) {
            sapply(u,function(u) {
                      integrate(function(v) {
                          abs(COP( u, v, cop=cop, para=para, ...) -
                           surCOP(u,v, cop=cop, para=para, ...))^p},
                      0, 1)$value
            })}, 0, 1) )
   if(is.null(myint)) {
      return(NA)
   } else {
      kp <- 1
      return((myint$value*kp)^(1/p))
   }
}


"LpCOPpermsym" <-
function(cop=NULL, para=NULL, p=2, brute=FALSE, delta=0.002, ...) {
   if(brute) {
      sum <- 0
      us <- vs <- seq(.Machine$double.eps, 1-.Machine$double.eps, delta)
      for(u in us) {
         tmp <- sum(sapply(vs, function(v) {
                      abs(COP(u,v, cop=cop, para=para, ...) -
                          COP(v,u, cop=cop, para=para, ...))^p }))
         sum <- sum + tmp
      }
      kp <- 1
      Lp <- (sum*delta^2*kp)^(1/p)
      return(Lp)
   }

   myint <- NULL
   try(myint <- integrate(function(u) {
            sapply(u,function(u) {
                      integrate(function(v) {
                          abs(COP(u,v, cop=cop, para=para, ...) -
                              COP(v,u, cop=cop, para=para, ...))^p},
                      0, 1)$value
            })}, 0, 1) )
   if(is.null(myint)) {
      return(NA)
   } else {
      kp <- 1
      return((myint$value*kp)^(1/p))
   }
}

