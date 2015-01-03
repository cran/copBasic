"FRECHETcop" <- function(u,v, para=NULL, rho=NULL, tau=NULL, par2rhotau=FALSE, ...) {
   if(is.null(para)) {
      if(! is.null(tau) & ! is.null(rho)) {
         if(tau < -1 | tau > 1) {
            warning("tau not in [-1,1], returning NULL")
            return(NULL)
         }
         if(rho < -1 | rho > 1) {
            warning("tau not in [-1,1], returning NULL")
            return(NULL)
         }
         b <- (3*tau - rho^2 - 2*rho)/(2*rho); a <- rho + b
         if(is.nan(b)) a <- b <- 0
         if(a >= 0 & b >= 0 & (a + b) <= 1) {
            para <- c(a,b); names(para) <- c("alpha", "beta")
            return(list(para=para, source="FRECHETcop"))
         } else {
            warning("incompatible rho and tau: (",rho,", ",tau,"), parameters a and b not a,b >= 0 & a + b <= 1")
            para <- c(NA,NA); names(para) <- c("alpha", "beta")
            return(list(para=para, source="FRECHETcop"))
         }
      } else {
         warning("could not determine parameters by rho and tau, returning NULL")
         return(NULL)
      }
   }
   para <- as.numeric(para); a <- para[1]; b <- para[2]
   if(par2rhotau) {
      if(a >= 0 & b >= 0 & (a + b) <= 1) {
         tau <- ((a-b)*(a+b+2))/3; rho <- a - b
         return(list(rho=rho, tau=tau, para=para, source="FRECHETcop"))
      } else {
         warning("parameters a and b not a,b >= 0 & a + b <= 1, returning NULL")
         return(NULL)
      }
   }
   if(a >= 0 & b >= 0 & (a + b) <= 1) {
      return(a*M(u,v) + (1-a-b)*P(u,v) + b*W(u,v))
   }
   warning("parameters a and b not a,b >= 0 & a + b <= 1, returning NULL")
   return(NULL)
}

#\dontrun{
#  plot(c(0,1), c(0,1), type="n")
#  for(tau in seq(-1,1, by=0.01)) {
#    for(rho in seq(-1,1, by=0.01)) {
#      #message(tau, " and ", rho)
#      fcop <- FRECHETcop(rho=rho, tau=tau)
#      if(! is.na(fcop$para[1])) {
#         points(fcop$para[1], fcop$para[2])
#      }
#    }
#  }
#}

