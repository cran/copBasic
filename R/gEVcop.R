"gEVcop" <- function(u,v, para=NULL, ...) {
   if(is.null(para)) {
      warning("no parameters, NULL")
   }
   if(length(para) == 1) {
      if(para[1] < 0 | para[1] > +1) {
         warning("Parameter not in 0 <= rho <= +1")
      }
   } else {
      warning("Parameter rho must be given")
      return(NULL)
   }
   if(length(u) > 1 & length(v) > 1 & length(u) != length(v)) {
      warning("length u = ", length(u), " and length v = ", length(v))
      warning("longer object length is not a multiple of shorter object length, ",
              "no recycling")
      return(NA)
   }
   if(length(u) == 1) {
      u <- rep(u, length(v))
   } else if(length(v) == 1) {
      v <- rep(v, length(u))
   }
   "A.gEV" <- function(x,y, rho) {
     a <- x/(x+y)
     A <- (x+y-2*rho*sqrt(x*y))/(1-rho^2)
     r1 <- rho^2/(1+rho^2); r2 <- 1/(1+rho^2)
     t1 <- (0 <= a & a <= r1)
     t2 <- (r1 < a & a <= r2)
     t3 <- (r2 < a & a <= 1 )
     A[a < 0] <- NA
     A[t1] <- y[t1]
     A[t3] <- x[t3]
     A[a > 1] <- NA
     return(A)
   }
   return(exp(-A.gEV(-log(u), -log(v), para[1])))
}
#https://doi.org/10.1016/j.advwatres.2006.08.001
