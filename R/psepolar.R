"psepolar" <- function(u, v=NULL, f=0.90, ...) {
   if(is.null(v)) {
      if(length(names(u)) != 2) {
         warning("a data.frame having only two columns is required")
         return(NULL)
      }
      v <- u[,2]; u <- u[,1] # v must come before u
   }
   if(length(u) != length(v)) {
      warning("argument(s) or implied arguments u and v are unequal ",
              "in length, returning NULL")
      return(NULL)
   }
   n <- length(u)
   Xstar  <- n/(n+1-rank(u)); Ystar <- n/(n+1-rank(v))
   Shat   <- Xstar + Ystar;   What  <- Xstar/Shat
   FXhat1 <- 1 - 1/Xstar;       FYhat1 <- 1 - 1/Ystar
   FXhat3 <- (rank(u) - 1/2)/n; FYhat3 <- (rank(v) - 1/2)/n
   Sf     <- lmomco::dat2bernqua(f, Shat, ...)
   zz <- data.frame(U=u, V=v, Xstar=Xstar, Ystar=Ystar,
                    FXhat1=FXhat1, FYhat1=FYhat1,
                    FXhat3=FXhat3, FYhat3=FYhat3,
                    What=What, Shat=Shat, Shat_ge_Sf=Shat >= Sf)
   return(list(table=zz, Sf=Sf))
}
