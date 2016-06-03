"spectralmeas" <-
function(u, v=NULL, w=NULL, f=0.90, snv=FALSE,
                            smooth=FALSE, nu=100, pdf=FALSE, ...) {
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
   if(is.null(w)) {
      warning("need the angles 'w'"); return(NULL)
   }
   if(pdf) smooth <- TRUE

   polar <- psepolar(u,v, f=f, ...)
   Sf <- polar$Sf; WS <- polar$table
   n <- length(WS$Shat); ix <- 1:n
   H3 <- vector(mode="numeric")
   for(i in 1:length(w)) {
     the.w <- w[i]
     In <- ix[WS$Shat >= Sf]; Nn <- length(In)
     Wn <- WS$What[In]
     Wbar <- mean(Wn); Sw <- 1/var(Wn)
     A <- (1 - (Wbar - 0.5)*Sw*(Wn - Wbar))
     p3 <- A/Nn
     if(! smooth) {
       H3[i] <- sum(p3*as.numeric(Wn <= the.w))
       next;
     }
     extra.term <- rep(1, length(Wn))
     if(pdf) {
       extra.term <- sapply(Wn, function(w) dbeta(the.w, nu*w, nu*(1-w)))
     } else if(smooth) {
       extra.term <- sapply(Wn, function(w) pbeta(the.w, nu*w, nu*(1-w)))
     }
     H3[i] <- sum(p3*extra.term)
   }
   ifelse(snv, return(qnorm(H3)), return(H3))
}
