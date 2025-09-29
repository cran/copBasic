"MOcop" <- function(u,v, para=NULL, lcomCOP=NULL, ...) { # Marshall-Olkin copula
   if(! is.null(lcomCOP)) {
     ofunc <- function(par, srho=NA, T3_12=NA, T3_21=NA) {
       lcm <- lcomCOP(cop=MOcop, para=pnorm(par))
       return((lcm$lcomUV[2] - srho )^2 + # look carefully, the 2, 3, 3 index
              (lcm$lcomUV[3] - T3_12)^2 + # use on the lcm list are correct, so do not
              (lcm$lcomVU[3] - T3_21)^2)  # expect to see 1, 2, 3 or 2, 3, 4.
     }
     rt <- NULL
     try(rt <- optim(c(0,0), ofunc, srho=lcomCOP[1], T3_12=lcomCOP[2], T3_21=lcomCOP[3]), silent=TRUE)
     if(is.null(rt)) {
       warning("could not optim the alpha, beta given the rhotau")
       return(NULL)
     }
     rt$para <- pnorm(rt$par)
     return(rt)
   }

   if(length(u) == 1) {
     u <- rep(u, length(v))
   } else if (length(v) == 1) {
     v <- rep(v, length(u))
   }
   alpha <- para[1]; beta <- para[2]
   f <- vector(mode="numeric", length(u))
   wnt <- u^alpha >= v^beta
   f[  wnt] <- u[  wnt]^(1-alpha) * v[  wnt]
   f[! wnt] <- u[! wnt]           * v[! wnt]^(1-beta)
   return(f)
}

