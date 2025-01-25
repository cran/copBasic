"RAYcop" <-
function(u, v, para=NULL, rho=NULL, method=c("default"),
               rel.tol=.Machine$double.eps^0.5, ...) {

  if(! is.null(rho)) {
    Rho2Theta <- function(rho) {
       coes <- c(1.32682824, -0.38876290, 0.09072305, -0.02921836)
       sapply(rho, function(r) {
       coes[1]*r^1 + coes[2]*r^2 + coes[3]*r^4 + coes[4]*r^6 })
    }
    p <- Rho2Theta(rho)
    names(p) <- "Theta"
    return(p)
  }

  if(length(u) > 1 & length(v) > 1 & length(u) != length(v)) {
    warning("length u = ", length(u), " and length v = ", length(v))
    warning("longer object length is not a multiple of shorter object length, ", "no recycling")
    return(NA)
  }
  if(length(u) == 1) {
    u <- rep(u, length(v))
  } else if(length(v) == 1) {
    v <- rep(v, length(u))
  }


  p <- NULL
  if(is.list(para)) {
    if(exists("para",   para)) p      <- para$para
    if(exists("method", para)) method <- para$method
  } else {
    p <- para[1]
  }
  if(is.null(p)) {
    warning("parameter from para to become p in the copula is NULL")
    return(NULL)
  }

  LARGE <- 0.9999
  if(para > LARGE) { # by empirical study of the "default" method
    return( M(u,v) )
  }

  method <- match.arg(method)

  # NEED TO STUDY MORE marcumq.chi <- function(a, b, nu=1) pchisq(b^2, df=2 * nu, ncp=a^2, lower.tail=FALSE)
  # NEED TO STUDY MORE marcumq.ser <- function(a, b, nu=1, n=100) {
  # NEED TO STUDY MORE   k <- exp(-(a^2+b^2)/2)
  # NEED TO STUDY MORE   s <- k*sum(sapply(seq(nu, n, by=1), function(t) (b/a)^t * besselI(a*b, nu=t)))
  # NEED TO STUDY MORE   1 - s
  # NEED TO STUDY MORE }
  # NEED TO STUDY MORE marcumq <- marcumq.chi
  # NEED TO STUDY MORE sapply(seq_len(length((u))), function(i) {
  # NEED TO STUDY MORE   a1 <- -log(1-u[i])
  # NEED TO STUDY MORE   if(is.infinite(a1)) return(v[i])
  # NEED TO STUDY MORE   a2 <- -log(1-v[i])
  # NEED TO STUDY MORE   if(is.infinite(a2)) return(u[i])
  # NEED TO STUDY MORE   a1 <- exp(log(a1) - log(1-p))
  # NEED TO STUDY MORE   a2 <- exp(log(a2) - log(1-p))
  # NEED TO STUDY MORE   a3 <- marcumq.chi(sqrt(2*a1), sqrt(2*p*a2))
  # NEED TO STUDY MORE   a4 <- marcumq.chi(sqrt(2*p*a1), sqrt(2*a2))
  # NEED TO STUDY MORE   zz <- 1 + (1-v[i])*a3 - (1-u[i])*(1-a4)
  # NEED TO STUDY MORE   zz[zz < 0] <- 0
  # NEED TO STUDY MORE   zz[zz > 1] <- 1
  # NEED TO STUDY MORE   return(zz)
  # NEED TO STUDY MORE })

  sapply(seq_len(length((u))), function(i) {
    a1 <- -log(1-u[i]) # use of log1p() does not seem to help get deeper into the tail
    if(is.infinite(a1)) return(v[i])
    a2 <- -log(1-v[i]) # use of log1p() does not seem to help get deeper into the tail
    if(is.infinite(a2)) return(u[i])
    a1 <- exp(log(a1) - log(1-p)) # use of log1p() does not seem to help get deeper into the tail
    a2 <- exp(log(a2) - log(1-p)) # use of log1p() does not seem to help get deeper into the tail
    #    print(c(a1, a2))
    #if(is.infinite(a1)) return(v[i])
    #if(is.infinite(a2)) return(u[i])
    #print(c(a1, a2))
    b1 <- exp(p*a2-a2)
    b2 <- exp(    -a1)
    #print(c(b1, b2))
    # The expon.scaled for besselI means that an 1/exp(-x) multiplier is needed. The formulation
    # below for the integral has a maximal amount of logarithmic opertions until exp() on the last call.
    i1 <- i2 <- NA
    if(method == "default") {
      try(i1 <- integrate(function(s) { x <- 2*sqrt(  s*a1)
                    exp( -s + x + log( besselI(x, 0, expon.scaled=TRUE) ) )
                  }, 0, p*a2, rel.tol=rel.tol)$value, silent=TRUE)
      if(is.na(i1)) return(min(u[i], v[i])) # slightly faster
      #if(is.na(i1)) return(M(u[i], v[i])) # perhaps not ideal, could try Monte Carlo integration
      # at this stage, and have in testing, can not seem to go beyond numerical blowup, so slipping
      # in M copula pinches the relation deep to the upper right and as para ---> 1 the M part
      # progresses down towards the lower left corner

      try(i2 <- integrate(function(t) { x <- 2*sqrt(p*t*a1)
                  exp( -t + x + log( besselI(x, 0, expon.scaled=TRUE) ) )
                  }, 0,   a2, rel.tol=rel.tol)$value, silent=TRUE)
      if(is.na(i2)) return(min(u[i], v[i])) # slightly faster
      #if(is.na(i2)) return(M(u[i], v[i]))
      #print(c(i1, i2))
      #t1 <- b1*(b2*i1 - 1)
      #t2 <- b2*i2
      #print(c(t1, t2))
      #zz <- 1 +  t1 - t2
      zz <- 1 + b1*(b2*i1 - 1) - b2*i2
    }
    zz[zz < 0] <- 0 # attempt to catch any truncation errors should they ever exist
    zz[zz > 1] <- 1 # attempt to catch any truncation errors should they ever exist
    return(zz)
  })
}

#RAYcop(0.5, 0.5, para=0.2)
#RAYcop(0.5, 0.5, para=0.2)
#RAYcop(0, 0.5, para=0.2)
#RAYcop(0, 0, para=0.2)

#UV <- simCOP(1000, cop=RAYcop, para=.991)
#ray1 <- lmomco::lmom2par(lmomco::vec2lmom(c(20, 5)), type="ray")
#ray2 <- lmomco::lmom2par(lmomco::vec2lmom(c(359, 17)), type="ray")
#UV   <- simCOP(1000, cop=RAYcop, para=0.7)
#x <- lmomco::quaray(UV[,1], ray1); y <- lmomco::quaray(UV[,2], ray2)
#plot(x, y)
#abline(v=lmomco::quaray(0.99, ray1))
#abline(h=lmomco::quaray(0.99, ray2))
