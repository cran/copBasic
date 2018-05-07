"AMHcop" <- function(u, v, para=NULL, tau=NULL, ...) {
    if(is.null(para)) {
      if(is.null(tau)) tau <- cor(u,v, method="kendall")
      "ktau" <- function(d) {
        stau <- (3*d-2)/(3*d) - (2*(1-d)^2*log(1-d))/(3*d^2)
        tau - stau
      }
      rt <- NULL
      try(rt <- uniroot(ktau, interval=c(-1,1-.Machine$double.eps)))
      if(! is.null(rt)) {
         para <- rt$root
         names(para) <- "theta"
         names(tau)  <- "Kendall Tau"
         return(list(para=para, tau=tau))
      } else {
         warning("could not solve for parameter Theta")
         return(NULL)
      }
    }
    
    rev <- FALSE
    if(length(para) == 2) {
      rev <- para[2]; para <- para[-2]
    }
    para <- as.numeric(para)
    if(length(para) != 1) {
      warning("copula only uses one parameter")
      return(NULL)
    }
    d <- para[1]
    if(d < -1) {
      warning("parameter Theta < -1")
      return(NULL)
    }
    if(d > 1) {
      warning("parameter Theta > +1")
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

    if(rev) {
      cop <- u + v - 1 + ((1-u)*(1-v))/(1-d*u*v)
      cop[is.nan(cop)] <- 1
    } else {
      cop <- (u*v)/(1-d*(1-u)*(1-v))
      cop[is.nan(cop)] <- 0
    }
    return(cop)
}
