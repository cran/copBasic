"AMHcop" <- function(u, v, para=NULL, rho=NULL, tau=NULL,
                            fit=c("rho","tau"), ...) {
    if(is.null(para)) {
      fit <- match.arg(fit)
      if(is.null(tau) & is.null(rho)) {
        if(fit == "rho") {
          rho <- cor(u,v, method="spearman")
        } else {
          tau <- cor(u,v, method="kendall")
        }
      }
      rt <- NULL
      if(is.null(rho)) {
        if(tau < (5 - 8*log(2))/3 | tau > 1/3) {
          warning("Kendall tau=",tau," is outside limits attainable")
          return(NULL)
        }
        "ktau" <- function(t) {
          stau <- (3*t-2)/(3*t) - (2*(1-t)^2*log(1-t))/(3*t^2)
          tau - stau
        }
        try(rt <- uniroot(ktau, interval=c(-1,1-.Machine$double.eps)))
        if(! is.null(rt)) {
          para <- rt$root
          names(para) <- "theta"
          names(tau)  <- "Kendall Tau"
          return(list(para=para, tau=tau))
        } else {
          warning("could not solve for parameter Theta via Kendall Tau")
          return(NULL)
        }
      } else {
        if(rho < 33 - 48*log(2) | rho > 4*pi^2 - 39) {
          warning("Spearman rho=",rho," is outside limits attainable")
          return(NULL)
        }
        "srho" <- function(t) {
          rho - sum(sapply(1:100, function(k) 3*t^k/choose(k+2, 2)^2))
        }
        try(rt <- uniroot(srho, interval=c(-1,1-.Machine$double.eps)))
        if(! is.null(rt)) {
          para <- rt$root
          names(para) <- "theta"
          names(rho)  <- "Spearman Rho"
          return(list(para=para, rho=rho))
        } else {
          warning("could not solve for parameter Theta via Spearman Rho")
          return(NULL)
        }
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
