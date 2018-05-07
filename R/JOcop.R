"JOcopB5" <- function(u, v, para=NULL, tau=NULL, ...) {
    if(is.null(para)) {
      if(is.null(tau)) tau <- cor(u,v, method="kendall")
      "ktau" <- function(d) { a <- 2/(2-d)
         if(! is.finite(a)) return(tau - (1 - trigamma(2)))
         tau - (1 + 2/(2-d)*(digamma(2)-digamma(1 + 2/d)))
      }
      rt <- NULL
      try(rt <- uniroot(ktau, interval=c(1,500)))
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
    if(length(para) != 1) {
       warning("copula only uses one parameter")
       return(NULL)
    }
    if(para[1] < 1) {
      warning("parameter Theta < 1")
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

    d <- para[1]
    if(d >= 500) return(M(u,v))
    cop <- 1 - ( (1-u)^d + (1-v)^d -
                ((1-u)^d)*((1-v)^d) )^(1/d)
    if(d >= 400) {
       cop[u > 0.75     | v > 0.75]    <- M(u,v)
    } else if(d >= 300) {
       cop[u > 0.94     | v > 0.94]    <- M(u,v)
    } else if(d >= 200) {
       cop[u > 0.95     | v > 0.95]    <- M(u,v)
    } else if(d >= 100) {
       cop[u > 0.97     | v > 0.97]    <- M(u,v)
    } else if(d >= 90)  {
       cop[u > 0.998    | v > 0.998]   <- M(u,v)
    } else if(d >= 85)  {
       cop[u > 0.999    | v > 0.999]   <- M(u,v)
    } else if(d >= 80)  {
       cop[u > 0.9995   | v > 0.9995]  <- M(u,v)
    } else if(d >= 70)  {
       cop[u > 0.9999   | v > 0.9999]  <- M(u,v)
    } else if(d >= 60)  {
       cop[u > 0.99999  | v > 0.99999] <- M(u,v)
    }
    return(cop)
}
