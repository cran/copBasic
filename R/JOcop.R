"JOcopB5" <- function(u, v, para=NULL, tau=NULL, ...) {
    parameter_large <- 1000
    if(is.null(para)) {
      if(is.null(tau)) tau <- cor(u,v, method="kendall")
      "ktau" <- function(d) { a <- 2/(2-d)
         if(! is.finite(a)) return(tau - (1 - trigamma(2)))
         tau - (1 + 2/(2-d)*(digamma(2)-digamma(1 + 2/d)))
      }
      rt <- NULL
      try(rt <- uniroot(ktau, interval=c(1, parameter_large)))
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
    if(d >= parameter_large) return(pmin(u,v)) # M(u,v)
    cop <- 1 - ( (1-u)^d + (1-v)^d -
                ((1-u)^d)*((1-v)^d) )^(1/d)
    if(d >= 1000) {
       wnt <- u > 0.45     | v > 0.45
       cop[wnt] <- pmin(u[wnt], v[wnt]) # M(u,v)
    } else if(d >= 900) {
       wnt <- u > 0.50     | v > 0.50
       cop[wnt] <- pmin(u[wnt], v[wnt]) # M(u,v)
    } else if(d >= 800) {
       wnt <- u > 0.55     | v > 0.55
       cop[wnt] <- pmin(u[wnt], v[wnt]) # M(u,v)
    } else if(d >= 700) {
       wnt <- u > 0.60     | v > 0.60
       cop[wnt] <- pmin(u[wnt], v[wnt]) # M(u,v)
    } else if(d >= 600) {
       wnt <- u > 0.65     | v > 0.65
       cop[wnt] <- pmin(u[wnt], v[wnt]) # M(u,v)
    } else if(d >= 500) {
       wnt <- u > 0.70     | v > 0.70
       cop[wnt] <- pmin(u[wnt], v[wnt]) # M(u,v)
    } else if(d >= 400) {
       wnt <- u > 0.75     | v > 0.75
       cop[wnt] <- pmin(u[wnt], v[wnt]) # M(u,v)
    } else if(d >= 300) {
       wnt <- u > 0.90     | v > 0.90
       cop[wnt] <- pmin(u[wnt], v[wnt]) # M(u,v)
    } else if(d >= 200) {
       wnt <- u > 0.92     | v > 0.92
       cop[wnt] <- pmin(u[wnt], v[wnt]) # M(u,v)
    } else if(d >= 100) {
       wnt <- u > 0.97     | v > 0.97
       cop[wnt] <- pmin(u[wnt], v[wnt]) # M(u,v)
    } else if(d >= 90)  {
       wnt <- u > 0.998    | v > 0.998
       cop[wnt] <- pmin(u[wnt], v[wnt]) # M(u,v)
    } else if(d >= 85)  {
       wnt <- u > 0.999    | v > 0.999
       cop[wnt] <- pmin(u[wnt], v[wnt]) # M(u,v)
    } else if(d >= 80)  {
       wnt <- u > 0.9995   | v > 0.9995
       cop[wnt] <- pmin(u[wnt], v[wnt]) # M(u,v)
    } else if(d >= 70)  {
       wnt <- u > 0.9999   | v > 0.9999
       cop[wnt] <- pmin(u[wnt], v[wnt]) # M(u,v)
    } else if(d >= 60)  {
       wnt <- u > 0.99999  | v > 0.99999
       cop[wnt] <- pmin(u[wnt], v[wnt]) # M(u,v)
    }
    return(cop)
}
