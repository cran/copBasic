"FGMcop" <- function(u, v, para=c(NA, 1, 1), ...) {
    if(is.null(para)) {
      warning("self parameter estimation not supported")
      return(NULL)
    }
    
    if(length(para) == 1) {
      para <- c(para, 1,1)
    }
    para <- as.numeric(para)
    if(length(para) != 3) {
      warning("copula requires three parameters")
      return(NULL)
    }
    d <- para[1]; a <- para[2]
    n <- as.integer(para[3])
    lwr <- -min(c(1,1/(n*a^2))); upr <- 1/(n*a)
    if(is.na(d)) {
      warning("parameter Theta is NA")
      return(NULL)
    } else if(d < lwr) {
      warning("parameter Theta < ", lwr)
      return(NULL)      
    } else if(d > upr) {
      warning("parameter Theta > ", upr)
      return(NULL)      
    }
    if(a <= 0) {
      warning("parameter Alpha <= 0")
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

    return(u*v*(1+d*(1-u^a)*(1-v^a))^n)
}
