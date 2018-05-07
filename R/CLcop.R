"CLcop" <- function(u, v, para=NULL, tau=NULL, ...) {
    if(is.null(para)) {
      if(is.null(tau)) {
        tau <- cor(u,v, method="kendall")
      }
      if(tau > 0.975) {
        warning("tau > 0.975, simCOP might begin to show rare failures this high")
      }
      para <- 2*tau/(1-tau)
      names(para) <- "theta"
      names(tau)  <- "Kendall Tau"
      return(list(para=para, tau=tau))
    }
    if(length(para) == 1) {
       if(para[1] < -1) {
          warning("Parameter Theta < -1")
          return(NULL)
       }
       tau <- para/(para+2)
    } else {
       warning("Parameter Theta can not be a vector")
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
    para.small <- 1E-4 # simCOP(10000, cop=CLcop, para=-.0001) [no failures]
    if(abs(para) < para.small) return(u*v)
    if(tau > 0.975) return(M(u,v))
    cop <- u^-para + v^-para - 1
    cop[cop < 0] <- 0
    return(cop^(-1/para))
}
