# Gumbel-Hougaard Copula
"GHcop" <-
function(u, v, para=NULL, tau=NULL, tau.big=0.985, cor=NULL, ...) {

  if(! is.null(cor)) tau <- cor

  # tau.big <- 0.985 # tests show at least a failure rate < 1/10000 in simCOP
  # if tau is kept this low, which is a really large tau.
  para.big <- 1/(1-tau.big)

  if(is.null(para)) {
     if(is.null(tau)) {
        if(    length(u)     ==  length(v) &
              (length(u) > 1  &  length(v) > 1)) {
           tau <- cor(u,v, method="kendall")
        } else {
           warning("Argument para (Alpha) is NULL and u an v have unequal length",
                   "that prevents Alpha estimation by Kendall's Tau, returning NULL")
           return(NULL)
        }
        para <- 1/(1-tau)
        if(para < 1) {
           warning("negative correlation can not be fit with this copula, returning NULL")
           return(NULL)
        }
        names(para) <- "theta"; names(tau) <- "tau"
        return(list(para=para, tau=tau))
     } else {
        if(tau < 0) {
           warning("Kendall's Tau is negative, not compatible with this copula, returning NULL")
           return(NULL)
        }
        para <- 1/(1-tau)
        names(para) <- "theta"; names(tau) <- "tau"
        return(list(para=para, tau=tau))
     }
  }
  if(para < 1 | para > para.big ) {
     warning("Parameter para (Alpha) is not in [1, ",para.big,"] (numerically too big), returning M(u,v)")
     return(M(u,v))
  }
   if(length(u) > 1 & length(v) > 1 & length(u) != length(v)) {
    warning("length u = ",length(u), " and length v = ",length(v))
    warning("longer object length is not a multiple of shorter object length, no recycling")
    return(NA)
  }
  # The extra hassle of vectorization made here is to handle situations
  # in which nested integrals are used where uneven vectors can be passed
  if(length(u) == 1) {
     u <- rep(u, length(v))
  } else if(length(v) == 1) {
     v <- rep(v, length(u))
  }
  cop <- ((-log(u))^para + (-log(v))^para)^(1/para)
  return(exp(-cop))
}


