"convexCOP" <- function(u,v, para, ...) {
  nm <- names(para)
  cops.idx <- grep("cop\\d+",  nm)
  pars.idx <- grep("para\\d+", nm)
  n <- length(cops.idx)
  p <- length(pars.idx)
  if(n != p) {
     warning("number of copulas and parameters are not equal")
     return()
  }
  if(length(grep("weights", nm)) == 0) {
     weights <- rep(1/n, n)
  } else {
     weights <- get("weights", para)
  }

  weights   <- weights/sum(weights)
  cop.names <- nm[cops.idx]; para.names <- nm[pars.idx]

  if(length(u) > 1 & length(v) > 1 & length(u) != length(v)) {
   warning("length u = ", length(u), " and length v = ", 
           length(v))
   warning("longer object length is not a multiple of shorter object length, ", 
           "no recycling")
   return(NA)
  }
  if(length(u) == 1) {
    u <- rep(u, length(v))
  } else if (length(v) == 1) {
    v <- rep(v, length(u))
  }

  HH <- sapply(1:length(u), function(k) {
          gg <- sapply(1:n, function(i) {
                    the.cop  <- get(cop.names[i],  para)
                    the.para <- get(para.names[i], para)
                    weights[i]*the.cop(u[k],v[k], para=the.para) })
          return(sum(gg)) })
  return(HH)
}

