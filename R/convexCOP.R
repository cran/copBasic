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

  weights <-  weights/sum(weights)

  cop.names  <- nm[cops.idx]; para.names <- nm[pars.idx]

  GG <- sapply(1:n, function(i) {
                    the.cop  <- get(cop.names[i],  para)
                    the.para <- get(para.names[i], para)
                    weights[i]*COP(u,v, cop=the.cop, para=the.para) })
  return(sum(GG))
}

