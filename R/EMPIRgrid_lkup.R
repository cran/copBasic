"EMPIRgrid_lkup" <-
function(u,v, para=NULL, ...) {
  if(length(u) > 1 & length(v) > 1 & length(u) != length(v)) {
    warning("length u = ", length(u), " and length v = ", length(v))
    warning("longer object length is not a multiple of shorter object length, ",
            "no recycling")
    return(NA)
  }
  if(           length(u) == 1) {
    u <- rep(u, length(v))
  } else if(    length(v) == 1) {
    v <- rep(v, length(u))
  }
  if(is.null(para)) {
    para <- EMPIRgrid_fast(cbind(u,v), gridonly=FALSE, ...)
  }
  if(! any(class(para) == "empirical.copula.grid")) {
    warning("expecting class to be 'empirical.copula.grid'")
  }
  if(! exists('empcop', para)) {
    warning("para does not contain in para$empcop for matrix of the gridded empirical copula")
  }
  # Extract the u and v probabilities of the gridded empirical copula and then make a vector of the
  ui <- para$u; vi <- para$v; ix <- seq_len(length(ui)) # indices for look up purposes to come.

  u[u < 0] <- 0; u[u > 1] <- 1
  v[v < 0] <- 0; v[v > 1] <- 1

  # https://en.wikipedia.org/wiki/Bilinear_interpolation
  x1i <- sapply(u, function(u) max( ix[ui <= u]) ) # lower indices of x  --->  x1
  y1i <- sapply(v, function(v) max( ix[vi <= v]) ) # lower indices of y  --->  y1
  x2i <- sapply(u, function(u) min( ix[ui >= u]) ) # upper indices of x  --->  y1
  y2i <- sapply(v, function(v) min( ix[vi >= v]) ) # upper indices of y  --->  y2

  x1 <- ui[x1i]; x2 <- ui[x2i]; y1 <- vi[y1i]; y2 <- vi[y2i]     # vectors of probs. from indices

  d   <- ( (x2 - x1) * (y2 - y1) )                               # vectorized denominator
  w11 <- ( (x2 - u ) * (y2 - v ) ) / d                           # vectorized weights
  w12 <- ( (x2 - u ) * (v  - y1) ) / d                           # vectorized weights
  w21 <- ( (u  - x1) * (y2 - v ) ) / d                           # vectorized weights
  w22 <- ( (u  - x1) * (v  - y1) ) / d                           # vectorized weights

  six  <- seq_len(length(u))  # indices of the sample values
  f11  <- sapply(six, function(k) para$empcop[ x1i[k], y1i[k] ])   # vectorized function values
  f12  <- sapply(six, function(k) para$empcop[ x1i[k], y2i[k] ])   # vectorized function values
  f21  <- sapply(six, function(k) para$empcop[ x2i[k], y1i[k] ])   # vectorized function values
  f22  <- sapply(six, function(k) para$empcop[ x2i[k], y2i[k] ])   # vectorized function values

  cop <- w11*f11 + w12*f12 + w21*f21 + w22*f22         # vectorized weighted mean computation
  cop[d == 0] <- f11[d == 0] # all denominators == 0 mean that the coordinates are on a single cell
  return(cop)
}

