"W_N5p12a" <-
function(u,v, para=1, ...) {
  para <- as.integer(para[1])
  if(para < 1) {
    warning("para must be a positive integer")
    return(NA)
  }
  if(length(u) > 1 & length(v) > 1 & length(u) != length(v)) {
    warning("length u = ",length(u), " and length v = ",length(v))
    warning("longer object length is not a multiple of shorter object length, no recycling in M()")
    return(NA)
  }
  # The extra hassle of vectorization made here is to handle situations
  # in which nested integrals are used where uneven vectors can be passed
  if(length(u) == 1) {
     u <- rep(u, length(v))
  } else if(length(v) == 1) {
     v <- rep(v, length(u))
  }
  # Nelsen(2006,p173)
  n <- para
  zz <- sapply(seq_len(length(u)), function(i) {
           for(k in seq_len(n)) {
             x1 <- y1 <- (k-1)/n
             x2 <- y2 <-  k   /n
             if(u[i] < x1 | u[i] > x2 | v[i] < y1 | v[i] > y2) next
             return(pmax((k-1)/n, u[i]+v[i]-k/n))
           }; return(pmin(u[i], v[i])) })
  return(zz)
  #zz <- pmin(u,v) # otherwise
  #for(i in seq_len(m)) {
  #  for(k in seq_len(n)) {
  #    x1 <- y1 <- (k-1)/n
  #    x2 <- y2 <-  k   /n
  #    if(u[i] < x1 | u[i] > x2 | v[i] < y1 | v[i] > y2) next
  #    zz[i] <- pmax((k-1)/n, u[i]+v[i]-k/n)
  #  }
  #}
  # (u,v) \in [(k-1)/n, k/n] x [(k-1)/n, k/n] for k=1,2,..para
  # max((k-1)/n, u+v - k/n)
  return(zz)
}

#UV <- simCOP(n=100, cop=Wshuf, para=2)
