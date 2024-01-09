"M_N5p12b" <-
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
             x1 <- (  k-1)/n; y1 <- (n-k  )/n
             x2 <-    k   /n; y2 <- (n-k+1)/n
             if(u[i] < x1 | u[i] > x2 | v[i] < y1 | v[i] > y2) next
              return( pmin(u[i]-(k-1)/n, v[i]-(n-k)/n) )
           }; return( pmax(u[i]+v[i]-1,0)) })
  return(zz)
  #zz <- pmax(u+v-1,0) # otherwise
  #for(i in seq_len(m)) {
  #  for(k in seq_len(n)) {
  #    x1 <- (  k-1)/n; y2 <- (n-k+1)/n
  #    x2 <-    k   /n; y1 <- (n-k  )/n
  #    #segments(x1, y1, x1=x2, y1=y2)
  #    if(u[i] < x1 | u[i] > x2 | v[i] < y1 | v[i] > y2) next
  #    zz[i] <- pmin(u[i]-(k-1)/n, v[i]-(n-k)/n)
  #  }
  #}
  # (u,v) \in [(k-1)/n, k/n] x [(n-k+1)/n, (n-k)/n] for k=1,2,..para
  # min(u - (k-1)/n, v - (n-k)/n)
  return(zz)
}

#UV <- simCOP(n=100, cop=Mshuf, para=20)
