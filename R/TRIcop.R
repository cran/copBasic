"TRIcop" <-
function(u, v, para=NULL, rhotau=NULL, ...) {
  if(is.null(para)) {
    if(is.null(rhotau)) {
      warning("returning NULL because para and rhotau are NULL")
      return(NULL)
    }
    para <- (rhotau + 1) / 2
    names(para) <- "theta"
    names(rhotau)  <- "Spearman Rho or Kendall Tau"
    return(list(para=para, rhotau=rhotau))
  }
  p <- para[1]

  if(length(u) > 1 & length(v) > 1 & length(u) != length(v)) {
    warning("length u = ", length(u), " and length v = ", length(v))
    warning("longer object length is not a multiple of shorter object length, ",
    "no recycling")
    return(NA)
  }
  if(       length(u) == 1) {
    u <- rep(u, length(v))
  } else if(length(v) == 1) {
    v <- rep(v, length(u))
  }

  r <- 1 - (1-p)*v
  cop <- u
  wnt <- 0 <= p*v & p*v <  u & u <  r
  cop[wnt] <- p*v[wnt]
  wnt <- p <= r   & r   <= u & u <= 1
  cop[wnt] <-   u[wnt] + v[wnt] - 1
  return(cop)
}
