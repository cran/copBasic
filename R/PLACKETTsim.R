"PLACKETTsim" <-
function(n, para=NULL, ...) {
  TT <- para[1]
  u <- runif(n)
  t <- runif(n)
  a <- t*(1-t)
  b <- TT + a*(TT-1)^2
  c <- 2*a*(u*TT^2 + 1 - u) + TT*(1-2*a)
  d <- sqrt(TT * (TT + 4*a*u*(1-u) * (1-TT)^2))
  v <- (c-(1-2*t)*d) / (2*b)
  z <- data.frame(U=u, V=v)
  return(z)
}
