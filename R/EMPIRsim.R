"EMPIRsim" <-
function(n=100, empgrid=NULL, kumaraswamy=FALSE,
         ploton=TRUE, points=TRUE, ...) {

  empinv <- EMPIRgridderinv(empgrid=empgrid, kumaraswamy=kumaraswamy)
  rows <- as.numeric(attributes(empinv)$rownames)
  ix <- 1:length(rows)
  if(ploton) {
    plot(c(0,1), c(0,1),
         type="n",
         xlab="U, NONEXCEEDANCE PROBABILITY",
         ylab="V, NONEXCEEDANCE PROBABILITY")
  }
  cols <- attributes(empinv)$colnames
  U <- vector(mode="numeric", length=n); V <- U
  for(i in 1:n) {
    u <- runif(1); t <- runif(1)
    ix.needed1 <- max(ix[rows <= u])
    ix.needed2 <- min(ix[rows >= u])
    if(ix.needed1 == 1) ix.needed1 <- 2
    if(ix.needed1 == ix.needed2) {
      v.available <- empinv[ix.needed1,]
      v <- approx(cols, y=v.available, xout=t, rule=2)$y
    } else {
      v.available1 <- empinv[ix.needed1,]
      v1 <- approx(cols, y=v.available1, xout=t, rule=2)$y
      v.available2 <- empinv[ix.needed2,]
      v2 <- approx(cols, y=v.available2, xout=t, rule=2)$y
      w1 <- u - rows[ix.needed1]
      w2 <- rows[ix.needed2] - u
      tw <- 1/w1 + 1/w2
      v <- (v1/w1 + v2/w2)/tw
    }
    U[i] <- u; V[i] <- v
  }
  if(points & ! is.null(dev.list())) points(U,V,...)
  z <- data.frame(U=U,V=V)
  return(z)
}

