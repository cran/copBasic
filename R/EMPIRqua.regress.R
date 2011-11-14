"EMPIRqua.regress" <-
function(F=0.5, U=seq(0.01,0.99, by=0.01), empinv=NULL,
         lowess=FALSE, f=1/5, ...) {
   cols <- attributes(empinv)$colnames
   ix <- 1:length(cols)
   ix.needed <- ix[as.character(cols) == as.character(F)]
   if(length(ix.needed) != 1) {
      warning("F value does not match against column names in empinv")
      return(data.frame(U=NA, V=NA))
   }
   U.available <- attributes(empinv)$rownames
   V.available <- empinv[,ix.needed]
   UVdf <- data.frame(U=U.available, V=V.available)
   UVdf <- UVdf[complete.cases(UVdf),]
   V <- approx(U.available, y=V.available, xout=U, rule=2)$y
   z <- data.frame(U=U,V=V)
   if(lowess) {
      lws <- lowess(z$U, y=z$V, f=f)
      z <- data.frame(U=lws$x, V=lws$y)
   }
   return(z)
}
