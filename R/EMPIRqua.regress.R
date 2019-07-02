"EMPIRqua.regress" <-
function(f=0.5, u=seq(0.01,0.99, by=0.01), empinv=NULL,
         lowess=FALSE, f.lowess=1/5, ...) {
   cols <- attributes(empinv)$colnames
   ix <- 1:length(cols)
   ix.needed <- ix[as.character(cols) == as.character(f)]
   if(length(ix.needed) != 1) {
      warning("f value does not match against row names in empinv, ",
              "likely source of this is a real number with too many digits ",
              "relative to a keyed entry")
      return(data.frame(U=NA, V=NA))
   }
   U.available <- attributes(empinv)$rownames
   V.available <- empinv[,ix.needed]
   UVdf <- data.frame(U=U.available, V=V.available)
   UVdf <- UVdf[complete.cases(UVdf),]
   # we know that the x are given in ordered seqeuence to so avoid
   # the warning
   # In regularize.values(x, y, ties, missing(ties)) :
   # collapsing to unique 'x' values
   V <- approx(U.available, y=V.available, xout=u, rule=2, ties="ordered")$y
   z <- data.frame(U=u,V=V)
   if(lowess) {
      lws <- lowess(z$U, y=z$V, f=f.lowess)
      z <- data.frame(U=lws$x, V=lws$y)
      z$V[z$V < 0] <- 0; z$V[z$V > 1] <- 1 # -WHA 2019/06/28 (found a case)
   }
   return(z)
}
