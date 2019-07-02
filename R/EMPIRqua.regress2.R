"EMPIRqua.regress2" <-
function(f=0.5, v=seq(0.01,0.99, by=0.01), empinv=NULL,
         lowess=FALSE, f.lowess=1/5, ...) {
   rows <- attributes(empinv)$rownames
   ix <- 1:length(rows)
   ix.needed <- ix[as.character(rows) == as.character(f)]
   if(length(ix.needed) != 1) {
      warning("f value does not match against row names in empinv, ",
              "likely source of this is a real number with too many digits ",
              "relative to a keyed entry")
      return(data.frame(U=NA, V=NA))
   }
   V.available <- attributes(empinv)$colnames
   U.available <- empinv[ix.needed,]
   UVdf <- data.frame(U=U.available, V=V.available)
   UVdf <- UVdf[complete.cases(UVdf),]
   # we know that the x are given in ordered seqeuence to so avoid
   # the warning
   # In regularize.values(x, y, ties, missing(ties)) :
   # collapsing to unique 'x' values
   U <- approx(V.available, y=U.available, xout=v, rule=2, ties="ordered")$y
   z <- data.frame(U=U,V=v)
   if(lowess) {
      lws <- lowess(z$V, y=z$U, f=f.lowess)
      z <- data.frame(U=lws$y, V=lws$x)
      z$U[z$U < 0] <- 0; z$U[z$U > 1] <- 1 # -WHA 2019/06/28 (found a case)
   }
   return(z)
}

