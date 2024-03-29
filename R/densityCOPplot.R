"densityCOPplot" <-
 function(cop=NULL, para=NULL, deluv=0.002,
          getmatrix=c("none", "cdenzz", "cden"), n=0,
          ploton=TRUE, snv=TRUE, origins=TRUE,
          contour.col=1, contour.lwd=1.5, ...) {
   getmatrix <- match.arg(getmatrix)

   U <- V <- seq(deluv, 1-deluv, by=+deluv)
   nU <- length(U); nV <- length(V)
   qU <-  qnorm(U); qV <-  qnorm(V)
   cdenzz <- cden <- matrix(ncol=nU, nrow=nV)

   ZV <- dnorm(qnorm(V))
   for(i in 1:nU) {
      uvals      <- rep(U[i], nV)
      dcop       <- densityCOP(uvals, V, cop=cop, para=para, ...)
      cden[,i]   <- dcop
      cdenzz[,i] <- dcop * dnorm(qnorm(uvals)) * ZV
   }
   rownames(cden)   <- V;  colnames(cden)   <- U
   rownames(cdenzz) <- V;  colnames(cdenzz) <- U

   if(ploton) {
      if(snv) {
        xlim <- ylim <- c(-1, 1)*rep(max(abs(range(c(qU, qV))), 2))
        xlab <- "STANDARD NORMAL VARIATE OF U"
        ylab <- "STANDARD NORMAL VARIATE OF V"
      } else {
        xlim <- ylim <- c(0, 1)
        xlab <- "U, NONEXCEEDANCE PROBABILITY"
        ylab <- "V, NONEXCEEDANCE PROBABILITY"
      }
      plot(xlim, ylim, type="n", xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab)
      if(snv & origins) {
         abline(h=0, lty=2)
         abline(v=0, lty=2)
      }
   }
   if(n > 0) {
      UV <- simCOP(n=n, cop=cop, para=para, ploton=FALSE, points=TRUE, snv=snv,
                   pch=16, col=rgb(0, 0, 1, 0.3), cex=0.5, ...)
   }
   # The transposition t() is CRITICAL!!!!!!!! Let the 2nd example in
   # densityCOPplot(), which is highly asymmetrical be the canonical demonstration.
   if(snv) {
     contour(x=qU, y=qV, z=t(cdenzz), lwd=contour.lwd, cex=2, add=TRUE, col=contour.col, ...)
   } else {
     contour(x=U, y=V,   z=t(cdenzz), lwd=contour.lwd, cex=2, add=TRUE, col=contour.col, ...)
   }

   if(getmatrix == "cdenzz") {
      return(cdenzz)
   } else if(getmatrix == "cden") {
      return(cden)
   }
}


