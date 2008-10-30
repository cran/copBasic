"level.curvesCOP" <-
function(cop=NULL, para=NULL, ploton=TRUE, lines=TRUE,
         plotMW=FALSE, ramp=TRUE, delu=0.001, delt=0.10,
         getlevel=NULL, ...) {
  if(ploton) {
    plot(c(0,1), c(0,1), type="n",
         xlab="U, NONEXCEEDANCE PROBABILTIY",
         ylab="V, NONEXCEEDANCE PROBABILTIY")
  }
  z <- list(level=getlevel, U=NULL, V=NULL)
  for(t in seq(0+delt,1-delt,delt)) {  # for each level t
    u <- seq(t, 1-delu, by=delu); v <- u
    for(i in 1:length(u)) {
      v[i] <- COPinv(cop=cop, u[i], t, para=para)
    }
    #cat(c(t," is done\n"))
    if(lines) {
      if(ramp) {
        lines(u,v, lwd=(0.5+2*t), ...)
      } else {
        lines(u,v, ...)
      }
    }
    if(! is.null(getlevel) &
       isTRUE(all.equal(getlevel,t)) )
               z <- list(level=t, U=u, V=v)
  }
  if(plotMW) {
    abline(0,  1, lty=2, cex=0.5)
    abline(1, -1, lty=2, cex=0.5)
  }
  if(! is.null(getlevel)) return(z)
}
