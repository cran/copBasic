"stabtaildepf" <-
function(uv=NULL, xy=NULL, k=function(n) as.integer(0.5*n),
             levelset=TRUE, ploton=TRUE, title=TRUE, delu=0.01,
             smooth=FALSE, ...) {
   if(is.null(uv)) {
      warning(" 'uv' is NULL, returning NULL"); return(NULL)
   }
   if(is.null(xy) & ! levelset) {
      warning(" 'xy' is NULL and levelset=FALSE, returning NULL"); return(NULL)
   }
   n <- length(uv[,1])
   if(is.function(k)) k <- k(n)
   if(levelset) {
      if(ploton) {
         plot(c(0,1), c(0,1), type="n",
              xlab="RELATIVE DISTANCE TO X END POINT",
              ylab="RELATIVE DISTANCE TO Y END POINT")
         if(title) mtext("Stable Tail Dependence Function")
      }
      if(! smooth) {
         afunc <- function(y, c=NULL, x=NULL, k=NULL, ...) {
          abs(c - stabtaildepf(xy=c(x,y), uv=uv, k=k, levelset=FALSE, ...)) }
         ZZ <- new.env()
         for(c in seq(0.1,1,by=0.1)) {
            x <- seq(0,c, by=delu)
            y <- sapply(x, function(j) {
                      optimise(afunc, interval=c(0,1), c=c, x=j, k=k)$minimum })
            x <- c(x,c); y <- c(y,0)
            lines(x,y, ...)
            assign(as.character(c), data.frame(x=x,y=y), envir=ZZ)
         }
         return(as.list(ZZ))
      } else {
         Hlis <- stabtaildepf(xy=NA, uv=uv, smooth=TRUE, levelset=FALSE, k=k)
         bfunc <- function(y, c=NULL, x=NULL, ...) {
                                  sol <- sapply(y, function(g) { 2*sum(Hlis$p3 *
                                         sapply(1:Hlis$Nn, function(i) {
                                  max(c(Hlis$Wn[i]*x, (1-Hlis$Wn[i])*g)) })) })
                                  abs(c - sol) }
         ZZ <- new.env()
         for(c in seq(0.1,1,by=0.1)) {
            x <- seq(0,c,by=delu)
            y <- sapply(x, function(j) {
                           optimise(bfunc, interval=c(0,1), c=c, x=j)$minimum })
            x <- c(x,c); y <- c(y,0)
            lines(x,y, ...)
            assign(as.character(c), data.frame(x=x,y=y), envir=ZZ)
         }
         return(as.list(ZZ))
      }
   } else {
      if(smooth) {
         WS <- psepolar(uv, ...)$table
         Sf <- sort(WS$Shat, decreasing=TRUE)[k+1]
         n <- length(WS$Shat); ix <- 1:n
         In <- ix[WS$Shat > Sf]; Nn <- length(In)
         Wn <- WS$What[In]
         Wbar <- mean(Wn); Sw <- 1/var(Wn)
         p3 <- (1 - (Wbar - 0.5)*Sw*(Wn - Wbar))/Nn
         zz <- list(Nn=Nn, Wn=Wn, p3=p3)
         return(zz)
      } else {
         R1 <- rank(uv[,1]); R2 <- rank(uv[,2])
         lhat <- (1/k)*sum((R1 > n + 1 - k*xy[1] | R2 > n + 1 - k*xy[2]))
         return(lhat)
      }
   }
}
