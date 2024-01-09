\encoding{utf8}
\name{CIRCcop}
\alias{CIRCcop}
\title{Copula of Circular Uniform Distribution}
\description{
The \emph{Circular copula} of the coordinates \eqn{(x, y)} of a point chosen at random on the unit circle (Nelsen, 2006, p. 56) is given by
\deqn{\mathbf{C}_{\mathrm{CIRC}}(u,v) = \mathbf{M}(u,v) \mathrm{\ for\ }|u-v| > 1/2\mathrm{,}}
\deqn{\mathbf{C}_{\mathrm{CIRC}}(u,v) = \mathbf{W}(u,v) \mathrm{\ for\ }|u+v-1| > 1/2\mathrm{,\ and}}
\deqn{\mathbf{C}_{\mathrm{CIRC}}(u,v) = \frac{u+v}{2} - \frac{1}{4} \mathrm{\ otherwise\ }\mathrm{.}}

The coordinates of the unit circle are given by
\deqn{\mathrm{CIRC}(x,y) = \biggl(\frac{\mathrm{cos}\bigl(\pi(u-1)\bigr)+1}{2}, \frac{\mathrm{cos}\bigl(\pi(v-1)\bigr)+1}{2}\biggr)\mathrm{.}}
}
\usage{
CIRCcop(u, v, para=NULL, as.circ=FALSE, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{Optional parameter list argument that can contain the logical \code{as.circ} instead;}
  \item{as.circ}{A logical, if true, to trigger the transformation \eqn{u = 1 - \mathrm{acos}(2x - 1) / \pi} and \eqn{v = 1 - \mathrm{acos}(2y - 1) / \pi} to convert \eqn{(X,Y)} coordinates of a uniform unit circle to the \eqn{(U,V)} in nonexceedance probability; and}
  \item{...}{Additional arguments to pass, if ever needed.}
}
\value{
  Value(s) for the copula are returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\examples{
CIRCcop(0.5, 0.5) # 0.25 quarterway along the diagonal upward to right
CIRCcop(0.5, 1  ) # 0.50 halfway across in horizontal direction
CIRCcop(1  , 0.5) # 0.50 halfway across in  vertical  direction

\dontrun{
  nsim <- 2000
  rtheta <- runif(nsim, min=0, max=2*pi) # polar coordinate simulation
  XY <- data.frame(X=cos(rtheta)/2 + 1/2, Y=sin(rtheta)/2 + 1/2)
  plot(XY, lwd=0.8, col="lightgreen", xaxs="i", yaxs="i", las=1,
           xlab="X OF UNIT CIRCLE OR NONEXCEEDANCE PROBABILITY U",
           ylab="Y OF UNIT CIRCLE OR NONEXCEEDANCE PROBABILITY V")
  UV <- simCOP(nsim, cop=CIRCcop, lwd=0.8, col="salmon3", ploton=FALSE)
  theta <- 3/4*pi+0.1 # select a point on the upper left of the circle
  x <- cos(theta)/2 + 1/2; y <- sin(theta)/2 + 1/2 # coordinates
  H <- CIRCcop(x, y, as.circ=TRUE) # 0.218169  # Pr[X <= x & Y <= y]
  points(x, y, pch=16, col="forestgreen", cex=2)
  segments(0, y, x, y, lty=2, lwd=2, col="forestgreen")
  segments(x, 0, x, y, lty=2, lwd=2, col="forestgreen")
  Hemp1 <- sum(XY$X <= x & XY$Y <= y) / nrow(XY) # about 0.22 as expected
  u <- 1-acos(2*x-1)/pi; v <- 1-acos(2*y-1)/pi
  segments(0, v, u, v, lty=2, lwd=2, col="salmon3")
  segments(u, 0, u, v, lty=2, lwd=2, col="salmon3")
  points(u, v, pch=16, cex=2,        col="salmon3")
  arrows(x, y, u, v, code=2, lwd=2, angle=15) # arrow points from (X,Y) coordinate
  # specified by angle theta in radians on the unit circle to the corresponding
  # coordinate in (U,V) domain of uniform circular distribution copula
  Hemp2 <- sum(UV$U <= u & UV$V <= v) / nrow(UV) # about 0.22 as expected
  # Hemp1 and Hemp2 are about equal to each other and converge as nsim
  # gets very large, but the origin of the simulations to get to each
  # are different: (1) one in polar coordinates and (2) by copula.
  # Now, draw the level curve for the empirical Hs and as nsim gets large the two
  # lines will increasingly plot on top of each other.
  lshemp1 <- level.setCOP(cop=CIRCcop, getlevel=Hemp1, lines=TRUE, col="blue", lwd=2)
  lshemp2 <- level.setCOP(cop=CIRCcop, getlevel=Hemp2, lines=TRUE, col="blue", lwd=2)
  txt <- paste0("level curves for Pr[X <= x & Y <= y] and\n",
                "level curves for Pr[U <= u & V <= v],\n",
                "which equal each other as nsim gets large")
  text(0.52, 0.52, txt, srt=-46, col="blue") # }

\dontrun{
  # Nelsen (2007, ex. 3.2, p. 57) # Singular bivariate distribution with
  # standard normal margins that is not bivariate normal.
  U <- runif(500); V <- simCOPmicro(U, cop=CIRCcop)
  X  <- qnorm(U, mean=0, sd=1);    Y <- qnorm(V, mean=0, sd=1)
  plot(X,Y, main="Nelsen (2007, ex. 3.2, p. 57)", xlim=c(-4,4), ylim=c(-4,4),
            lwd=0.8, col="turquoise")
  rug(X, side=1, col=grey(0,0.5), tcl=0.5)
  rug(Y, side=2, col=grey(0,0.5), tcl=0.5) #}

\dontrun{
  DX <- c(5, 5, -5, -5); DY <- c(5, 5, -5, -5); D <- 6; R <- D/2
  plot(DX, DY, type="n", xlim=c(-10, 10), ylim=c(-10,10), xlab="X", ylab="Y")
  abline(h=DX, lwd=2, col="seagreen"); abline(v=DY, lwd=2, col="seagreen")
  for(i in seq_len(length(DX))) {
    for(j in seq_len(length(DY))) {
      UV <- simCOP(n=30, cop=CIRCcop, pch=16, col="darkgreen", cex=0.5, graphics=FALSE)
      points(UV[,1]-0.5, UV[,2]-0.5, pch=16, col="darkgreen", cex=0.5)
      XY <- data.frame(X=DX[i]+sign(DX[i])*D*(cos(pi*(UV$U-1))+1)/2-sign(DX[i])*R,
                       Y=DY[j]+sign(DY[j])*D*(cos(pi*(UV$V-1))+1)/2-sign(DY[j])*R)
      points(XY, lwd=0.8, col="darkgreen")
    }
    abline(h=DX[i]+R, lty=2, col="seagreen"); abline(h=DX[i]-R, lty=2, col="seagreen")
    abline(v=DY[i]+R, lty=2, col="seagreen"); abline(v=DY[i]-R, lty=2, col="seagreen")
  } #}

\dontrun{
  para <- list(cop1=CIRCcop, para1=NULL, cop2=W, para2=NULL, alpha=0.8, beta=0.8)
  UV <- simCOP(n=2000, col="darkgreen", cop=composite2COP, para=para)
  XY <- data.frame(X=(cos(pi*(UV$U-1))+1)/2, Y=(cos(pi*(UV$V-1))+1)/2)
  plot(XY, type="n", xlab=paste0("X OF CIRCULAR UNIFORM DISTRIBUTION OR\n",
                                 "NONEXCEEDANCD PROBABILITY OF U"),
                     ylab=paste0("Y OF CIRCULAR UNIFORM DISTRIBUTION OR\n",
                                 "NONEXCEEDANCD PROBABILITY OF V"))
  JK <- data.frame(U=1 - acos(2*XY$X - 1)/pi, V=1 - acos(2*XY$Y - 1)/pi)
  segments(x0=UV$U, y0=UV$V, x1=XY$X, y1=XY$Y, col="lightgreen", lwd=0.8)
  points(XY, lwd=0.8, col="darkgreen")
  points(JK, pch=16,  col="darkgreen", cex=0.5)

  t <- seq(0.001, 0.999, by=0.001)
  t <- diagCOPatf(t, cop=composite2COP, para=para)
  AB <- data.frame(X=(cos(pi*(t-1))+1)/2, Y=(cos(pi*(t-1))+1)/2)
  lines(AB, lwd=4, col="seagreen") #}
}
\keyword{copula (formulas)}
\keyword{copula}
\keyword{cicular copula}
\keyword{Nelsen (2006) Examples and Exercises}
