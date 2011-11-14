\name{EMPIRgridderinv}
\alias{EMPIRgridderinv}
\title{ Derivatives of the Grid of the Bivariate Emprical Copula }
\description{
Generate a gridded representation of the bivariate empirical copula.
}
\usage{
EMPIRgridderinv(empgrid=NULL, kumaraswamy=FALSE, ...)
}
\arguments{
  \item{empgrid}{The grid from \code{\link{EMPIRgrid}}, and}
  \item{kumaraswamy}{A logical to trigger Kumaraswamy smoothing of the conditional quantile function;}
  \item{...}{Additional arguments to pass.}
}
\value{
  The gridded values of
}
\author{ W.H. Asquith }
\seealso{ \code{\link{EMPIRcop}}, \code{\link{EMPIRcopdf}}, \code{\link{EMPIRgrid}}, \code{\link{EMPIRgridder2}} }
\examples{
\dontrun{
uv <- simCOP(n=10000, cop=PSP, ploton=FALSE, points=FALSE)
fakeU <- pp(uv[,1], sort=FALSE)
fakeV <- pp(uv[,2], sort=FALSE)
uv <- data.frame(U=fakeU, V=fakeV)

uv.grid <- EMPIRgrid(para=uv, deluv=.1) # CPU hungry
uv.inv1 <- EMPIRgridderinv(empgrid=uv.grid)
uv.inv2 <- EMPIRgridderinv2(empgrid=uv.grid)
plot(uv, pch=16, col=rgb(0,0,0,.1),
     xlim=c(0,1), ylim=c(0,1),
     xlab="U, NONEXCEEDANCE PROBABILITY",
     ylab="V, NONEXCEEDANCE PROBABILTIY")
lines(qua.regressCOP(F=0.5, cop=PSP), col=2)
lines(qua.regressCOP(F=0.2, cop=PSP), col=2)
lines(qua.regressCOP(F=0.7, cop=PSP), col=2)
lines(qua.regressCOP(F=0.1, cop=PSP), col=2)
lines(qua.regressCOP(F=0.9, cop=PSP), col=2)

med.wrtu <- EMPIRqua.regress(F=0.5, empinv=uv.inv1)
lines(med.wrtu, col=2, lwd=4)
qua.wrtu <- EMPIRqua.regress(F=0.2, empinv=uv.inv1)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(F=0.7, empinv=uv.inv1)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(F=0.1, empinv=uv.inv1)
lines(qua.wrtu, col=2, lwd=2, lty=4)
qua.wrtu <- EMPIRqua.regress(F=0.9, empinv=uv.inv1)
lines(qua.wrtu, col=2, lwd=2, lty=4)


lines(qua.regressCOP2(F=0.5, cop=PSP), col=4)
lines(qua.regressCOP2(F=0.2, cop=PSP), col=4)
lines(qua.regressCOP2(F=0.7, cop=PSP), col=4)
lines(qua.regressCOP2(F=0.1, cop=PSP), col=4)
lines(qua.regressCOP2(F=0.9, cop=PSP), col=4)

med.wrtv <- EMPIRqua.regress2(F=0.5, empinv=uv.inv2)
lines(med.wrtv, col=4, lwd=4)
qua.wrtv <- EMPIRqua.regress2(F=0.2, empinv=uv.inv2)
lines(qua.wrtv, col=4, lwd=2, lty=2)
qua.wrtv <- EMPIRqua.regress2(F=0.7, empinv=uv.inv2)
lines(qua.wrtv, col=4, lwd=2, lty=2)
qua.wrtv <- EMPIRqua.regress2(F=0.1, empinv=uv.inv2)
lines(qua.wrtv, col=4, lwd=2, lty=4)
qua.wrtv <- EMPIRqua.regress2(F=0.9, empinv=uv.inv2)
lines(qua.wrtv, col=4, lwd=2, lty=4)
}


\dontrun{
# Now try a much more complex shape
para   <- list(alpha=.15,  beta=.65,
               cop1=PLACKETTcop, cop2=PLACKETTcop,
               para1=.005, para2=1000)
uv <- simCOP(n=30000, cop=composite2COP, para=para)
fakeU <- pp(uv[,1], sort=FALSE)
fakeV <- pp(uv[,2], sort=FALSE)
uv <- data.frame(U=fakeU, V=fakeV)

uv.grid <- EMPIRgrid(para=uv, deluv=.05) # CPU hungry
uv.inv1 <- EMPIRgridderinv(empgrid=uv.grid)
uv.inv2 <- EMPIRgridderinv2(empgrid=uv.grid)
plot(uv, pch=16, col=rgb(0,0,0,.1),
     xlim=c(0,1), ylim=c(0,1),
     xlab="U, NONEXCEEDANCE PROBABILITY",
     ylab="V, NONEXCEEDANCE PROBABILTIY")
lines(qua.regressCOP(F=0.5, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(F=0.2, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(F=0.7, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(F=0.1, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(F=0.9, cop=composite2COP, para=para), col=2)

med.wrtu <- EMPIRqua.regress(F=0.5, empinv=uv.inv1)
lines(med.wrtu, col=2, lwd=4)
qua.wrtu <- EMPIRqua.regress(F=0.2, empinv=uv.inv1)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(F=0.7, empinv=uv.inv1)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(F=0.1, empinv=uv.inv1)
lines(qua.wrtu, col=2, lwd=2, lty=4)
qua.wrtu <- EMPIRqua.regress(F=0.9, empinv=uv.inv1)
lines(qua.wrtu, col=2, lwd=2, lty=4)


lines(qua.regressCOP2(F=0.5, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(F=0.2, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(F=0.7, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(F=0.1, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(F=0.9, cop=composite2COP, para=para), col=4)

med.wrtv <- EMPIRqua.regress2(F=0.5, empinv=uv.inv2)
lines(med.wrtv, col=4, lwd=4)
qua.wrtv <- EMPIRqua.regress2(F=0.2, empinv=uv.inv2)
lines(qua.wrtv, col=4, lwd=2, lty=2)
qua.wrtv <- EMPIRqua.regress2(F=0.7, empinv=uv.inv2)
lines(qua.wrtv, col=4, lwd=2, lty=2)
qua.wrtv <- EMPIRqua.regress2(F=0.1, empinv=uv.inv2)
lines(qua.wrtv, col=4, lwd=2, lty=4)
qua.wrtv <- EMPIRqua.regress2(F=0.9, empinv=uv.inv2)
lines(qua.wrtv, col=4, lwd=2, lty=4)
}
}
\keyword{distribution}
\keyword{multivariate}
