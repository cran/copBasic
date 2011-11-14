\name{EMPIRqua.regress}
\alias{EMPIRqua.regress}
\title{ Derivatives of the Grid of the Bivariate Emprical Copula }
\description{
Generate quantile regression from the gridded inversion of the bivariate empirical copula.
}
\usage{
EMPIRqua.regress(F=0.5, U=seq(0.01,0.99, by=0.01), empinv=NULL,
                 lowess=FALSE, f=1/5, ...)
}
\arguments{
  \item{F}{The nonexceedance probability \eqn{F} to perform regression at and defaults to median regression \eqn{F=1/2},}
  \item{U}{A vector of \eqn{u} nonexceedance probabilities,}
  \item{empinv}{The grid from \code{\link{EMPIRgridderinv}},}
  \item{lowess}{Perform \code{lowess} smooth on the quantile regression using the smooth factor of \code{f=1/5} or other,}
  \item{f}{Smooth factor of same argument name fed to \code{lowess}, and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The gridded values of
}
\author{ W.H. Asquith }
\seealso{ \code{\link{EMPIRgridderinv}}, \code{\link{EMPIRqua.regress2}}, \code{\link{EMPIRmed.regress}}, \code{\link{EMPIRmed.regress2}}
}
\examples{

# EXAMPLE 1
\dontrun{
theta <- 25
uv <- simCOP(n=1000, cop=PLACKETTcop, para=theta, ploton=FALSE, points=FALSE)
fakeU <- pp(uv[,1], sort=FALSE)
fakeV <- pp(uv[,2], sort=FALSE)
uv <- data.frame(U=fakeU, V=fakeV)

uv.grid <- EMPIRgrid(para=uv, deluv=.05) # CPU hungry
uv.inv1 <- EMPIRgridderinv(empgrid=uv.grid)
plot(uv, pch=16, col=rgb(0,0,0,.1),
     xlim=c(0,1), ylim=c(0,1),
     xlab="U, NONEXCEEDANCE PROBABILITY",
     ylab="V, NONEXCEEDANCE PROBABILTIY")
lines(qua.regressCOP(F=0.5, cop=PLACKETTcop, para=theta), lwd=2)
lines(qua.regressCOP(F=0.2, cop=PLACKETTcop, para=theta), lwd=2)
lines(qua.regressCOP(F=0.7, cop=PLACKETTcop, para=theta), lwd=2)
lines(qua.regressCOP(F=0.1, cop=PLACKETTcop, para=theta), lwd=2)
lines(qua.regressCOP(F=0.9, cop=PLACKETTcop, para=theta), lwd=2)

med.wrtu <- EMPIRqua.regress(F=0.5, empinv=uv.inv1, lowess=TRUE, f=0.1)
lines(med.wrtu, col=2, lwd=4)
qua.wrtu <- EMPIRqua.regress(F=0.2, empinv=uv.inv1, lowess=TRUE, f=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(F=0.7, empinv=uv.inv1, lowess=TRUE, f=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(F=0.1, empinv=uv.inv1, lowess=TRUE, f=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=4)
qua.wrtu <- EMPIRqua.regress(F=0.9, empinv=uv.inv1, lowess=TRUE, f=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=4)

library(quantreg) # Quantile Regression by quantreg
U <- seq(.01, .99, by=.01)
rqlm <- rq(V~U, data=uv, tau=0.1)
rq.1 <- rqlm$coefficients[1] + rqlm$coefficients[2]*U
rqlm <- rq(V~U, data=uv, tau=0.2)
rq.2 <- rqlm$coefficients[1] + rqlm$coefficients[2]*U
rqlm <- rq(V~U, data=uv, tau=0.5)
rq.5 <- rqlm$coefficients[1] + rqlm$coefficients[2]*U
rqlm <- rq(V~U, data=uv, tau=0.7)
rq.7 <- rqlm$coefficients[1] + rqlm$coefficients[2]*U
rqlm <- rq(V~U, data=uv, tau=0.9)
rq.9 <- rqlm$coefficients[1] + rqlm$coefficients[2]*U

lines(U, rq.1, col=4, lwd=2, lty=4)
lines(U, rq.2, col=4, lwd=2, lty=2)
lines(U, rq.5, col=4, lwd=4)
lines(U, rq.7, col=4, lwd=2, lty=2)
lines(U, rq.9, col=4, lwd=2, lty=4)
}


# EXAMPLE 2
\dontrun{
# Start again with the PSP copula
uv <- simCOP(n=10000, cop=PSP, ploton=FALSE, points=FALSE)
fakeU <- pp(uv[,1], sort=FALSE)
fakeV <- pp(uv[,2], sort=FALSE)
uv <- data.frame(U=fakeU, V=fakeV)

uv.grid <- EMPIRgrid(para=uv, deluv=.05) # CPU hungry
uv.inv1 <- EMPIRgridderinv(empgrid=uv.grid)
plot(uv, pch=16, col=rgb(0,0,0,.1),
     xlim=c(0,1), ylim=c(0,1),
     xlab="U, NONEXCEEDANCE PROBABILITY",
     ylab="V, NONEXCEEDANCE PROBABILTIY")
lines(qua.regressCOP(F=0.5, cop=PSP), lwd=2)
lines(qua.regressCOP(F=0.2, cop=PSP), lwd=2)
lines(qua.regressCOP(F=0.7, cop=PSP), lwd=2)
lines(qua.regressCOP(F=0.1, cop=PSP), lwd=2)
lines(qua.regressCOP(F=0.9, cop=PSP), lwd=2)

med.wrtu <- EMPIRqua.regress(F=0.5, empinv=uv.inv1, lowess=TRUE, f=0.1)
lines(med.wrtu, col=2, lwd=4)
qua.wrtu <- EMPIRqua.regress(F=0.2, empinv=uv.inv1, lowess=TRUE, f=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(F=0.7, empinv=uv.inv1, lowess=TRUE, f=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(F=0.1, empinv=uv.inv1, lowess=TRUE, f=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=4)
qua.wrtu <- EMPIRqua.regress(F=0.9, empinv=uv.inv1, lowess=TRUE, f=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=4)

library(quantreg) # Quantile Regression by quantreg
U <- seq(.01, .99, by=.01)
rqlm <- rq(V~U, data=uv, tau=0.1)
rq.1 <- rqlm$coefficients[1] + rqlm$coefficients[2]*U
rqlm <- rq(V~U, data=uv, tau=0.2)
rq.2 <- rqlm$coefficients[1] + rqlm$coefficients[2]*U
rqlm <- rq(V~U, data=uv, tau=0.5)
rq.5 <- rqlm$coefficients[1] + rqlm$coefficients[2]*U
rqlm <- rq(V~U, data=uv, tau=0.7)
rq.7 <- rqlm$coefficients[1] + rqlm$coefficients[2]*U
rqlm <- rq(V~U, data=uv, tau=0.9)
rq.9 <- rqlm$coefficients[1] + rqlm$coefficients[2]*U

lines(U, rq.1, col=4, lwd=2, lty=4)
lines(U, rq.2, col=4, lwd=2, lty=2)
lines(U, rq.5, col=4, lwd=4)
lines(U, rq.7, col=4, lwd=2, lty=2)
lines(U, rq.9, col=4, lwd=2, lty=4)
}


# EXAMPLE 3
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

# EXAMPLE 4
\dontrun{
# Now try a much more complex shape
# lowess smoothing on quantile regression is possible,
# see next example
para   <- list(alpha=.15,  beta=.65,
               cop1=PLACKETTcop, cop2=PLACKETTcop,
               para1=.005, para2=1000)
uv <- simCOP(n=20000, cop=composite2COP, para=para)
fakeU <- pp(uv[,1], sort=FALSE)
fakeV <- pp(uv[,2], sort=FALSE)
uv <- data.frame(U=fakeU, V=fakeV)

uv.grid <- EMPIRgrid(para=uv, deluv=.025) # CPU hungry
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


# EXAMPLE 5
\dontrun{
plot(uv, pch=16, col=rgb(0,0,0,.1),
     xlim=c(0,1), ylim=c(0,1),
     xlab="U, NONEXCEEDANCE PROBABILITY",
     ylab="V, NONEXCEEDANCE PROBABILTIY")
lines(qua.regressCOP(F=0.5, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(F=0.2, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(F=0.7, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(F=0.1, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(F=0.9, cop=composite2COP, para=para), col=2)

med.wrtu <- EMPIRqua.regress(F=0.5, empinv=uv.inv1, lowess=TRUE)
lines(med.wrtu, col=2, lwd=4)
qua.wrtu <- EMPIRqua.regress(F=0.2, empinv=uv.inv1, lowess=TRUE)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(F=0.7, empinv=uv.inv1, lowess=TRUE)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(F=0.1, empinv=uv.inv1, lowess=TRUE)
lines(qua.wrtu, col=2, lwd=2, lty=4)
qua.wrtu <- EMPIRqua.regress(F=0.9, empinv=uv.inv1, lowess=TRUE)
lines(qua.wrtu, col=2, lwd=2, lty=4)


lines(qua.regressCOP2(F=0.5, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(F=0.2, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(F=0.7, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(F=0.1, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(F=0.9, cop=composite2COP, para=para), col=4)

med.wrtv <- EMPIRqua.regress2(F=0.5, empinv=uv.inv2, lowess=TRUE)
lines(med.wrtv, col=4, lwd=4)
qua.wrtv <- EMPIRqua.regress2(F=0.2, empinv=uv.inv2, lowess=TRUE)
lines(qua.wrtv, col=4, lwd=2, lty=2)
qua.wrtv <- EMPIRqua.regress2(F=0.7, empinv=uv.inv2, lowess=TRUE)
lines(qua.wrtv, col=4, lwd=2, lty=2)
qua.wrtv <- EMPIRqua.regress2(F=0.1, empinv=uv.inv2, lowess=TRUE)
lines(qua.wrtv, col=4, lwd=2, lty=4)
qua.wrtv <- EMPIRqua.regress2(F=0.9, empinv=uv.inv2, lowess=TRUE)
lines(qua.wrtv, col=4, lwd=2, lty=4)
}

# EXAMPLE 6
\dontrun{
plot(uv, pch=16, col=rgb(0,0,0,.1),
     xlim=c(0,1), ylim=c(0,1),
     xlab="U, NONEXCEEDANCE PROBABILITY",
     ylab="V, NONEXCEEDANCE PROBABILTIY")
lines(qua.regressCOP(F=0.5, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(F=0.2, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(F=0.7, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(F=0.1, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(F=0.9, cop=composite2COP, para=para), col=2)

med.wrtu <- EMPIRqua.regress(F=0.5, empinv=uv.inv1, lowess=TRUE, f=0.1)
lines(med.wrtu, col=2, lwd=4)
qua.wrtu <- EMPIRqua.regress(F=0.2, empinv=uv.inv1, lowess=TRUE, f=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(F=0.7, empinv=uv.inv1, lowess=TRUE, f=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(F=0.1, empinv=uv.inv1, lowess=TRUE, f=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=4)
qua.wrtu <- EMPIRqua.regress(F=0.9, empinv=uv.inv1, lowess=TRUE, f=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=4)


lines(qua.regressCOP2(F=0.5, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(F=0.2, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(F=0.7, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(F=0.1, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(F=0.9, cop=composite2COP, para=para), col=4)

med.wrtv <- EMPIRqua.regress2(F=0.5, empinv=uv.inv2, lowess=TRUE, f=0.1)
lines(med.wrtv, col=4, lwd=4)
qua.wrtv <- EMPIRqua.regress2(F=0.2, empinv=uv.inv2, lowess=TRUE, f=0.1)
lines(qua.wrtv, col=4, lwd=2, lty=2)
qua.wrtv <- EMPIRqua.regress2(F=0.7, empinv=uv.inv2, lowess=TRUE, f=0.1)
lines(qua.wrtv, col=4, lwd=2, lty=2)
qua.wrtv <- EMPIRqua.regress2(F=0.1, empinv=uv.inv2, lowess=TRUE, f=0.1)
lines(qua.wrtv, col=4, lwd=2, lty=4)
qua.wrtv <- EMPIRqua.regress2(F=0.9, empinv=uv.inv2, lowess=TRUE, f=0.1)
lines(qua.wrtv, col=4, lwd=2, lty=4)
}



}
\keyword{distribution}
\keyword{multivariate}
