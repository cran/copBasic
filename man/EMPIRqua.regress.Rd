\name{EMPIRqua.regress}
\alias{EMPIRqua.regress}
\title{ Quantile Regression of the Grid of the Bivariate Empirical Copula for V with respect to U }
\description{
Perform \emph{quantile regression} from the gridded inversion of the \emph{bivariate empirical copula} of \eqn{V} with respect to \eqn{U}.
}
\usage{
EMPIRqua.regress(f=0.5, u=seq(0.01,0.99, by=0.01), empinv=NULL,
                 lowess=FALSE, f.lowess=1/5, ...)
}
\arguments{
  \item{f}{The nonexceedance probability \eqn{F} to perform regression at and defaults to median regression \eqn{F=1/2};}
  \item{u}{A vector of \eqn{u} nonexceedance probabilities;}
  \item{empinv}{The grid from \code{\link{EMPIRgridderinv}};}
  \item{lowess}{Perform \code{lowess} smooth on the quantile regression using the smooth factor of \code{f.lowess};}
  \item{f.lowess}{Smooth factor of almost the same argument name fed to the \code{lowess()} function in \R; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The gridded values of the quantile regression of \eqn{V} with respect to \eqn{U}.
}
\author{ W.H. Asquith }
\seealso{ \code{\link{EMPIRgridderinv}}, \code{\link{EMPIRqua.regress2}}, \code{\link{EMPIRmed.regress}}, \code{\link{EMPIRmed.regress2}}
}
\examples{
\dontrun{ # EXAMPLE 1
theta <- 25
uv <- simCOP(n=1000, cop=PLACKETTcop, para=theta, ploton=FALSE, points=FALSE)
fakeU <- lmomco::pp(uv[,1], sort=FALSE)
fakeV <- lmomco::pp(uv[,2], sort=FALSE)
uv <- data.frame(U=fakeU, V=fakeV)

uv.grid <- EMPIRgrid(para=uv, deluv=0.05) # CPU hungry
uv.inv1 <- EMPIRgridderinv(empgrid=uv.grid)
plot(uv, pch=16, col=rgb(0,0,0,.1), xlim=c(0,1), ylim=c(0,1),
     xlab="U, NONEXCEEDANCE PROBABILITY", ylab="V, NONEXCEEDANCE PROBABILITY")
lines(qua.regressCOP(f=0.5, cop=PLACKETTcop, para=theta), lwd=2)
lines(qua.regressCOP(f=0.2, cop=PLACKETTcop, para=theta), lwd=2)
lines(qua.regressCOP(f=0.7, cop=PLACKETTcop, para=theta), lwd=2)
lines(qua.regressCOP(f=0.1, cop=PLACKETTcop, para=theta), lwd=2)
lines(qua.regressCOP(f=0.9, cop=PLACKETTcop, para=theta), lwd=2)

med.wrtu <- EMPIRqua.regress(f=0.5, empinv=uv.inv1, lowess=TRUE, f.lowess=0.1)
lines(med.wrtu, col=2, lwd=4)
qua.wrtu <- EMPIRqua.regress(f=0.2, empinv=uv.inv1, lowess=TRUE, f.lowess=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(f=0.7, empinv=uv.inv1, lowess=TRUE, f.lowess=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(f=0.1, empinv=uv.inv1, lowess=TRUE, f.lowess=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=4)
qua.wrtu <- EMPIRqua.regress(f=0.9, empinv=uv.inv1, lowess=TRUE, f.lowess=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=4)

library(quantreg) # Quantile Regression by quantreg
U <- seq(0.01, 0.99, by=0.01)
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
lines(U, rq.9, col=4, lwd=2, lty=4)#}

\dontrun{ # EXAMPLE 2
# Start again with the PSP copula
uv <- simCOP(n=10000, cop=PSP, ploton=FALSE, points=FALSE)
fakeU <- lmomco::pp(uv[,1], sort=FALSE)
fakeV <- lmomco::pp(uv[,2], sort=FALSE)
uv <- data.frame(U=fakeU, V=fakeV)

uv.grid <- EMPIRgrid(para=uv, deluv=0.05) # CPU hungry
uv.inv1 <- EMPIRgridderinv(empgrid=uv.grid)
plot(uv, pch=16, col=rgb(0,0,0,0.1), xlim=c(0,1), ylim=c(0,1),
     xlab="U, NONEXCEEDANCE PROBABILITY", ylab="V, NONEXCEEDANCE PROBABILITY")
lines(qua.regressCOP(f=0.5, cop=PSP), lwd=2)
lines(qua.regressCOP(f=0.2, cop=PSP), lwd=2)
lines(qua.regressCOP(f=0.7, cop=PSP), lwd=2)
lines(qua.regressCOP(f=0.1, cop=PSP), lwd=2)
lines(qua.regressCOP(f=0.9, cop=PSP), lwd=2)

med.wrtu <- EMPIRqua.regress(f=0.5, empinv=uv.inv1, lowess=TRUE, f.lowess=0.1)
lines(med.wrtu, col=2, lwd=4)
qua.wrtu <- EMPIRqua.regress(f=0.2, empinv=uv.inv1, lowess=TRUE, f.lowess=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(f=0.7, empinv=uv.inv1, lowess=TRUE, f.lowess=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(f=0.1, empinv=uv.inv1, lowess=TRUE, f.lowess=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=4)
qua.wrtu <- EMPIRqua.regress(f=0.9, empinv=uv.inv1, lowess=TRUE, f.lowess=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=4)

library(quantreg) # Quantile Regression by quantreg
U <- seq(0.01, 0.99, by=0.01)
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
lines(U, rq.9, col=4, lwd=2, lty=4)#}

\dontrun{ # EXAMPLE 3
uv <- simCOP(n=10000, cop=PSP, ploton=FALSE, points=FALSE)
fakeU <- lmomco::pp(uv[,1], sort=FALSE)
fakeV <- lmomco::pp(uv[,2], sort=FALSE)
uv <- data.frame(U=fakeU, V=fakeV)

uv.grid <- EMPIRgrid(para=uv, deluv=0.1) # CPU hungry
uv.inv1 <- EMPIRgridderinv(empgrid=uv.grid)
uv.inv2 <- EMPIRgridderinv2(empgrid=uv.grid)
plot(uv, pch=16, col=rgb(0,0,0,.1), xlim=c(0,1), ylim=c(0,1),
     xlab="U, NONEXCEEDANCE PROBABILITY", ylab="V, NONEXCEEDANCE PROBABILITY")
lines(qua.regressCOP(f=0.5, cop=PSP), col=2)
lines(qua.regressCOP(f=0.2, cop=PSP), col=2)
lines(qua.regressCOP(f=0.7, cop=PSP), col=2)
lines(qua.regressCOP(f=0.1, cop=PSP), col=2)
lines(qua.regressCOP(f=0.9, cop=PSP), col=2)

med.wrtu <- EMPIRqua.regress(f=0.5, empinv=uv.inv1)
lines(med.wrtu, col=2, lwd=4)
qua.wrtu <- EMPIRqua.regress(f=0.2, empinv=uv.inv1)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(f=0.7, empinv=uv.inv1)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(f=0.1, empinv=uv.inv1)
lines(qua.wrtu, col=2, lwd=2, lty=4)
qua.wrtu <- EMPIRqua.regress(f=0.9, empinv=uv.inv1)
lines(qua.wrtu, col=2, lwd=2, lty=4)

lines(qua.regressCOP2(f=0.5, cop=PSP), col=4)
lines(qua.regressCOP2(f=0.2, cop=PSP), col=4)
lines(qua.regressCOP2(f=0.7, cop=PSP), col=4)
lines(qua.regressCOP2(f=0.1, cop=PSP), col=4)
lines(qua.regressCOP2(f=0.9, cop=PSP), col=4)

med.wrtv <- EMPIRqua.regress2(f=0.5, empinv=uv.inv2)
lines(med.wrtv, col=4, lwd=4)
qua.wrtv <- EMPIRqua.regress2(f=0.2, empinv=uv.inv2)
lines(qua.wrtv, col=4, lwd=2, lty=2)
qua.wrtv <- EMPIRqua.regress2(f=0.7, empinv=uv.inv2)
lines(qua.wrtv, col=4, lwd=2, lty=2)
qua.wrtv <- EMPIRqua.regress2(f=0.1, empinv=uv.inv2)
lines(qua.wrtv, col=4, lwd=2, lty=4)
qua.wrtv <- EMPIRqua.regress2(f=0.9, empinv=uv.inv2)
lines(qua.wrtv, col=4, lwd=2, lty=4)#}

\dontrun{ # EXAMPLE 4
# Now try a much more complex shape
# lowess smoothing on quantile regression is possible,
# see next example
para   <- list(alpha=0.15,  beta=0.65,
               cop1=PLACKETTcop, cop2=PLACKETTcop, para1=0.005, para2=1000)
uv <- simCOP(n=20000, cop=composite2COP, para=para)
fakeU <- lmomco::pp(uv[,1], sort=FALSE)
fakeV <- lmomco::pp(uv[,2], sort=FALSE)
uv <- data.frame(U=fakeU, V=fakeV)

uv.grid <- EMPIRgrid(para=uv, deluv=0.025) # CPU hungry
uv.inv1 <- EMPIRgridderinv(empgrid=uv.grid)
uv.inv2 <- EMPIRgridderinv2(empgrid=uv.grid)
plot(uv, pch=16, col=rgb(0,0,0,0.1), xlim=c(0,1), ylim=c(0,1),
     xlab="U, NONEXCEEDANCE PROBABILITY", ylab="V, NONEXCEEDANCE PROBABILTIY")
lines(qua.regressCOP(f=0.5, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(f=0.2, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(f=0.7, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(f=0.1, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(f=0.9, cop=composite2COP, para=para), col=2)

med.wrtu <- EMPIRqua.regress(f=0.5, empinv=uv.inv1)
lines(med.wrtu, col=2, lwd=4)
qua.wrtu <- EMPIRqua.regress(f=0.2, empinv=uv.inv1)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(f=0.7, empinv=uv.inv1)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(f=0.1, empinv=uv.inv1)
lines(qua.wrtu, col=2, lwd=2, lty=4)
qua.wrtu <- EMPIRqua.regress(f=0.9, empinv=uv.inv1)
lines(qua.wrtu, col=2, lwd=2, lty=4)

lines(qua.regressCOP2(f=0.5, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(f=0.2, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(f=0.7, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(f=0.1, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(f=0.9, cop=composite2COP, para=para), col=4)

med.wrtv <- EMPIRqua.regress2(f=0.5, empinv=uv.inv2)
lines(med.wrtv, col=4, lwd=4)
qua.wrtv <- EMPIRqua.regress2(f=0.2, empinv=uv.inv2)
lines(qua.wrtv, col=4, lwd=2, lty=2)
qua.wrtv <- EMPIRqua.regress2(f=0.7, empinv=uv.inv2)
lines(qua.wrtv, col=4, lwd=2, lty=2)
qua.wrtv <- EMPIRqua.regress2(f=0.1, empinv=uv.inv2)
lines(qua.wrtv, col=4, lwd=2, lty=4)
qua.wrtv <- EMPIRqua.regress2(f=0.9, empinv=uv.inv2)
lines(qua.wrtv, col=4, lwd=2, lty=4)#}

\dontrun{ # EXAMPLE 5
plot(uv, pch=16, col=rgb(0,0,0,.1), xlim=c(0,1), ylim=c(0,1),
     xlab="U, NONEXCEEDANCE PROBABILITY", ylab="V, NONEXCEEDANCE PROBABILITY")
lines(qua.regressCOP(f=0.5, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(f=0.2, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(f=0.7, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(f=0.1, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(f=0.9, cop=composite2COP, para=para), col=2)

med.wrtu <- EMPIRqua.regress(f=0.5, empinv=uv.inv1, lowess=TRUE)
lines(med.wrtu, col=2, lwd=4)
qua.wrtu <- EMPIRqua.regress(f=0.2, empinv=uv.inv1, lowess=TRUE)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(f=0.7, empinv=uv.inv1, lowess=TRUE)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(f=0.1, empinv=uv.inv1, lowess=TRUE)
lines(qua.wrtu, col=2, lwd=2, lty=4)
qua.wrtu <- EMPIRqua.regress(f=0.9, empinv=uv.inv1, lowess=TRUE)
lines(qua.wrtu, col=2, lwd=2, lty=4)

lines(qua.regressCOP2(f=0.5, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(f=0.2, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(f=0.7, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(f=0.1, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(f=0.9, cop=composite2COP, para=para), col=4)

med.wrtv <- EMPIRqua.regress2(f=0.5, empinv=uv.inv2, lowess=TRUE)
lines(med.wrtv, col=4, lwd=4)
qua.wrtv <- EMPIRqua.regress2(f=0.2, empinv=uv.inv2, lowess=TRUE)
lines(qua.wrtv, col=4, lwd=2, lty=2)
qua.wrtv <- EMPIRqua.regress2(f=0.7, empinv=uv.inv2, lowess=TRUE)
lines(qua.wrtv, col=4, lwd=2, lty=2)
qua.wrtv <- EMPIRqua.regress2(f=0.1, empinv=uv.inv2, lowess=TRUE)
lines(qua.wrtv, col=4, lwd=2, lty=4)
qua.wrtv <- EMPIRqua.regress2(f=0.9, empinv=uv.inv2, lowess=TRUE)
lines(qua.wrtv, col=4, lwd=2, lty=4)#}

\dontrun{ # EXAMPLE 6 (changing the smoothing on the lowess)
plot(uv, pch=16, col=rgb(0,0,0,0.1), xlim=c(0,1), ylim=c(0,1),
     xlab="U, NONEXCEEDANCE PROBABILITY", ylab="V, NONEXCEEDANCE PROBABILTIY")
lines(qua.regressCOP(f=0.5, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(f=0.2, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(f=0.7, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(f=0.1, cop=composite2COP, para=para), col=2)
lines(qua.regressCOP(f=0.9, cop=composite2COP, para=para), col=2)

med.wrtu <- EMPIRqua.regress(f=0.5, empinv=uv.inv1, lowess=TRUE, f.lowess=0.1)
lines(med.wrtu, col=2, lwd=4)
qua.wrtu <- EMPIRqua.regress(f=0.2, empinv=uv.inv1, lowess=TRUE, f.lowess=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(f=0.7, empinv=uv.inv1, lowess=TRUE, f.lowess=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=2)
qua.wrtu <- EMPIRqua.regress(f=0.1, empinv=uv.inv1, lowess=TRUE, f.lowess=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=4)
qua.wrtu <- EMPIRqua.regress(f=0.9, empinv=uv.inv1, lowess=TRUE, f.lowess=0.1)
lines(qua.wrtu, col=2, lwd=2, lty=4)

lines(qua.regressCOP2(f=0.5, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(f=0.2, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(f=0.7, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(f=0.1, cop=composite2COP, para=para), col=4)
lines(qua.regressCOP2(f=0.9, cop=composite2COP, para=para), col=4)

med.wrtv <- EMPIRqua.regress2(f=0.5, empinv=uv.inv2, lowess=TRUE, f.lowess=0.1)
lines(med.wrtv, col=4, lwd=4)
qua.wrtv <- EMPIRqua.regress2(f=0.2, empinv=uv.inv2, lowess=TRUE, f.lowess=0.1)
lines(qua.wrtv, col=4, lwd=2, lty=2)
qua.wrtv <- EMPIRqua.regress2(f=0.7, empinv=uv.inv2, lowess=TRUE, f.lowess=0.1)
lines(qua.wrtv, col=4, lwd=2, lty=2)
qua.wrtv <- EMPIRqua.regress2(f=0.1, empinv=uv.inv2, lowess=TRUE, f.lowess=0.1)
lines(qua.wrtv, col=4, lwd=2, lty=4)
qua.wrtv <- EMPIRqua.regress2(f=0.9, empinv=uv.inv2, lowess=TRUE, f.lowess=0.1)
lines(qua.wrtv, col=4, lwd=2, lty=4)#}
}
\keyword{empirical copula}
\keyword{empirical copula (quantile regression)}

