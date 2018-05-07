\name{EMPIRgridder}
\alias{EMPIRgridder}
\title{ Derivatives of the Grid of the Bivariate Empirical Copula for V with respect to U }
\description{
Generate derivatives of \eqn{V} with respect to \eqn{U} of a gridded representation of the \emph{bivariate empirical copula} (see \code{\link{EMPIRcop}}). This function is the empirical analog to \code{\link{derCOP}}.
}
\usage{
EMPIRgridder(empgrid=NULL, ...)
}
\arguments{
  \item{empgrid}{The grid from \code{\link{EMPIRgrid}}; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The gridded values of the derivatives of the bivariate empirical copula.
}
\author{ W.H. Asquith }
\seealso{ \code{\link{EMPIRcop}}, \code{\link{EMPIRcopdf}}, \code{\link{EMPIRgrid}}, \code{\link{EMPIRgridder2}} }
\examples{
\dontrun{
para   <- list(alpha=0.15,  beta=0.65, cop1=PLACKETTcop, cop2=PLACKETTcop,
               para1=0.005, para2=1000)
uv <- simCOP(n=1000, cop=composite2COP, para=para)
fakeU <- lmomco::pp(uv[,1], sort=FALSE)
fakeV <- lmomco::pp(uv[,2], sort=FALSE)
uv <- data.frame(U=fakeU, V=fakeV)

"trans3d" <- function(x,y,z, pmat) {
   tmat <- cbind(x,y,z,1) \%*\% pmat; return(tmat[,1:2] / tmat[,4])
}

the.grid <- EMPIRgrid(para=uv, deluv=0.1)
the.diag <- diagCOP(cop=EMPIRcop, para=uv, ploton=FALSE, lines=FALSE)
empcop <- EMPIRcopdf(para=uv) # data.frame of all points

the.persp <- persp(the.grid$empcop, theta=-25, phi=20,
                   xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")
points(trans3d(empcop$u, empcop$v, empcop$empcop, the.persp),
       col=rgb(0,1-sqrt(empcop$empcop),1,sqrt(empcop$empcop)), pch=16, cex=0.75)

# Now extract the copula sections
some.lines <- trans3d(rep(0.2, length(the.grid$v)),
                      the.grid$v, the.grid$empcop[3,], the.persp)
lines(some.lines, lwd=2, col=2)
some.lines <- trans3d(the.grid$u, rep(0.6, length(the.grid$u)),
                      the.grid$empcop[,7], the.persp)
lines(some.lines, lwd=2, col=3)
some.lines <- trans3d(rep(0.7, length(the.grid$v)), the.grid$v,
                      the.grid$empcop[8,], the.persp)
lines(some.lines, lwd=2, col=6)

# Now compute some derivatives or conditional cumulative
# distribution functions
empder <- EMPIRgridder(empgrid=the.grid)
some.lines <- trans3d(rep(0.2, length(the.grid$v)), the.grid$v, empder[3,], the.persp)
lines(some.lines, lwd=4, col=2)

empder <- EMPIRgridder2(empgrid=the.grid)
some.lines <- trans3d(the.grid$u, rep(0.6, length(the.grid$u)), empder[,7], the.persp)
lines(some.lines, lwd=4, col=3)

empder <- EMPIRgridder(empgrid=the.grid)
some.lines <- trans3d(rep(0.7, length(the.grid$v)), the.grid$v, empder[8,], the.persp)
lines(some.lines, lwd=4, col=6)

# Demonstrate conditional quantile function extraction for
# the 70th percentile of U and see how it plots on top of
# the thick purple line
empinv <- EMPIRgridderinv(empgrid=the.grid)
some.lines <- trans3d(rep(0.7, length(the.grid$v)), empinv[8,],
                      attributes(empinv)$colnames, the.persp)
lines(some.lines, lwd=4, col=5, lty=2)#}
}
\keyword{empirical copula}
\keyword{empirical copula (derivatives)}
\keyword{derivative}
