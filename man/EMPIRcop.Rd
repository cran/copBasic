\name{EMPIRcop}
\alias{EMPIRcop}
\title{ The Bivariate Empirical Copula }
\description{
The Bivariate Empirical Copula for a bivariate sample of length \eqn{n} is

\deqn{\mathbf{C}_{n}(u,v) =
     \frac{1}{n}\sum_{i=1}^n \mathbf{1}\bigl(u_{obs} \le u, v_{obs} \le v \bigr)}
where \eqn{u_{obs}} and \eqn{v_{obs}} are the estimated nonexceedance probabilities in unranked form as computed by Weibull plotting positions (\eqn{i/(n+1)}, for example) or other plotting position formula. The example shown in this documentation sufficiently clarifies this concept. The \code{sort=FALSE} of the function \code{pp} of the \pkg{lmomco} package is present just for the very operation needed here for \eqn{u_{obs}} and \eqn{v_{obs}}.
}
\usage{
EMPIRcop(u, v, para=NULL, ...)
}
\arguments{
  \item{u}{A nonexceedance probability in X direction,}
  \item{v}{A nonexceedance probability in Y direction,}
  \item{para}{A vector (single element) of parameters---the U-statistics of the data (see example), and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for the copula is returned.
}
\note{
See p. 140 of the reference.

Also the \code{\link{derCOP}} and \code{\link{derCOP2}} functions are critical functions (derivatives of a copula) for many copula operations within the \pkg{copBasic} package. However, the \code{EMPIRcop} function is not yet compatible with the method of numerical derivation. Other functions such as the quantile regression functions (\code{\link{qua.regressCOP}}, for example) or \code{\link{derCOPinv}} or \code{\link{simCOP}}.
}
\references{
Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007,
Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{ W.H. Asquith }
\seealso{ \code{\link{simCOP}}, \code{\link{PSP}}, \code{\link{level.curvesCOP}} }
\examples{
psp <- simCOP(n=50, cop=PSP, ploton=FALSE, points=FALSE) *150;
# Pretend psp is real data, the use of *150 is to clearly get the
# probabilities from simCOP into some other arbitrary unit system.

# The sort=FALSE is critical in the following two calls
fakeU <- pp(psp[,1], sort=FALSE); # Weibull plotting position i/(n+1)
fakeV <- pp(psp[,2], sort=FALSE); # Weibull plotting position i/(n+1)

uv <- data.frame(U=fakeU, V=fakeV); # our U-statistics

# these next two values should be REAL close with n=1000
print(EMPIRcop(0.4,0.6,para=uv));
print(PSP(0.4,0.6));

level.curvesCOP(cop=PSP); # parametric, fast,  BLACK CURVES

# The level curves of the empirical copula consume lots of CPU.
# The next operation is slow, so change delu to a larger value
# from the default (0.02) works ok for a "modern" computer.
# RED CURVES
level.curvesCOP(cop=EMPIRcop, para=uv, delu=0.02, col=2, ploton=FALSE)

\dontrun{

diagCOP(cop=EMPIRcop, para=uv)

}

\dontrun{
# Experimental for author

# From R Graphics by Murrell (2005, p.112)
"trans3d" <- function(x,y,z, pmat) {
   tmat <- cbind(x,y,z,1) %*% pmat
   return(tmat[,1:2] / tmat[,4])
}

the.grid <- EMPIRgrid(para=uv)
the.diag <- diagCOP(cop=EMPIRcop, para=uv, ploton=FALSE, lines=FALSE)

the.persp <- persp(the.grid$z, theta=-25, phi=20,
          xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")

the.trace <- trans3d(the.diag$t, the.diag$t, the.diag$diagcop, the.persp)
lines(the.trace, lwd=2, col=2)


the.persp <- persp(x=the.grid$x, y=the.grid$y, z=the.grid$z,
                   theta=-25, phi=20,
          xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")

the.trace <- trans3d(the.diag$t, the.diag$t, the.diag$diagcop, the.persp)
lines(the.trace, lwd=2, col=2)

}


}
\keyword{distribution}
\keyword{multivariate}
