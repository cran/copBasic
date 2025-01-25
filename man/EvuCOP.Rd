\encoding{utf8}
\name{EvuCOP}
\alias{EvuCOP}
\title{Expected value of V given U}
\description{
Compute the \emph{expected value} of \eqn{V} given a \eqn{U} (the \eqn{X} direction) through the \emph{conditional distribution function} \eqn{F(X)} using the appropriate \emph{partial derivative} of a copula (\eqn{\mathbf{C}(u,v)}) with respect to \eqn{U}. The inversion of the partial derivative is the \emph{conditional quantile function}. Basic principles provide the expectation for a \eqn{x \ge 0} is
\deqn{E[X] = \int_0^\infty xf(x)\mathrm{d}x = \int_0^\infty \bigl(1-F_x(X)\bigr)\mathrm{d}x\mbox{,}}
which for the setting here becomes
\deqn{E[V \mid U = u] = \int_0^1 \bigl(1 - \frac{\delta}{\delta u} \mathbf{C}(u,v)\bigr)\mathrm{d}v\mbox{.}}
This function solves the integral using the \code{\link{derCOP}} function. Verification study is provided in the \bold{Note} section.
}
\usage{
EvuCOP(u=seq(0.01, 0.99, by=0.01), cop=NULL, para=NULL, asuv=FALSE, nsim=1E5,
    subdivisions=100L, rel.tol=.Machine$double.eps^0.25, abs.tol=rel.tol, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{cop}{A copula function with vectorization as in \code{asCOP};}
  \item{para}{Vector of parameters or other data structures, if needed, to pass to the copula;}
  \item{asuv}{Return a data frame of the \eqn{U} and \eqn{V};}
  \item{nsim}{Number of simulations for Monte Carlo integration when the numerical integration fails (see \bold{Note} in \code{\link{EvuCOP}});}
  \item{subdivisions}{Argument of same name passed to \code{integrate()};}
  \item{rel.tol}{Argument of same name passed to \code{integrate()};}
  \item{abs.tol}{Argument of same name passed to \code{integrate()}; and}
  \item{...}{Additional arguments to pass to \code{\link{derCOP}}.}
}
\value{
  Value(s) for the expectation are returned.
}
\note{
For the \code{\link{PSP}} copula with no parameters, compute the the median and mean \eqn{V} given \eqn{U=0.4}, respectively:
\preformatted{
  U <- 0.4; n <- 1E4
  med.regressCOP(u=U, cop=PSP)                            # V = 0.4912752
  set.seed(1)
  median(replicate(n, derCOPinv(cop=PSP, U, runif(1)) ) ) # V = 0.4876440
  mean(  replicate(n, derCOPinv(cop=PSP, U, runif(1)) ) ) # V = 0.5049729
}

It is seen in the above that the median \eqn{V} given \eqn{U} is very close to the mean, but is not equal. Using the derivative inversion within \code{\link{med.regressCOP}} the median is about 0.491 and then using large-sample simulation, about 0.491 too is computed. This confirms the median and long standing proven use of \code{\link{derCOP}} (conditional distribution function) and \code{\link{derCOPinv}} (conditional quantile function) within the package. The expectation (mean) by simulation provides the anchor point to check implementation of \code{EuvCOP()}. The mean for \eqn{V} given \eqn{U} is about 0.505. Continuing, the core logic of \code{EvuCOP()} is to use numerical integration of the conditional distribution function (the partial derivative) and not bother for speed purposes to use the inversion of the partial derivative:
\preformatted{
  integrate(function(v)                      1-derCOP(   cop=PSP, U, v),
            lower=0, upper=1) # 0.5047805 with absolute error < 1.4e-11

  integrate(function(v) sapply(v, function(t)  derCOPinv(cop=PSP, U, t)),
            lower=0, upper=1) # 0.5047862 with absolute error < 7.2e-05
}
The two integrals match, which functions as a confirmation of the \eqn{(1-F)} term in the mathematical definition. Finally, the two integrals match the simulation results. The expectation or mean \eqn{V \mid U=0.4} for the \code{\link{PSP}} copula is about 0.5048.
}
\author{W.H. Asquith}
\seealso{\code{\link{EuvCOP}}, \code{\link{derCOP}}}
\examples{
# Highly asymmetric and reflected Clayton copula for which visualization
para <- list(cop=CLcop, para=30, alpha=0.2, beta=0.6, reflect=3)
# UV <- simCOP(5000, cop=breveCOP, para=para, cex=0.5); abline(v=0.25, col="red")
EvuCOP(0.25, cop=breveCOP, para=para)  # 0.5982261
# confirms that at U=0.25 that an intuitive estimate would be about 0.6.

\dontrun{
  # Secondary validation of the EvuCOP() and EuvCOP() implementation
  UV <- simCOP(200, cop=PSP)
  u <- seq(0.005, 0.995, by=0.005)
  v <- sapply(u, function(t) integrate(function(k)
                         1 - derCOP(cop=PSP, t, k),  lower=0, upper=1)$value)
  lines(u,v, col="red", lwd=7, lty=1)   # red line
  v <- seq(0.005, 0.995, by=0.005)
  u <- sapply(v, function(t) integrate(function(k)
                         1 - derCOP2(cop=PSP, k, t), lower=0, upper=1)$value)
  lines(u,v, col="red", lwd=7, lty=2)   # dashed red line

  uv <- seq(0.005, 0.995, by=0.005)     # solid and dashed white lines
  lines(EvuCOP(uv, cop=PSP, asuv=TRUE), col="white",  lwd=3,   lty=1)
  lines(EuvCOP(uv, cop=PSP, asuv=TRUE), col="white",  lwd=3,   lty=3)

  # median regression lines for comparison, green and green dashed lines
  lines(med.regressCOP( uv, cop=PSP), col="seagreen", lwd=1.5, lty=1)
  lines(med.regressCOP2(uv, cop=PSP), col="seagreen", lwd=1.5, lty=4) #}

\dontrun{
  uv <- seq(0.005, 0.995, by=0.005) # stress testing eample with singularity
  UV <- simCOP(50, cop=M_N5p12b, para=2)
  lines(EvuCOP(uv, cop=M_N5p12b, para=2, asuv=TRUE), col="red",     lwd=5)
  lines(EuvCOP(uv, cop=M_N5p12b, para=2, asuv=TRUE), col="skyblue", lwd=1) #}

\dontrun{
  uv <- seq(0.005, 0.995, by=0.005) # more asymmetry ---- mean regression in UV and VU
  para <- list(cop=GHcop, para=23, alpha=0.1, beta=0.6, reflect=3)
  para <- list(cop=breveCOP, para=para)
  UV <- simCOP(200, cop=COP, para=para)
  lines(EuvCOP(uv,  cop=COP, para=para, asuv=TRUE), col="red",  lwd=2)
  lines(EvuCOP(uv,  cop=COP, para=para, asuv=TRUE), col="blue", lwd=2) #}

\dontrun{
  # Open questions? The derCOP() and derCOPinv() functions of the package have long
  # been known to work "properly." But let us think again on the situation of
  # permutation symmetry about the equal value line. Recalling that this symmetry is
  # orthogonal to the equal value line, it remains open whether there could be
  # asymmetry in the vertical (or horizontal). Let us draw some median regression lines
  # and see that the do not plot perfectly on the equal value line, but coudl this be
  # down to numerical issues and by association the simulation of the copula itself
  # that is also using derCOPinv() (conditional simulation method). Then, we can plot
  # the expections and we see that these are not equal to the medians, but again are
  # close. *** Do results here indicate edges of numerical performance? ***
  t <- seq(0.01, 0.99, by=0.01)
  UV <- simCOP(10000,   cop=N4212cop, para=4, pch=21, lwd=0.8, col=8, bg="white")
  lines(med.regressCOP( cop=N4212cop, para=4, asuv=TRUE), col="red")
  lines(med.regressCOP2(cop=N4212cop, para=4, asuv=TRUE), col="red")
  abline(0, 1, col="deepskyblue", lwd=3); abline(v=0.5, col="deepskyblue", lwd=4)
  lines(EvuCOP(t, cop=N4212cop, para=4, asuv=TRUE), pch=16, col="darkgreen")
  lines(EuvCOP(t, cop=N4212cop, para=4, asuv=TRUE), pch=16, col="darkgreen") #}
}
\keyword{copula (utility)}
\keyword{copula operator}
\keyword{copula (conditional distribution)}
\keyword{conditional expectation}
\keyword{mean V given U}
\concept{expectation}
\keyword{Theory Examples (reflection/rotation)}
