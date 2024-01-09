\encoding{utf8}
\name{EuvCOP}
\alias{EuvCOP}
\title{Expected value of U given V}
\description{
Compute the \emph{expected value} of \eqn{U} given a \eqn{V} (the \eqn{Y} direction) through the \emph{conditional distribution function} \eqn{G(Y)} using the appropriate \emph{partial derivative} of a copula (\eqn{\mathbf{C}(u,v)}) with respect to \eqn{V}. The inversion of the partial derivative is the \emph{conditional quantile function}. Basic principles provide the expectation for a \eqn{y \ge 0} is

\deqn{E[Y] = \int_0^\infty yf(y)\mathrm{d}y = \int_0^\infty \bigl(1-G_y(Y)\bigr)\mathrm{d}y\mbox{,}}

which for the setting here becomes

\deqn{E[U \mid V = v] = \int_0^1 \bigl(1 - \frac{\delta}{\delta v} \mathbf{C}(u,v)\bigr)\mathrm{d}u\mbox{.}}

This function solves the integral using the \code{\link{derCOP2}} function. This avoids a call of the \code{\link{derCOPinv2}} through its \code{uniroot()} inversion of the derivative. The example shown for \code{EuvCOP()} below does a validation check using conditional simulation, which is dependence (of course) of the design of the \pkg{copBasic} package, as part of simple isolation of a horizontal slice of the simulation and computing the mean of the \eqn{V} within the slice.
}
\usage{
EuvCOP(v=seq(0.01, 0.99, by=0.01), cop=NULL, para=NULL, asuv=FALSE, nsim=1E5,
    subdivisions=100L, rel.tol=.Machine$double.eps^0.25, abs.tol=rel.tol, ...)
}
\arguments{
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{cop}{A copula function with vectorization as in \code{asCOP};}
  \item{para}{Vector of parameters or other data structures, if needed, to pass to the copula;}
  \item{asuv}{Return a data frame of the \eqn{U} and \eqn{V};}
  \item{nsim}{Number of simulations for Monte Carlo integration when the numerical integration fails (see \bold{Note});}
  \item{subdivisions}{Argument of same name passed to \code{integrate()};}
  \item{rel.tol}{Argument of same name passed to \code{integrate()};}
  \item{abs.tol}{Argument of same name passed to \code{integrate()}; and}
  \item{...}{Additional arguments to pass to \code{\link{derCOP2}}.}
}
\value{
  Value(s) for the expectation are returned.
}
\note{
The author is well aware that the name of this function does not contain the number \code{2} as the family of functions also sharing this \emph{with respect to \eqn{v}} nature. It was a design mistake in 2008 to have used the \code{2}. The \code{uv} in the function name is the moniker for this \emph{with respect to \eqn{v}}.

There can be the rare examples of the numerical integration failing. In such circumstances, Monte Carlo integration is performed and the returned vector becomes a named vector with the \code{sim} identifying values stemming from the simulation.
\preformatted{
  para <- list(cop=RFcop, para=0.9)
  para <- list(cop=COP, para=para, reflect=1, alpha=0, beta=0.3)
  EuvCOP(c(0.0001, 0.0002, 0.001, 0.01, 0.1), cop=composite1COP, para=para)
  #            sim
  #[1] 0.001319395 0.002238238 0.006905300 0.034608078 0.173451788
}
}
\author{W.H. Asquith}
\seealso{\code{\link{EvuCOP}}, \code{\link{derCOP2}}}
\examples{
# We can show algorithmic validation using a highly asymmetric case of a
# copula having its parameter also nearly generating a singular component.
v <- c(0.2, 0.8); n <- 5E2; set.seed(1)
para <- list(cop=HRcop, para=120, alpha=0.4, beta=0.05)
UV   <- simCOP(n, cop=composite1COP, para=para, graphics=FALSE) # set TRUE to view

sapply(v, function(vv) EuvCOP(vv, cop=composite1COP, para=para))
# [1] 0.3051985 0.7059999

sapply(v, function(vv) mean( UV$U[UV$V > vv - 50/n & UV$V < vv + 50/n] ) )
# [1] 0.2796518 0.7092755 # general validation is thus shown as n-->large

# If visualized, we see in the lower corner than heuristics suggest a mean further
# to the right of the "singularity" for v=0.2 than v=0.80. For v=0.80, the
# "singularity" appears tighter given the upper tail dependency contrast of the
# coupla in the symmetrical case (alpha=0, beta=0) and changing the parameter to
# a Spearman Rho (say) similar to the para settting in this example. So, 0.70 for
# the mean given v=0.80 makes sense. Further notice that the two estimates of the
# mean are further apparent for v=0.2 relative to v=0.80. Again, this makes sense
# when the copula is visualized even at small n let alone large n.

# See additional Examples under EvuCOP().

\dontrun{
  set.seed(1)
  n <- 5000; Vlo <- rep(0.001, n); Vhi <- rep(0.95, n); Theta <- 3
  Ulo <- simCOPmicro(Vlo, cop=JOcopB5, para=Theta); dlo <- density(Ulo)
  Uhi <- simCOPmicro(Vhi, cop=JOcopB5, para=Theta); dhi <- density(Uhi)
  dlo$x[dlo$x < 0] <- 0; dhi$x[dhi$x < 0] <- 0
  dlo$x[dlo$x > 1] <- 1; dhi$x[dhi$x > 1] <- 1

  summary(Ulo)
  Ulomu <- EuvCOP(Vlo[1], cop=JOcopB5, para=Theta); print(Ulomu)
  #      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
  # 0.0000669 0.0887330 0.2006123 0.2504796 0.3802847 0.9589315
  #                  Ulomu -----> 0.2502145
  summary(Uhi)
  Uhimu <- EuvCOP(Vhi[1], cop=JOcopB5, para=Theta); print(Uhimu)
  #      Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
  #   0.01399  0.90603    0.93919 0.9157600   0.95946   0.99411
  #                  Uhimu -----> 0.9154093

  UV <- simCOP(n, cop=JOcopB5, para=Theta,
                  cex=0.6, pch=21, bg="palegreen", col="darkgreen")
  abline(h=Vlo[1], col="salmon", lty=3) # near the bottom to form datum for density
  abline(h=Vhi[1], col="purple", lty=3) # near the   top  to form datum for density
  lines(dlo$x,   dlo$y/max(dlo$y)/2 +    Vlo[1],  col="salmon", lwd=2)
  # re-scaled density along the line already drawn near the bottom (Vlo)
  # think rug plotting to bottom the values plotting very close to the line
  lines(dhi$x, 1-dhi$y/max(dhi$y)/2 - (1-Vhi[1]), col="purple", lwd=2)
  # re-scaled  density along the line already drawn near the  top  (Vhi)
  # think rug plotting to bottom the values plotting very close to the line
  uv <- seq(0.001, 0.999, by=0.001) # for trajectory of E[U | V=v]
  lines(EuvCOP(uv, cop=JOcopB5, para=Theta), uv, col="blue", lwd=3.5, lty=2)
  points(Ulomu, Vlo[1], pch=16, col="salmon", cex=2)
  points(Uhimu, Vhi[1], pch=16, col="purple", cex=2) #}
}
\keyword{copula (utility)}
\keyword{copula operator}
\keyword{copula (conditional distribution)}
\keyword{conditional expectation}
\keyword{mean U given V}
\concept{expectation}
