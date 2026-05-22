\encoding{utf8}
\name{gEVcop}
\alias{gEVcop}
\title{The Gaussian-based (Extreme Value) Copula}
\description{
The \emph{g-EV copula} (Joe, 2014, p. 105) is a limiting form of the \emph{Normal (Gaussian) copula} (\code{\link{NORMcop}}):
\deqn{
\mathbf{C}_{\rho}(u,v) = \mathbf{gEV}(u,v; \rho) =
\mathrm{exp}\bigl(-A(x,y; \rho)\bigr)\mbox{,}
}
where \eqn{x = -\log(u)}, \eqn{y = -\log(v)}, and
\deqn{
A(x,y; \rho) = y\mbox{,}
}
for \eqn{0 \le x/(x+y) \le \rho^2/(1+\rho^2)},
\deqn{
A(x,y; \rho) = (x+y - 2\rho\sqrt{xy})/(1-\rho^2)\mbox{,}
}
for \eqn{\rho^2/(1+\rho^2) \le x/(x+y) \le 1/(1+\rho^2)},
\deqn{
A(x,y; \rho) = x\mbox{,}
}
for \eqn{1/(1+\rho^2) \le x/(x+y) \le 1} and where \eqn{\rho \in [0,1]}. A somewhat curious observation is that this copula has relative hard boundaries into the upper-left and lower-right corners when compared to the other copulas supported by the \pkg{copBasic} package. In other words, the hull defined by the copula has a near hard (not fuzzy) curvilinear boundaries that adjust with the parameter \eqn{\rho}. The lower-case \code{g} in the copula name is deliberately to avoid the sequence \code{GEV} as to never be confused with the \emph{generalized extreme value distribution}.
}
\usage{
gEVcop(u, v, para=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{The parameter \eqn{\rho}; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{tEVcop}}, \code{\link{NORMcop}}
}
\examples{
\dontrun{
  UV <- simCOP(200, cop=gEVcop, para=0.8) #}

\dontrun{
  # Joe (2014, p. 105) has brief detail indicating rho = [0,1] and though it seems
  # Rho would be a Pearson correlation, this does not seem to be the case. The Rho
  # seems to start with that of the Gaussian and then through the extreme-value
  # transform, it just assumes the role of a parameter called Rho.
  rho <- 0.8
  UV <- simCOP(2000, cop=gEVcop, para=rho)
  P <- cor(UV[,1], UV[,2], method="pearson")
  if(abs(P - rho) < 0.001) {
    print("Yes same")
  } else { print("nope not") } # }

\dontrun{
  gEVparameter <- seq(0.02, 1, by=0.02)
  SpearmanRho <- sapply(gEVparameter, function(k) rhoCOP(cop=gEVcop, para=k))
  plot(gEVparameter, SpearmanRho)

  for(rho in gEVparameter) {
    UV <- simCOP(n=1000, cop=gEVcop, para=rho)
    mtext(paste0("Parameter = ", rho))
  } # }
}
\keyword{g-EV copula}
\keyword{copula (formulas)}
\keyword{copula}
\keyword{package copula (comparison to)}
\concept{g-EV copula}
\keyword{copula (extreme value)}
\keyword{extreme value copula}
\keyword{Spearman Rho}
\keyword{Pearson correlation coefficient}
