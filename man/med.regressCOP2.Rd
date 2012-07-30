\name{med.regressCOP2}
\alias{med.regressCOP2}
\title{ Perform Median Regression using a Copula by Numerical Derivative Method for U with respect to V}
\description{
Perform median regression of a copula by numerical derivatives of the copula. See the documentation for \code{\link{qua.regressCOP2}} for mathematical details.
}
\usage{
med.regressCOP2(V=seq(0.01,0.99, by=0.01),
                cop=NULL, para=NULL, ...)
}
\arguments{
  \item{V}{A vector of \eqn{v} nonexceedance probabilities,}
  \item{cop}{A copula function,}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula,}
  \item{...}{Additional arguments to pass.}
}
\value{
  A data frame of the regressed probabilities of \eqn{U} and provided \eqn{V} values is returned.
}
\note{
See pp. 217--218 of the reference.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{qua.regressCOP2}}}
\examples{

theta <- 10
R <- med.regressCOP2(cop=PLACKETTcop, para=c(theta))
plot(R$U,R$V, type="l", lwd=5, lty=2, xlim=c(0,1), ylim=c(0,1))
lines((1+(theta-1)*R$V)/(theta+1),R$V, col=4)

theta <- 0.02
R <- med.regressCOP2(cop=PLACKETTcop, para=c(theta))
lines(R$U,R$V, lwd=5, lty=2)
lines((1+(theta-1)*R$V)/(theta+1),R$V, col=2)
}
\keyword{distribution}
\keyword{multivariate}
