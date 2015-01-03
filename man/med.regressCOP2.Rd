\name{med.regressCOP2}
\alias{med.regressCOP2}
\title{ Perform Median Regression using a Copula by Numerical Derivative Method for U with respect to V}
\description{
Perform \emph{median regression} of a copula (Nelsen, 2006, pp. 217--218) by inversion of numerical derivatives of the copula. See the documentation for \code{\link{qua.regressCOP2}} for mathematical details.
}
\usage{
med.regressCOP2(v=seq(0.01,0.99, by=0.01), cop=NULL, para=NULL, ...)
}
\arguments{
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula; and}
  \item{...}{Additional arguments to pass such \code{\link{qua.regressCOP2}} and \code{\link{derCOPinv2}} that are called in succession.}
}
\value{
  An \R \code{data.frame} of the median regressed probabilities of \eqn{U} and provided \eqn{V} values is returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{med.regressCOP}}, \code{\link{qua.regressCOP2}}, \code{\link{qua.regressCOP.draw}}
}
\examples{
# See other examples under med.regressCOP
theta <- 10
R <- med.regressCOP2(cop=PLACKETTcop, para=theta)
\dontrun{
plot(R$U,R$V, type="l", lwd=5, lty=2, xlim=c(0,1), ylim=c(0,1))
lines((1+(theta-1)*R$V)/(theta+1),R$V, col=4)
theta <- 0.02
R <- med.regressCOP2(cop=PLACKETTcop, para=theta)
lines(R$U,R$V, lwd=5, lty=2)
lines((1+(theta-1)*R$V)/(theta+1),R$V, col=2)
}
}
\keyword{median regression}
\keyword{quantile regression}
\concept{quantile regression}


