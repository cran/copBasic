\name{med.regressCOP}
\alias{med.regressCOP}
\title{ Perform Median Regression using a Copula by Numerical Derivative Method for V with respect to U}
\description{
Perform \emph{median regression} (Nelson, 2006, pp. 217--218) of a copula by inversion of numerical derivatives of the copula. See the documentation for \code{\link{qua.regressCOP}} for mathematical details. The \code{\link{qua.regressCOP.draw}} supports so-called \emph{quantile regression} along numerous probability levels (see Examples).
}
\usage{
med.regressCOP(u=seq(0.01,0.99, by=0.01), cop=NULL, para=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula; and}
  \item{...}{Additional arguments to pass such \code{\link{qua.regressCOP}} and \code{\link{derCOPinv}} that are called in succession.}
}
\value{
  An \R \code{data.frame} of the median regressed probabilities of \eqn{V} and provided \eqn{U} values is returned.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{med.regressCOP2}}, \code{\link{qua.regressCOP}}, \code{\link{qua.regressCOP.draw}}
}
\examples{
theta <- 0.45
R <- med.regressCOP(cop=PLACKETTcop, para=theta)
plot(R$U,R$V, type="l", lwd=5, lty=2, xlim=c(0,1), ylim=c(0,1))
lines(R$U,(1+(theta-1)*R$U)/(theta+1), col=2)
theta <- 202
R <- med.regressCOP(cop=PLACKETTcop, para=theta)
lines(R$U,R$V, lwd=5, lty=2)
lines(R$U,(1+(theta-1)*R$U)/(theta+1), col=2)
\dontrun{
# INDEPENDENCE YIELDS STRAIGHT LINES, RED IS THE MEDIAN REGRESSION
F <- seq(0.1, 0.9, by=0.1)
plot(c(0,1),c(0,1), type="n", lwd=3,
     xlab="U, NONEXCEEDANCE PROBABILITY", ylab="V, NONEXCEEDANCE PROBABILITY")
# Draw the regression of V on U and then U on V (swap=TRUE)
qua.regressCOP.draw(f=F, cop=P, para=NA, ploton=FALSE)
qua.regressCOP.draw(f=F, cop=P, para=NA, ploton=FALSE, swap=TRUE, lty=2)
}
\dontrun{
# NEGATIVE PLACKETT THETA YIELDS CURVES DOWN TO RIGHT, RED IS THE MEDIAN REGRESSION
theta <- 0.5; F <- seq(0.1, 0.9, by=0.1)
plot(c(0,1),c(0,1), type="n", lwd=3,
     xlab="U, NONEXCEEDANCE PROBABILITY", ylab="V, NONEXCEEDANCE PROBABILITY")
# Draw the regression of V on U and then U on V (swap=TRUE)
qua.regressCOP.draw(f=F, cop=PLACKETTcop, ploton=FALSE, para=theta)
qua.regressCOP.draw(f=F, cop=PLACKETTcop, ploton=FALSE, para=theta, swap=TRUE, lty=2)
}
}
\keyword{median regression}
\keyword{quantile regression}
\concept{quantile regression}

