\name{med.regressCOP}
\alias{med.regressCOP}
\title{ Perform Median Regression using a Copula by Numerical Derivative Method for V with respect to U}
\description{
Perform median regression of a copula by numerical derivatives of the copula. See the documentation for \code{\link{qua.regressCOP}} for mathematical details.
}
\usage{
med.regressCOP(U=seq(0.01,0.99, by=0.01),
               cop=NULL, para=NULL, ...)
}
\arguments{
  \item{U}{A vector of \eqn{u} nonexceedance probabilities,}
  \item{cop}{A copula function,}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula,}
  \item{...}{Additional arguments to pass.}
}
\value{
  A data frame of the regressed probabilities of \eqn{V} and provided \eqn{U} values is returned.
}
\note{
See pp. 217--218 of the reference.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{qua.regressCOP}}}
\examples{

theta <- 10
R <- med.regressCOP(cop=PLACKETTcop, para=c(theta))
plot(R$U,R$V, type="l", lwd=5, lty=2, xlim=c(0,1), ylim=c(0,1))
lines(R$U,(1+(theta-1)*R$U)/(theta+1), col=2)

theta <- 0.02
R <- med.regressCOP(cop=PLACKETTcop, para=c(theta))
lines(R$U,R$V, lwd=5, lty=2)
lines(R$U,(1+(theta-1)*R$U)/(theta+1), col=2)



plot(c(0,1),c(0,1), type="n", lwd=3,
     xlab="U, NONEXCEEDANCE PROBABILITY",
     ylab="V, NONEXCEEDANCE PROBABILITY")
# Draw the regression of V on U and then U on V (swap=TRUE)
qua.regressCOP.draw(F=seq(.1,.9,by=.1), cop=P, para=NA)
qua.regressCOP.draw(F=seq(.1,.9,by=.1), cop=P, para=NA,
                    swap=TRUE, lty=2)



theta <- 0.5
plot(c(0,1),c(0,1), type="n", lwd=3,
     xlab="U, NONEXCEEDANCE PROBABILITY",
     ylab="V, NONEXCEEDANCE PROBABILITY")
# Draw the regression of V on U and then U on V (swap=TRUE)
qua.regressCOP.draw(F=seq(.1,.9,by=.1), cop=PLACKETTcop, para=c(theta))
qua.regressCOP.draw(F=seq(.1,.9,by=.1), cop=PLACKETTcop, para=c(theta),
                    swap=TRUE, lty=2)

}
\keyword{distribution}
\keyword{multivariate}
