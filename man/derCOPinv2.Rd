\name{derCOPinv2}
\alias{derCOPinv2}
\title{ Numerical Derivative Inverse of a Copula for U with respect to V }
\description{
Compute the inverse of a numerical partial derivative for \eqn{U} with respect to \eqn{V} of a copula, which is a \emph{conditional quantile function} for nonexceedance probability \eqn{t}, or
\deqn{t = c_v(u) = \mathbf{C}^{(-1)}_{1|2}(u|v) =  \frac{\delta \mathbf{C}(u,v)}{\delta v}\mbox{,}}
and solving for \eqn{u}. Nelsen (2006, pp. 13, 40--41) shows that this inverse is quite important for random variable generation using the \emph{conditional distribution method}. This function is not vectorized and will not be so.
}
\usage{
derCOPinv2(cop=NULL, v, t, trace=FALSE,
           delv=.Machine$double.eps^0.50, para=NULL, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{v}{A single nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{t}{A single nonexceedance probability level \eqn{t};}
  \item{trace}{A logical controlling a \code{message} on whether the signs on the \code{uniroot} are the same---this is helpful in exploring the numerical derivative limits of a given implementation of a copula.}
  \item{delv}{The \eqn{\Delta v} interval for the derivative;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to \code{cop}; and}
  \item{...}{Additional arguments to pass to the copula.}
}
\value{
  Value(s) for the derivative inverse are returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{derCOP2}} }
\examples{
u <- runif(1); t <- runif(1)
derCOPinv2(u,t, cop=W)   # perfect negative dependence
derCOPinv2(u,t, cop=P)   # independence
derCOPinv2(u,t, cop=M)   # perfect positive dependence
derCOPinv2(u,t, cop=PSP) # a parameterless copula example
\dontrun{
# Simulate 500 values from product (independent) copula
plot(NA,NA, type="n", xlim=c(0,1), ylim=c(0,1), xlab="U", ylab="V")
for(i in 1:500) {
   v <- runif(1); t <- runif(1)
   points(derCOPinv2(cop=P,v,t),v, cex=0.5, pch=16) # black dots
}
# Simulate 500 of a Frechet Family copula and note crossing singularities.
for(i in 1:500) {
   v <- runif(1); t <- runif(1)
   u <- derCOPinv2(v,t, cop=FRECHETcop, para=list(alpha=0.7, beta=0.169))
   points(u,v, cex=2, pch=16, col=2) # red dots
} #}
}
\keyword{copula (utility)}
\keyword{copula (derivative inverse)}
\keyword{conditional quantile function}
\keyword{copula operator}
\keyword{derivative}
\keyword{copula (conditional quantile function)}

