\name{derCOPinv}
\alias{derCOPinv}
\title{ Numerical Derivative Inverse  of a Copula for V with respect to U }
\description{
Compute the inverse of a numerical partial derivative for \eqn{V} with respect to \eqn{U} of a copula or
\deqn{t = c_u(v) = \frac{\delta \mathbf{C}(u,v)}{\delta u}\mbox{,}}
and solving for \eqn{v}. Nelsen (2006, pp. 13, 40--41) shows that this inverse is quite important for random variable generation. This function is not vectorized.
}
\usage{
derCOPinv(cop=NULL, u, t, delu=.Machine$double.eps^0.50, para=NULL, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{u}{A single nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{t}{A single nonexceedance probability level \eqn{t};}
  \item{delu}{The \eqn{\Delta u} interval for the derivative;}
  \item{para}{Vector of parameters or other data structures, if needed, to pass to \code{cop}; and}
  \item{...}{Additional arguments to pass to the copula.}
}
\value{
  Value(s) for the derivative inverse are returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{derCOP}}}
\examples{
u <- runif(1); t <- runif(1)
derCOPinv(cop=W,u,t); derCOPinv(cop=P,u,t); derCOPinv(cop=M,u,t)
derCOPinv(cop=PSP,u,t)

\dontrun{
# Simulate 1000 values from product (independent) copula
plot(c(0,1),c(0,1), type="n")
for(i in 1:1000) {
   u <- runif(1); t <- runif(1); v <- derCOPinv(cop=P,u,t)
   points(u,v, cex=0.5, pch=16) # black dots
}
# Now simulate 1000 from the PSP copula and note the lower tail
# dependence, but absence of upper tail dependence.
for(i in 1:1000) {
   u <- runif(1); t <- runif(1); v <- derCOPinv(cop=PSP,u,t)
   points(u,v, cex=0.5, pch=16, col=2) # red dots
}
}
}
\keyword{copula (utility)}
\keyword{copula (derivative inverse)}
\keyword{conditional quantile function}
\keyword{copula operator}
\keyword{derivative}


