\name{derCOPinv2}
\alias{derCOPinv2}
\title{ Numerical Derivative Inverse of a Copula for U with respect to V }
\description{
Compute the inverse of a numerical partial derivative for \eqn{U} with respect to \eqn{V} of a copula or
\deqn{t = c_v(u) = \frac{\delta \mathbf{C}(u,v)}{\delta v}\mbox{,}}
and solving for \eqn{u}. Nelson (2006, pp. 13, 40--41) shows that this inverse is quite important for random variable generation. This function is not vectorized.
}
\usage{
derCOPinv2(cop=NULL, v, t, delv=.Machine$double.eps^0.50, para=NULL, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{v}{A single nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{t}{A single nonexceedance probability level \eqn{t};}
  \item{delv}{The \eqn{\Delta v} interval for the derivative;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to \code{cop}; and}
  \item{...}{Additional arguments to pass to the copula.}
}
\value{
  Value(s) for the derivative inverse are returned.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{derCOP2}} }
\examples{
v <- runif(1); t <- runif(1)
derCOPinv2(cop=W,v,t); derCOPinv2(cop=P,v,t); derCOPinv2(cop=M,v,t)
derCOPinv2(cop=PSP,v,t)

\dontrun{
# Simulate 1000 values from product (independent) copula
plot(c(0,1),c(0,1), type="n")
for(i in 1:1000) {
   v <- runif(1); t <- runif(1); u <- derCOPinv2(cop=P,v,t)
   points(u,v, cex=0.5, pch=16) # black dots
}
# Now simulate 1000 from the PSP copula and note the lower tail
# dependence, but absence of upper tail dependence.
for(i in 1:1000) {
   v <- runif(1); t <- runif(1); u <- derCOPinv2(cop=PSP,v,t)
   points(u,v, cex=0.5, pch=16, col=2) # red dots
}
}
}
\keyword{copula (utility)}
\keyword{copula (derivative inverse)}
\keyword{conditional quantile function}
\keyword{copula operator}


