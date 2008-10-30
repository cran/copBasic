\name{derCOPinv2}
\alias{derCOPinv2}
\title{ The Inverse of a Numeric Derivative for U with respect to V of a Copula }
\description{
Compute the inverse of a numeric derivative for \eqn{U} with respect to \eqn{V} of a copula or
\deqn{t = c_v(u) = \frac{\delta \mathbf{C}(u,v)}{\delta v}\mbox{,}}
and solving for \eqn{u}.
}
\usage{
derCOPinv2(cop=NULL, v, t, delv=.Machine$double.eps^0.50,
           para=NULL, ...)
}
\arguments{
  \item{cop}{A copula function,}
  \item{v}{A nonexceedance probability in \eqn{Y} direction,}
  \item{t}{A nonexceedance probability in \eqn{t} direction,}
  \item{delv}{The \eqn{\delta v} interval for the derivative,}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula, and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for the copula is returned.
}
\note{
See p.~13,~40--41 of the reference.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269~p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{derCOP2}}, \code{\link{W}}, \code{\link{P}}, \code{\link{M}}, \code{\link{PSP}}}
\examples{
v <- runif(1)
t <- runif(1)
derCOPinv2(cop=W,v,t)
derCOPinv2(cop=P,v,t)
derCOPinv2(cop=M,v,t)
derCOPinv2(cop=PSP,v,t)

# Simulate 1000 values from product (independent)
# copula
plot(c(0,1),c(0,1),type="n")
for(i in 1:1000) {
  v <- runif(1); t <- runif(1)
  u <- derCOPinv2(cop=P,v,t)
  points(u,v,cex=0.5,pch=16)
}
# Now simulate 1000 from the PSP copula and note
# the lower tail dependence, but absence of upper
# tail dependence.
for(i in 1:1000) {
  v <- runif(1); t <- runif(1)
  u <- derCOPinv2(cop=PSP,v,t)
  points(u,v,cex=0.5,pch=16,col=2)
}

}
\keyword{distribution}
\keyword{multivariate}
