\name{derCOPinv}
\alias{derCOPinv}
\title{ The Inverse of a Numeric Derivative for V with respect to U of a Copula }
\description{
Compute the inverse of a numeric derivative for \eqn{V} with respect to \eqn{U} of a copula or
\deqn{t = c_u(v) = \frac{\delta \mathbf{C}(u,v)}{\delta u}\mbox{,}}
and solving for \eqn{v}.}
\usage{
derCOPinv(cop=NULL, u, t, delu=.Machine$double.eps^0.50,
          para=NULL, ...)
}
\arguments{
  \item{cop}{A copula function,}
  \item{u}{A nonexceedance probability in \eqn{X} direction,}
  \item{t}{A nonexceedance probability in \eqn{t} direction,}
  \item{delu}{The \eqn{\delta u} interval for the derivative,}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula, and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for the copula is returned.
}
\note{
See pp. 13 and 40--41 of the reference.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{derCOP}}, \code{\link{W}}, \code{\link{P}}, \code{\link{M}}, \code{\link{PSP}}}
\examples{
u <- runif(1)
t <- runif(1)
derCOPinv(cop=W,u,t)
derCOPinv(cop=P,u,t)
derCOPinv(cop=M,u,t)
derCOPinv(cop=PSP,u,t)

# Simulate 1000 values from product (independent)
# copula
plot(c(0,1),c(0,1),type="n")
for(i in 1:1000) {
  u <- runif(1); t <- runif(1)
  v <- derCOPinv(cop=P,u,t)
  points(u,v,cex=0.5,pch=16)
}
# Now simulate 1000 from the PSP copula and note
# the lower tail dependence, but absence of upper
# tail dependence.
for(i in 1:1000) {
  u <- runif(1); t <- runif(1)
  v <- derCOPinv(cop=PSP,u,t)
  points(u,v,cex=0.5,pch=16,col=2)
}

}
\keyword{distribution}
\keyword{multivariate}
