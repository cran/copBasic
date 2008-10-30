\name{COPinv2}
\alias{COPinv2}
\title{ The Inverse of a Copula for U with respect to V }
\description{
Compute the inverse of a copula for \eqn{U} with respect to \eqn{V} or
\deqn{t = \mathbf{C}(u,v=V)\mbox{,}}
and solving for \eqn{u}.
}
\usage{
COPinv2(cop=NULL, v, t, para=NULL, ...)
}
\arguments{
  \item{cop}{A copula function,}
  \item{v}{A nonexceedance probability in \eqn{Y} direction,}
  \item{t}{A nonexceedance probability in \eqn{t} direction,}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula, and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for \eqn{u} is returned.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269~p.
}
\author{W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{COPinv}}}
\examples{
# 
}
\keyword{distribution}
\keyword{multivariate}
