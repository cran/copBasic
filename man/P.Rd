\name{P}
\alias{P}
\title{ The Product (Independence) Copula }
\description{
Compute the product copula
\deqn{\Pi(u,v) = uv}
}
\usage{
P(u, v, ...)
}
\arguments{
  \item{u}{A nonexceedance probability in X direction,}
  \item{v}{A nonexceedance probability in Y direction, and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for the copula is returned.
}
\note{
See p. 12 of the reference.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{M}}, \code{\link{W}}}
\examples{
P(0.4,0.6)
P(0,0)
P(1,1)
}
\keyword{distribution}
\keyword{multivariate}
