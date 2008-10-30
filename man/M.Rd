\name{M}
\alias{M}
\title{ The Frechet-Hoeffding Upper Bound }
\description{
Compute the Frechet-Hoeffding upper bound copula
\deqn{\mathbf{M}(u,v) = \mathrm{min}(u,v)} 
}
\usage{
M(u, v, ...)
}
\arguments{
  \item{u}{A nonexceedance probability in X direction.}
  \item{v}{A nonexceedance probability in Y direction.}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for the copula is returned.
}
\note{
See p.~11 of the reference.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269~p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{W}}, \code{\link{P}}}
\examples{
M(0.4,0.6)
M(0,0)
M(1,1)
}
\keyword{distribution}
\keyword{multivariate}
