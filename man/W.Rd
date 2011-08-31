\name{W}
\alias{W}
\title{ The Frechet-Hoeffding Lower Bound }
\description{
Compute the Frechet-Hoeffding lower bound copula
\deqn{\mathbf{W}(u,v) = \mathrm{max}(u+v-1,0)}
}
\usage{
W(u, v, ...)
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
See p. 11 of the reference.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{M}}, \code{\link{P}}}
\examples{
W(0.4,0.6)
W(0,0)
W(1,1)
}
\keyword{distribution}
\keyword{multivariate}
