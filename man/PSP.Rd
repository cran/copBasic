\name{PSP}
\alias{PSP}
\title{ The ratio of the Product Copula to Summation minus Product Copula }
\description{
Compute the ratio of the product copula to the summation minus product copula
\deqn{PSP(u,v) = \frac{\Pi}{\Sigma - \Pi}}
where \eqn{\Pi} is the product copula (\code{\link{P}}) and \eqn{\Sigma} is the sum \eqn{\Sigma = u + v}.
}
\usage{
PSP(u, v, ...)
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
See p.~23 of the reference.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269~p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{M}}, \code{\link{W}}, \code{\link{P}}}
\examples{
PSP(0.4,0.6)
PSP(0,0)
PSP(1,1)
}
\keyword{distribution}
\keyword{multivariate}
