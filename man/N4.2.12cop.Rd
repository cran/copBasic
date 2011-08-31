\name{N4.2.12cop}
\alias{N4.2.12cop}
\title{ The Copula of Equation 4.2.12 of Nelson's Book }
\description{
The N4.2.12 Copula is
\deqn{\mathbf{C}_{\Theta}(u,v) = (1 + [(u^{-1} -1)^\Theta + (v^{-1} -1)^\Theta]^{1/\Theta})^{-1}}

The N4.2.12 Copula is not comprehensive because \eqn{\Theta = 0} the copula becomes \eqn{\mathbf{PSP}(u,v)}(\code{\link{PSP}}), as \eqn{\Theta \rightarrow \infty} the copula becomes \eqn{\mathbf{M}(u,v)} (\code{\link{M}}). The copula is undefined for \eqn{\Theta < 1}. The N4.2.12 Copula  has respective lower and upper tail dependency and the \code{\link{simCOP}} examples in this documentation will show.
}
\usage{
N4.2.12cop(u, v, para=NULL, infis=100, ...)
}
\arguments{
  \item{u}{A nonexceedance probability in X direction,}
  \item{v}{A nonexceedance probability in Y direction,}
  \item{para}{A vector (single element) of parameters---the \eqn{\Theta} parameter,}
  \item{infis}{What is infinity? Testing shows that > 100 is about right to consider the copula as becoming \eqn{\mathbf{M}(u,v)} (\code{\link{M}}), and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for the copula is returned.
}
\note{
See p. 91 of the reference.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\examples{
N4.2.12cop(0.4,0.6,para=1)
PSP(0.4,0.6) # same two values


N4.2.12cop(0.4,0.6,para=10)

\dontrun{
D <- simCOP(n=400, cop=N4.2.12cop, para=2)
D <- simCOP(n=400, cop=N4.2.12cop, para=10)
D <- simCOP(n=400, cop=N4.2.12cop, para=100)
}
}
\keyword{distribution}
\keyword{multivariate}
