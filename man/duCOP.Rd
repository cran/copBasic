\name{duCOP}
\alias{duCOP}
\title{ The Dual of a Copula }
\description{
Compute the dual of a copula from a copula 

\deqn{\mathrm{Pr}[U \le v\mid V \le v] = \tilde{\mathbf{C}}(u,v) = u + v - \mathbf{C}(u,v)}

The dual of a copula is the expression for the probability that either \eqn{U \le u} or \eqn{V \le v}, which is obviously unlike a copula that has \eqn{U \le u} and \eqn{V \le v}.
}
\usage{
duCOP(cop=NULL, u, v, ...)
}
\arguments{
  \item{cop}{A copula function,}
  \item{u}{A nonexceedance probability in X direction,}
  \item{v}{A nonexceedance probability in Y direction, and}
  \item{...}{Additional arguments to pass (such as parameters, if needed, for the copula in the form of a list.}
}
\value{
  The value for the dual of a copula is returned.
}
\note{
See p.~33--34 of the reference.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269~p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{coCOP}}, \code{\link{surCOP}}, \code{\link{W}}, \code{\link{P}}, \code{\link{M}}, \code{\link{PSP}}}
\examples{
u <- runif(1)
t <- runif(1)
duCOP(cop=W,u,t)
duCOP(cop=P,u,t)
duCOP(cop=M,u,t)
duCOP(cop=PSP,u,t)
}
\keyword{distribution}
\keyword{multivariate}
