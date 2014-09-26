\name{PSP}
\alias{PSP}
\title{ The ratio of the Product Copula to Summation minus Product Copula }
\description{
Compute \emph{PSP copula} (Nelson, 2006, p. 23) is named by the author (Asquith) for the \pkg{copBasic} package and is
\deqn{\mathbf{PSP}(u,v) = \frac{\mathbf{\Pi}}{\Sigma - \mathbf{\Pi}}\mbox{,}}
where \eqn{\mathbf{\Pi}} is the \emph{product copula} (\code{\link{P}}) and \eqn{\Sigma} is the sum \eqn{\Sigma = u + v}. The \eqn{\mathbf{PSP}(u,v)} copula is a special case of the \code{\link{N4212cop}} copula. This copula is included in \pkg{copBasic} because of its simplicity and for pedagogic purposes. The name PSP comes from Product, Summation, Product to loosely reflect the mathematical formula shown.
}
\usage{
PSP(u, v, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction; and}
  \item{...}{Additional arguments to pass, which for this copula are not needed, but given here to support flexible implementation.}
}
\value{
  The value for the copula is returned.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{P}}, \code{\link{N4212cop}} }
\examples{
PSP(0.4,0.6)
PSP(0,0)
PSP(1,1)
}
\keyword{copula}

