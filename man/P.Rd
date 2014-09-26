\name{P}
\alias{P}
\title{ The Product (Independence) Copula }
\description{
Compute the \emph{product copula} (Nelson, 2006, p. 12), which is defined as
\deqn{\mathbf{\Pi}(u,v) = uv\mbox{.}}
This is the copula of statistical independence between \eqn{U} and \eqn{V} and is sometimes referred to as the \emph{independence copula}.
}
\usage{
P(u, v, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for the copula is returned.
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
\keyword{copula (formulas)}
\concept{independence copula}
\concept{product copula}
\keyword{copula}

