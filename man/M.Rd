\encoding{utf8}
\name{M}
\alias{M}
\title{ The \enc{Fréchet}{Frechet}--Hoeffding Upper Bound Copula }
\description{
Compute the \emph{\enc{Fréchet}{Frechet}--Hoeffding upper bound copula} (Nelsen, 2006, p. 11), which is defined as
\deqn{\mathbf{M}(u,v) = \mathrm{min}(u,v)\mbox{.}}
This is the copula of perfect association (\emph{comonotonicity}, \emph{perfectly positive dependence}) between \eqn{U} and \eqn{V} and is sometimes referred to as the \emph{comonotonicity copula}. Its opposite is the \eqn{\mathbf{W}(u,v)} copula (\emph{countermonotonicity copula}; \code{\link{W}}), and statistical \emph{independence} is the \eqn{\mathbf{\Pi}(u,v)} copula (\code{\link{P}}).
}
\usage{
M(u, v, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{W}}, \code{\link{P}}}
\examples{
M(0.4,0.6)
M(0,0)
M(1,1)
}
\keyword{copula (formulas)}
\keyword{copula (limits/bounds)}
\concept{Frechet upper bound copula}
\concept{Frechet-Hoeffding upper bound copula}
\concept{Frechet--Hoeffding upper bound copula}
\concept{Frechet upper bound}
\concept{Frechet-Hoeffding upper bound}
\concept{Frechet--Hoeffding upper bound}
\keyword{copula}

