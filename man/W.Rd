\encoding{utf8}
\name{W}
\alias{W}
\title{ The \enc{Fréchet}{Frechet}-Hoeffding Lower Bound Copula }
\description{
Compute the \emph{\enc{Fréchet}{Frechet}-Hoeffding lower bound copula} (Nelsen, 2006, p. 11), which is defined as
\deqn{\mathbf{W}(u,v) = \mathrm{max}(u+v-1,0)\mbox{.}}
This is the copula of perfect anti-association (\emph{countermonotonicity}, \emph{perfectly negative dependence}) between \eqn{U} and \eqn{V} and is sometimes referred to as the \emph{countermonotonicity copula}. Its opposite is the \eqn{\mathbf{M}(u,v)} copula (\emph{comonotonicity copula}; \code{\link{M}}), and statistical \emph{independence} is the \eqn{\mathbf{\Pi}(u,v)} copula (\code{\link{P}}).
}
\usage{
W(u, v, ...)
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
\seealso{\code{\link{M}}, \code{\link{P}}}
\examples{
W(0.4,0.6)
W(0,0)
W(1,1)
}
\keyword{copula (formulas)}
\keyword{copula (limits/bounds)}
\concept{Frechet upper bound copula}
\concept{Frechet-Hoeffding upper bound copula}
\concept{Frechet upper bound}
\concept{Frechet-Hoeffding upper bound}
\keyword{copula}

