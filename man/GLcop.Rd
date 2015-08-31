\encoding{utf8}
\name{GLcop}
\alias{GLcop}
\title{ The Galambos Extreme Value Copula}
\description{
The \emph{Galambos copula} (Joe, 2014, p. 174) is
\deqn{\mathbf{C}_{\Theta}(u,v) = \mathbf{GL}(u,v) = uv\,\mathrm{exp}\{[x^{-\Theta} + y^{-\Theta}]^{-1/\Theta}\}\mbox{,}}
where \eqn{\Theta \in [0, \infty)}, \eqn{x = -\log(u)}, and \eqn{y = -\log(v)}. As \eqn{\Theta \rightarrow 0^{+}}, the copula limits to \emph{independence} (\eqn{\mathbf{\Pi}}; \code{\link{P}}) and as  \eqn{\Theta \rightarrow \infty}, the copula limits to perfect association (\eqn{\mathbf{M}}; \code{\link{M}}).
The copula here is a \emph{bivariate extreme value copula} (\eqn{BEV}), and the parameter \eqn{\Theta} requires numerical methods.
}
\usage{
GLcop(u, v, para=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (single element) of parameters---the \eqn{\Theta} parameter of the copula; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{M}}, \code{\link{P}}, \code{\link{GHcop}}, \code{\link{HRcop}}, \code{\link{rhobevCOP}}
}
\examples{
# Parameter Theta = pi recovery through Blomqvist Beta (Joe, 2014, p. 175)
log(2)/(log(log(2)/log(1+blomCOP(cop=GLcop, para=pi))))
}
\keyword{Galambos copula}
\keyword{copula (formulas)}
\keyword{copula}
\concept{Galambos extreme value copula}
\keyword{Joe (2014) Examples and Exercises}
