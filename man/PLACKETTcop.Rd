\name{PLACKETTcop}
\alias{PLACKETTcop}
\title{ The Plackett Copula }
\description{
The \emph{Plackett copula} (Nelson, 2006, pp. 89--92) is
\deqn{\mathbf{C}_{\Theta}(u,v) = \frac{[1+(\Theta-1)(u+v)]-\sqrt{[1+(\Theta-1)(u+v)]^2 - 4uv\Theta(\Theta-1)}}{2(\Theta - 1)}\mbox{.}}

The Plackett copula is \emph{comprehensive} because as \eqn{\Theta \rightarrow 0} the copula becomes \eqn{\mathbf{W}(u,v)} (see \code{\link{W}}), as \eqn{\Theta \rightarrow \infty} the copula becomes \eqn{\mathbf{M}(u,v)} (see \code{\link{M}}) and for \eqn{\Theta = 1} the copula is \eqn{\mathbf{\Pi}(u,v)} (see \code{\link{P}},  independence). The Plackett copula has been widely used in modeling and as an alternative to bivariate distributions. The Plackett copula has respective \emph{lower-} and \emph{upper-tail dependency parameters} of \eqn{\lambda_L = 0} and \eqn{\lambda_U = 0} (see \code{\link{taildepCOP}}).
}
\usage{
PLACKETTcop(u, v, para=NULL, ...)
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
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{PLACKETTpar}}, \code{\link{PLACKETTsim}}, \code{\link{W}}, and \code{\link{M}}
}
\examples{
PLACKETTcop(0.4,0.6,para=1)
P(0.4,0.6) # same two values because Theta == 1 is Independence
PLACKETTcop(0.4,0.6,para=10.25)
}
\keyword{Plackett copula}
\keyword{copula (formulas)}
\concept{comprehensive copula}
\keyword{copula}
