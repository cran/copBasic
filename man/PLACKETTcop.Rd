\name{PLACKETTcop}
\alias{PLACKETTcop}
\title{ The Plackett Copula }
\description{
The Plackett Copula is
\deqn{\mathbf{C}_{\Theta}(u,v) = \frac{[1+(\Theta-1)(u+v)]-\sqrt{[1+(\Theta-1)(u+v)]^2 - 4uv\Theta(\Theta-1)}}{2(\Theta - 1)}}

The Plackett family is comprehensive because as \eqn{\Theta \rightarrow 0} the copula becomes \eqn{\mathbf{W}(u,v)}(\code{\link{W}}), as \eqn{\Theta \rightarrow \infty} the copula becomes \eqn{\mathbf{M}(u,v)} (\code{\link{M}}) and for \eqn{\Theta = 1} the copula is \eqn{\Pi(u,v)} (\code{\link{P}}, independence). The Plackett family has been widely used in modeling and as an alternative to bivariate distributions. The Plackett family has respective lower and upper tail dependencies of \eqn{\lambda_L = 0} and \eqn{\lambda_U = 0}.
}
\usage{
PLACKETTcop(u, v, para=NULL, ...)
}
\arguments{
  \item{u}{A nonexceedance probability in X direction,}
  \item{v}{A nonexceedance probability in Y direction,}
  \item{para}{A vector (single element) of parameters---the \eqn{\Theta} parameter, and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for the copula is returned.
}
\note{
See p.~91 of the reference.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269~p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{PLACKETTpar}}, \code{\link{PLACKETTsim}}, \code{\link{W}}, and \code{\link{M}}
}
\examples{
PLACKETTcop(0.4,0.6,para=c(1))
P(0.4,0.6) # same two values


PLACKETTcop(0.4,0.6,para=c(10))
}
\keyword{distribution}
\keyword{multivariate}
