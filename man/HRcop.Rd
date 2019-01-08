\encoding{utf8}
\name{HRcop}
\alias{HRcop}
\title{ The \enc{Hüsler}{Husler}--Reiss Extreme Value Copula}
\description{
The \emph{\enc{Hüsler}{Husler}--Reiss copula} (Joe, 2014, p. 176) is
\deqn{\mathbf{C}_{\Theta}(u,v) = \mathbf{HR}(u,v) = \mathrm{exp}\bigr[-x \Phi(X) - y\Phi(Y)\bigr]\mbox{,}}
where \eqn{\Theta \ge 0}, \eqn{x = - \log(u)}, \eqn{y = - \log(v)}, \eqn{\Phi(.)} is the cumulative distribution function of the standard normal distribution, \eqn{X} and \eqn{Y} are defined as:
\deqn{X = \frac{1}{\Theta} + \frac{\Theta}{2}  \log[x/y]\mbox{\ and\ } Y = \frac{1}{\Theta} + \frac{\Theta}{2} \log[y/x]\mbox{.}}
As \eqn{\Theta \rightarrow 0^{+}}, the copula limits to \emph{independence} (\eqn{\mathbf{\Pi}}; \code{\link{P}}). The copula here is a \emph{bivariate extreme value copula} (\eqn{BEV}), and the parameter \eqn{\Theta} requires numerical methods.
}
\usage{
HRcop(u, v, para=NULL, ...)
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
\seealso{\code{\link{P}}, \code{\link{GHcop}}, \code{\link{GLcop}}, \code{\link{tEVcop}}
}
\examples{
# Parameter Theta = pi recovery through Blomqvist's Beta (Joe, 2014, p. 176)
qnorm(1 - log(1+blomCOP(cop=HRcop, para=pi))/(2*log(2)))^(-1)
}
\keyword{Husler--Reiss copula}
\keyword{copula (formulas)}
\keyword{copula}
\concept{\enc{Hüsler}{Husler}--Reiss extreme value copula}
\concept{\enc{Hüsler}{Husler}-Reiss extreme value copula}
\concept{Husler--Reiss extreme value copula}
\concept{Husler-Reiss extreme value copula}
\keyword{Joe (2014) Examples and Exercises}
\keyword{copula (extreme value)}
\keyword{extreme value copula}
