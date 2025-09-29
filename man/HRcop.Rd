\encoding{utf8}
\name{HRcop}
\alias{HRcop}
\title{The \enc{Hüsler}{Husler}--Reiss Extreme Value Copula}
\description{
The \emph{\enc{Hüsler}{Husler}--Reiss copula} (Joe, 2014, p. 176) is
\deqn{\mathbf{C}_{\Theta}(u,v) = \mathbf{HR}(u,v) = \mathrm{exp}\bigr[-x \Phi(X) - y\Phi(Y)\bigr]\mbox{,}}
where \eqn{\Theta \ge 0}, \eqn{x = - \log(u)}, \eqn{y = - \log(v)}, \eqn{\Phi(.)} is the cumulative distribution function of the standard normal distribution, \eqn{X} and \eqn{Y} are defined as:
\deqn{X = \frac{1}{\Theta} + \frac{\Theta}{2}  \log\biggl(\frac{x}{y}\biggr)\mbox{\ and\ } Y = \frac{1}{\Theta} + \frac{\Theta}{2} \log\biggl(\frac{y}{x}\biggr)\mbox{.}}
As \eqn{\Theta \rightarrow 0^{+}}, the copula limits to \emph{independence} (\eqn{\mathbf{\Pi}}; \code{\link{P}}). The \eqn{\mathbf{HR}} copula is a \emph{bivariate extreme value copula} (\eqn{BEV}), and the parameter \eqn{\Theta} requires numerical methods. Because there is \dQuote{hardly any practical difference among [these] bivariate exchangeable parametric families} distiguishing between these (\code{\link{GHcop}}, \code{\link{GLcop}}, and \code{\link{HRcop}}) \dQuote{requires extremely large sample sizes} (Hofert \emph{et al.}, 2016, p. 117). This observation is strongly supported by the \emph{L-comoment ratio diagrams} explored in \code{\link{LCOMDIA_ManyCops}}.
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
Hofert, M., Kojadinovic, I., \enc{Mächler}{Machler},  M., and Yan, J., 2018, Elements of copula modeling with R: Dordrecht, Netherlands, Springer.

Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{P}}, \code{\link{GHcop}}, \code{\link{GLcop}}, \code{\link{tEVcop}}
}
\examples{
# Parameter Theta = pi recovery through the Blomqvist Beta (Joe, 2014, p. 176)
qnorm(1 - log( 1 + blomCOP(cop=HRcop, para=pi) ) / ( 2 * log(2) ) )^(-1)
}
\keyword{Husler--Reiss copula}
\keyword{copula (formulas)}
\keyword{copula}
\concept{Husler--Reiss extreme value copula}
\concept{Husler-Reiss extreme value copula}
\concept{Husler-Reiss copula}
\concept{Husler--Reiss copula}
\keyword{Joe (2014) Examples and Exercises}
\keyword{copula (extreme value)}
\keyword{extreme value copula}
