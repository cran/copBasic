\encoding{utf8}
\name{N4220cop}
\alias{N4220cop}
\title{The Copula of Equation 4.2.20 of Nelsen's Book}
\description{
The \emph{N4220 copula} (Nelsen, 2006, p. 118; eq. 4.2.20) is named by the author (Asquith) for the \pkg{copBasic} package and is defined as

\deqn{\mathbf{C}_{\mathrm{N4220}}(u,v; \Theta) = \biggl[\mathrm{log}\biggl( \mathrm{exp}\bigl(u^{-\Theta}\bigr) +
\mathrm{exp}\bigl(v^{-\Theta}\bigr) -
\mathrm{exp}(1)\biggr)\biggr]^{-1/\Theta}\mbox{.}}

The \eqn{\mathbf{N4220}(u,v)} copula is \emph{not comprehensive} because for \eqn{\Theta = 0^{+}} the copula becomes the \emph{independence} \eqn{\mathbf{P}(u,v)} copula (\code{\link{P}}) and as \eqn{\Theta \rightarrow \infty} the copula becomes \eqn{\mathbf{M}(u,v)} (\code{\link{M}}). The copula is undefined for \eqn{\Theta < 0}. This copula has \emph{lower-tail dependency}, which is the reverse of the \eqn{\mathbf{rN4220}(u,v)} that has \emph{uppr-tail dependency.} Hence, a reflected (refer to \code{\link{COP}}, \code{reflect=2}) version of this copula is natively supported as the \code{\link{rN4220cop}} to put the tail dependency into the upper-right corner and more \dQuote{hydrologic-system like.}
}
\usage{
N4220cop(u, v, para=NULL, insertM=TRUE, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (single element) of parameters---the \eqn{\Theta} parameter of the copula;}
  \item{insertM}{For the tail that pinches out towards \eqn{\mathbf{M}(u,v)} (\code{\link{M}}) use an \emph{ad hoc} algorithm to insert that copula into the tail (refer to source code) and that algorithm is dependent on the \code{\link{simCOP}} and the numerical partial derivative inversion for simulation that the package is based around; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\examples{
sapply(c(0.1, 1, 10), function(p) N4220cop(0.4, 0.6, para=p))
# 0.2611771 0.3714941 0.4000000 # perfect correlation controls for p=10
}
\keyword{copula (formulas)}
\keyword{copula}
