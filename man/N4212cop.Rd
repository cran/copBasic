\encoding{utf8}
\name{N4212cop}
\alias{N4212cop}
\title{ The Copula of Equation 4.2.12 of Nelsen's Book }
\description{
The \emph{N4212 copula} (Nelsen, 2006, p. 91; eq. 4.2.12) is named by the author (Asquith) for the \pkg{copBasic} package and is defined as
\deqn{\mathbf{C}_{\mathrm{N4212}}(u,v; \Theta) = \biggl(1 + \bigl[(u^{-1} -1)^\Theta + (v^{-1} -1)^\Theta\bigr]^{1/\Theta}\biggr)^{-1}\mbox{.}}

The \eqn{\mathbf{N4212}(u,v)} copula is \emph{not comprehensive} because for \eqn{\Theta = 1} the copula becomes the so-called \eqn{\mathbf{PSP}(u,v)} copula (see \code{\link{PSP}}) and as \eqn{\Theta \rightarrow \infty} the copula becomes \eqn{\mathbf{M}(u,v)} (see \code{\link{M}}). The copula is undefined for \eqn{\Theta < 1}. The N4212 copula  has respective \emph{lower-} and \emph{upper-tail dependency} (see \code{\link{taildepCOP}}).

Although \pkg{copBasic} is intended to not implement or \dQuote{store house} the enormous suite of copula functions available in the literature, the N4212 copula is included to give the package another copula to test against.
}
\usage{
N4212cop(u, v, para=NULL, infis=100, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (single element) of parameters---the \eqn{\Theta} parameter of the copula;}
  \item{infis}{What is infinity? Testing shows that \code{infis =} \eqn{\Theta > 100} is about right to consider the copula as becoming \eqn{\mathbf{M}(u,v)} (see \code{\link{M}}); and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\examples{
N4212cop(0.4,0.6, para=1) == PSP(0.4,0.6) # TRUE
N4212cop(0.4,0.6, para=10) # 0.3999928
taildepCOP(cop=N4212cop, para=10) # LamL = 0.93303; LamU = 0.92823
\dontrun{
D <- simCOP(n=400, cop=N4212cop, para=2)
D <- simCOP(n=400, cop=N4212cop, para=10,  ploton=FALSE, col=2)
D <- simCOP(n=400, cop=N4212cop, para=100, ploton=FALSE, col=3)#}
}
\keyword{copula (formulas)}
\keyword{copula}
