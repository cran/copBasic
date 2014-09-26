\name{N4212cop}
\alias{N4212cop}
\title{ The Copula of Equation 4.2.12 of Nelson's Book }
\description{
The \emph{N4212 copula} (Nelson, 2006, p. 91) is named by the author for the \pkg{copBasic} package and is defined as
\deqn{\mathbf{C}_{\Theta}(u,v) = (1 + [(u^{-1} -1)^\Theta + (v^{-1} -1)^\Theta]^{1/\Theta})^{-1}\mbox{.}}

The N4212 copula is \emph{not comprehensive} because for \eqn{\Theta = 1} the copula becomes the so-called \eqn{\mathbf{PSP}(u,v)} copula (see \code{\link{PSP}}) and as \eqn{\Theta \rightarrow \infty} the copula becomes \eqn{\mathbf{M}(u,v)} (see \code{\link{M}}). The copula is undefined for \eqn{\Theta < 1}. The N4212 copula  has respective \emph{lower-} and \emph{upper-tail dependency} (see \code{\link{taildepCOP}}).

Although \pkg{copBasic} is intended to not implement or \dQuote{store house} the enormous suite of copula functions in the literature, the N4212 copula is included to give the package another copula with a parameter besides the Plackett copula (see \code{\link{PLACKETTcop}}) largely for pedagogic purposes. But the Plackett is \emph{comprehensive} and N4212 is \emph{not comprehensive} so both those concepts are available for instructional purposes.
}
\usage{
N4212cop(u, v, para=NULL, infis=100, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (single element) of parameters---the \eqn{\Theta} parameter of the \emph{N4212} copula;}
  \item{infis}{What is infinity? Testing shows that \eqn{\Theta > 100} is about right to consider the copula as becoming \eqn{\mathbf{M}(u,v)} (see \code{\link{M}}); and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for the copula is returned.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\examples{
N4212cop(0.4,0.6,para=1) # 0.3157895
PSP(0.4,0.6) # 0.3157895
N4212cop(0.4,0.6,para=10)
taildepCOP(cop=N4212cop, para=10) # LamL = 0.93303; LamU = 0.92823
\dontrun{
D <- simCOP(n=400, cop=N4212cop, para=2)
D <- simCOP(n=400, cop=N4212cop, para=10)
D <- simCOP(n=400, cop=N4212cop, para=100)
}
}
\keyword{copula (formulas)}
\keyword{copula}
