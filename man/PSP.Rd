\encoding{utf8}
\name{PSP}
\alias{PSP}
\title{The Ratio of the Product Copula to Summation minus Product Copula}
\description{
Compute \emph{PSP copula} (Nelsen, 2006, p. 23) is named by the author (Asquith) for the \pkg{copBasic} package and is
\deqn{\mathbf{PSP}(u,v) = \frac{\mathbf{\Pi}}{\mathbf{\Sigma} - \mathbf{\Pi}} = \frac{uv}{u + v - uv}\mbox{,}}
where \eqn{\mathbf{\Pi}} is the \emph{indpendence} or \emph{product copula} (\code{\link{P}}) and \eqn{\mathbf{\Sigma}} is the sum \eqn{\mathbf{\Sigma} = u + v}. The \eqn{\mathbf{PSP}(u,v)} copula is a special case of the \eqn{\mathbf{N4212}(u,v)} copula (\code{\link{N4212cop}}). The \eqn{\mathbf{PSP}} is included in \pkg{copBasic} because of its simplicity and for pedagogical purposes. The name \dQuote{PSP} comes from \dQuote{Product, Summation, Product} to loosely reflect the mathematical formula shown.  Nelsen (2006, p. 114) notes that the PSP copula shows up in several families and designates it as \dQuote{\eqn{\mathbf{\Pi}/(\mathbf{\Sigma}-\mathbf{\Pi})}.} The PSP is undefined for \eqn{u = v = 0} but no internal trapping is made; calling functions will have to intercept the \code{NaN} so produced for \eqn{\{0, 0\}}. The \eqn{\mathbf{PSP}} is left internally untrapping \code{NaN} so as to be available to stress other copula utility functions within the \pkg{copBasic} package.
}
\usage{
PSP(u, v, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction; and}
  \item{...}{Additional arguments to pass, which for this copula are not needed, but given here to support flexible implementation.}
}
\value{
  Value(s) for the copula are returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{P}}, \code{\link{N4212cop}}}
\examples{
PSP(0.4,0.6)
PSP(0,0)
PSP(1,1)
}
\keyword{copula (formulas)}
\keyword{copula}

