\encoding{utf8}
\name{PLACKETTsim}
\alias{PLACKETTsim}
\title{ Direct Simulation of a Plackett Copula }
\description{
Simulation of the \emph{Plackett copula} (Nelsen, 2006, pp. 89--92) is made by this function using analytical formula (Durante, 2007, p. 247; see source code). Thus the \code{PLACKETTsim} function can be useful for comparison against the numerical derivative (\emph{conditional distribution method}) methods (\code{\link{simCOP}}, \code{\link{simCOPmicro}}) otherwise used in \pkg{copBasic}. The documentation for \code{\link{PLACKETTcop}} provides the mathematical formula of the Plackett copula.
}
\usage{
PLACKETTsim(n, para=NULL, ...)
}
\arguments{
  \item{n}{Sample size;}
  \item{para}{The \eqn{\Theta} parameter of the Plackett copula; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  An \R \code{data.frame} of the values \eqn{U} and \eqn{V} for the nonexceedance probabilities is returned.
}
\references{
Durante, F., 2007, Families of copulas, Appendix C, \emph{in} Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{PLACKETTcop}}, \code{\link{PLACKETTpar}} }
\examples{
PLACKETTsim(10, para= 1  ) # simulate P (independence) copula through a Plackett
PLACKETTsim(10, para=20.3) # simulate strong positive Plackett
}
\keyword{copula (simulation)}
\keyword{Plackett copula}

