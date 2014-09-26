\name{PLACKETTsim}
\alias{PLACKETTsim}
\title{ Direct Simulation of a Plackett Copula }
\description{
Simulation of the \emph{Plackett copula} (Nelson, 2006, pp. 89--92) is made by this function. The documentation for \code{\link{PLACKETTcop}} provides the mathematical formula of the copula.
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
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{PLACKETTcop}}, \code{\link{PLACKETTpar}} }
\examples{
PLACKETTsim(10, para= 1  ) # simulate P (independence) copula through a Plackett
PLACKETTsim(10, para=20.3) # simulate strong positive Plackett
}
\keyword{copula (simulation)}
\keyword{Plackett copula}

