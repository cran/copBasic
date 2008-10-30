\name{blomCOP}
\alias{blomCOP}
\title{ The Blomqvist's Beta of a Copula }
\description{
Compute the Blomqvist's Beta \eqn{\beta_\mathbf{C}} of a copula according to Nelson (2006) by

\deqn{\beta_\mathbf{C} = 4\mathbf{C}\biggl(\frac{1}{2},\frac{1}{2}\biggr) - 1\mbox{.}}

Nelson reports that \dQuote{although, Blomqvist's Beta depends only on the copula only through its value at the center of \eqn{[0,1]\times[0,1]}, it nevertheless often provides an accurate approximation to Spearman's Rho \code{\link{rhoCOP}} and Kendall's Tau \code{\link{tauCOP}}.}
}
\usage{
blomCOP(cop=NULL, para=NULL, ...)
}
\arguments{
  \item{cop}{A copula function,}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula, and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for \eqn{\beta_\mathbf{C}} is returned.
}
\note{
See p.~182 of the reference.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269~p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{rhoCOP}}, \code{\link{tauCOP}}
}
\examples{
blom <- blomCOP(cop=PSP)
print(blom)
}
\keyword{multivariate}

