\name{PLACKETTsim}
\alias{PLACKETTsim}
\title{ Direct Simulation of a Plackett Copula }
\description{
Simulation of the Plackett Copula is made by this function.
}
\usage{
PLACKETTsim(n, para=NULL, ...)
}
\arguments{
  \item{n}{Sample size,}
  \item{para}{A vector (single element) of parameters---the \eqn{\Theta} parameter, and}
  \item{...}{Additional arguments to pass.}
}
\value{
  A data frame of the values are returned.
}
\note{
See p. 247 of Salvadori and others (2007).
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in nature---An approach using copulas: Dordrecht, Netherlands, Springer, Water Science and Technology Library 56, 292 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{M}}, \code{\link{W}}, \code{\link{P}}}
\examples{
PLACKETTsim(10,para=c(1)) # simulate P copula through a Plackett
PLACKETTsim(10,para=c(20)) # simulate strong positive Plackett

}
\keyword{distribution}
\keyword{multivariate}
