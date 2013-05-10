\name{PLACKETTpar}
\alias{PLACKETTpar}
\title{ Estimate the Parameter of the Plackett Copula }
\description{
The parameter \eqn{\Theta} of the Plackett Copula is related to Spearman's Rho (\eqn{\rho_S \ne 1})
\deqn{\rho_S(\Theta) = \frac{\Theta + 1}{\Theta - 1} - \frac{2\Theta\log(\Theta)}{(\Theta - 1)^2}}
and a median split estimator best shown in code. First, compute the two medians \code{medx <- median(x)} and \code{medy <- median(y)}. Second, compute the number of occurrences where both values are less than their medians \code{k <- length(x[x < medx & y < medy])}. Third, express this as a probability \code{m <- k/length(x)}, and finally compute the \eqn{\Theta} by
\deqn{\Theta = \frac{4m^2}{(1-2m)^2}}
}
\usage{
PLACKETTpar(x, y, rho=NULL, byrho=FALSE, ...)
}
\arguments{
  \item{x}{Vector of values for variable X,}
  \item{y}{Vector of values for variable Y,}
  \item{rho}{Spearman's Rho and \code{byrho} is set to \code{TRUE} automatically,}
  \item{byrho}{Should Spearman's Rho be used instead of median split, and}
  \item{...}{Additional arguments to pass.}
}
\value{
  A value for \eqn{\Theta} is returned.
}
\note{
See p. 92 of Nelson (2006) and p. 247 of Salvadori and others (2007). The input values \code{x} and \code{y} are not used if Spearman's Rho is provided by \code{rho}.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in nature---An approach using copulas: Dordrecht, Netherlands, Springer, Water Science and Technology Library 56, 292 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{PLACKETTcop}} and \code{\link{PLACKETTsim}}
}
\examples{
Q1 <- rnorm(1000)
Q2 <- Q1+rnorm(1000)
PLACKETTpar(Q1,Q2)
PLACKETTpar(Q1,Q2,byrho=TRUE)

PLACKETTpar(rho=0.76)
PLACKETTpar(rho=-0.76)


}
\keyword{distribution}
\keyword{multivariate}
