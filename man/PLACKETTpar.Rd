\name{PLACKETTpar}
\alias{PLACKETTpar}
\title{ Estimate the Parameter of the Plackett Copula }
\description{
The parameter \eqn{\Theta} of the \emph{Plackett copula} (Nelson, 2006, pp. 89--92) is related to \emph{Spearman's Rho} (\eqn{\rho_S \ne 1}, see \code{\link{rhoCOP}})
\deqn{\rho_S(\Theta) = \frac{\Theta + 1}{\Theta - 1} - \frac{2\Theta\log(\Theta)}{(\Theta - 1)^2}}
and a \emph{median-split estimator} best shown as an algorithm. First, compute the two medians: \cr \code{medx <- median(x)} and \code{medy <- median(y)}. Second, compute the number of occurrences where both values are less than their medians \code{k <- length(x[x < medx & y < medy])}. Third, express this as a probability \code{m <- k/length(x)}, and finally the median-split estimator of  \eqn{\Theta} computed by
\deqn{\Theta = \frac{4m^2}{(1-2m)^2}\mbox{.}}
Nelson (2006, p. 92) and Salvadori \emph{et al.} (2007, p. 247) provide further details. The input values \code{x} and \code{y} are not used if Spearman's Rho is provided by \code{rho}.
}
\usage{
PLACKETTpar(x, y, rho=NULL, byrho=FALSE, ...)
}
\arguments{
  \item{x}{Vector of values for random variable \eqn{X};}
  \item{y}{Vector of values for random variable \eqn{Y};}
  \item{rho}{Spearman's Rho and \code{byrho} is set to \code{TRUE} automatically;}
  \item{byrho}{Should Spearman's Rho be used instead of the median-split estimator; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  A value for the Plackett copula \eqn{\Theta} is returned.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{PLACKETTcop}}, \code{\link{PLACKETTsim}}, \code{\link{rhoCOP}}
}
\examples{
Q1 <- rnorm(1000); Q2 <- Q1+rnorm(1000)
PLACKETTpar(Q1,Q2)
PLACKETTpar(Q1,Q2, byrho=TRUE)
PLACKETTpar(rho= 0.76)
PLACKETTpar(rho=-0.76)
}
\keyword{Plackett copula}
\keyword{copula (estimation)}

