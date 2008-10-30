\name{composite1COP}
\alias{composite1COP}
\title{ Composition of an Asymmetric Copula from a Single Symmetric Copula }
\description{
An asymmetric copula for a single copula is created by the following result related to \dQuote{composition of copulas} in the reference. Let \eqn{\mathbf{C}(u,v)} by a symmetric copula, \eqn{\mathbf{C} \ne \Pi} (for \eqn{\Pi} see \code{\link{P}}). A family of asymmetric copulas \eqn{\mathbf{C}_{\alpha,\beta}}, with parameters \eqn{0 < \alpha,\beta < 1, \alpha \ne \beta}, that includes \eqn{\mathbf{C}(u,v)} as a limiting case, is given by

\deqn{\mathbf{C}_{\alpha,\beta}(u,v) = u^\alpha v^\beta \cdot \mathbf{C}(u^{1-\alpha},v^{1-\beta})\mbox{.}}

The \code{\link{composite2COP}} function is based on a more general result (see \code{\link{composite2COP}} for full details of compositing copulas.
}
\usage{
composite1COP(u,v,para,...)
}
\arguments{
  \item{u}{A nonexceedance probability in X direction,}
  \item{v}{A nonexceedance probability in Y direction,}
  \item{para}{A special parameter \code{list()} (see Note), and}
  \item{...}{Additional arguments to pass.}
}
\value{
  A value for the composited copula is returned.
}
\note{
See p.~266 of the reference and Proposition~C.3 for complete discussion.

The following descriptions list in detail the structure and content of the \code{para} argument:
\describe{
  \item{\samp{alpha}}{The \eqn{\alpha} parameter,}
  \item{\samp{beta}}{The \eqn{\beta} parameter,}
  \item{\samp{cop1}}{Function of the first copula, and}
  \item{\samp{para1}}{Vector of parameters for the first copula.}
}
For the \samp{para} argument of this function the same nomenclature as used for \code{\link{composite2COP}} is used, but \samp{cop2} and \samp{para2} have been dropped. The \samp{cop1} and \samp{para1} names rename enumerated so that the \code{para} argument of the more general \code{\link{composite2COP}} function could be used directly in \code{\link{composite1COP}}. Albeit, the second copula and its parameters would of course not be used.
}
\references{
Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007,
Extremes in Nature---An approach using copulas: Springer, 289~p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{composite2COP}}}
\examples{
alpha <- 0.24; beta <- 0.23
Theta1 <- NA;

# The W() and PSP() copulas do not take parameters, but show how
# the parameters (ThetaX) would be set should either or both of the
# copulas require parameters.
para   <- list(alpha=alpha,      beta=beta,
                cop1=W,          para1=c(Theta1))
t <- composite1COP(0.4,0.6,para)
print(t)
}
\keyword{distribution}
\keyword{multivariate}
