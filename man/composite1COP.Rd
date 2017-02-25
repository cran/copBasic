\name{composite1COP}
\alias{composite1COP}
\title{ Composition of a Single Symmetric Copula with Two Compositing Parameters }
\description{
The \emph{composition of a single copula} (Salvadori \emph{et al.}, 2006, p. 266, prop. C.3) is created by the following result related to \dQuote{composition of copulas} in that reference. Suppose \eqn{\mathbf{C}(u,v)} is a \emph{symmetric copula} (see \code{\link{COP}}) with parameters \eqn{\Theta} and \eqn{\mathbf{C} \ne \mathbf{\Pi}} (for \eqn{\mathbf{\Pi}} see \code{\link{P}}), then a family of generally \emph{asymmetric copulas} \eqn{\mathbf{C}_{\alpha,\beta; \Theta}} with \bold{two} \emph{compositing parameters} \eqn{0 < \alpha,\beta < 1, \mbox{and\ } \alpha \ne \beta}, which also includes just the copula \eqn{\mathbf{C}(u,v)} as a limiting case for \eqn{\alpha = \beta = 0} and is given by

\deqn{\mathbf{C}_{\alpha,\beta}(u,v) = u^\alpha v^\beta \cdot \mathbf{C}(u^{1-\alpha},v^{1-\beta})\mbox{.}}

The \code{\link{composite2COP}} function is based on a slighty more general result (see \code{\link{composite2COP}} for further details of copula composition).
}
\usage{
composite1COP(u,v, para, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A special parameter \code{list} (see \bold{Note}); and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the composited copula are returned.
}
\note{
The following descriptions list in detail the structure and content of the \code{para} argument:
\describe{
  \item{\code{alpha}}{--- The \eqn{\alpha} compositing parameter;}
  \item{\code{beta}}{--- The \eqn{\beta} compositing parameter;}
  \item{\code{cop1}}{--- Function of the copula \eqn{\mathbf{C}(u,v)}; and}
  \item{\code{para1}}{--- Vector of parameters \eqn{\Theta_\mathbf{C}} for \eqn{\mathbf{C}(u,v)}.}
}
For the \code{para} argument of this function, the same nomenclature as used for \code{\link{composite2COP}} is used with obviously \code{cop2} and \code{para2} dropped for \code{composite1COP}. The \code{cop1} and \code{para1} names remain enumerated for \code{composite1COP} so that the \code{para} argument of the more general \code{\link{composite2COP}} function could be used directly in \code{\link{composite1COP}}. Albeit, the second copula and its parameters would not be used. A more complex (extended) composition in \code{\link{composite3COP}} extends this basic parameter structure.
}
\references{
Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{composite2COP}}, \code{\link{composite3COP}}}
\examples{
alpha <- 0.24; beta <- 0.23; Theta1 <- NA;
# W() does not use a parameter, but show how a parameter would be set if needed.
para  <- list(alpha=alpha, beta=beta, cop1=W, para1=Theta1)
t <- composite1COP(0.4,0.6,para)
if( t != W(0.4,0.6)) message("Not equal as expected")
}
\keyword{copula composition}
\keyword{copula composition (two compositing parameters)}

