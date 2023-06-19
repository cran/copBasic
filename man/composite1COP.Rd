\encoding{utf8}
\name{composite1COP}
\alias{composite1COP}
\title{Composition of a Single Symmetric Copula with Two Compositing Parameters}
\description{
The \emph{composition of a single copula} (Salvadori \emph{et al.}, 2006, p. 266, prop. C.3) is created by the following result related to \dQuote{composition of copulas} in that reference. Suppose \eqn{\mathbf{C}(u,v)} is a \emph{symmetric copula} (see \code{\link{COP}}) with parameters \eqn{\Theta} and \eqn{\mathbf{C} \ne \mathbf{\Pi}} (for \eqn{\mathbf{\Pi}} see \code{\link{P}}), then a family of generally \emph{asymmetric copulas} \eqn{\mathbf{C}_{\alpha,\beta; \Theta}} with \bold{two} \emph{compositing parameters} \eqn{0 < \alpha,\beta < 1, \mbox{and\ } \alpha \ne \beta}, which also includes just the copula \eqn{\mathbf{C}(u,v)} as a limiting case for \eqn{\alpha = \beta = 0} and is given by

\deqn{\mathbf{C}_{\alpha,\beta}(u,v) = u^\alpha v^\beta \cdot \mathbf{C}(u^{1-\alpha},v^{1-\beta})\mbox{.}}

The \code{composite1COP} function provides the means for \emph{permutation asymmetry} from a \emph{permutation symmetric} copula as also described by Joe (2017, p. 124) but do so in a more general way through the provision of two and not just one parameter. Joe's description is supported herein if one of the \eqn{\alpha} or \eqn{\beta} is held at zero. Very loosely, the \eqn{\alpha > 0} kicks probability density down towards the lower right corner, whereas \eqn{\beta > 0} kicks density up towards the upper left corner. Finally, the \code{\link{composite2COP}} function is based on a slighty more general result (see \code{\link{composite2COP}} for further details of copula composition).
}
\usage{
composite1COP(u, v, para, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A special parameter \code{list} (see \bold{Note}); and}
  \item{...}{Additional arguments to pass to the copula.}
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
For the \code{para} argument, the same nomenclature as used for \code{\link{composite2COP}} is used with obviously \code{cop2} and \code{para2} dropped for \code{composite1COP}. The \code{cop1} and \code{para1} names remain enumerated for \code{composite1COP} so that the \code{para} argument of the more general \code{\link{composite2COP}} function could be used directly in \code{\link{composite1COP}}. Albeit, the second copula and its parameters would not be used. A more complex (extended) composition in \code{\link{composite3COP}} extends this basic parameter structure.
}
\references{
Joe, H., 2017, Parametric copula families for statistical models (chap. 8) \emph{in} Copulas and dependence models with applications---Contributions in honor of Roger B. Nelsen, \emph{eds.} Flores, U.M., Amo Artero, E., Durante, F., \enc{Sánchez}{Sanchez}, J.F.: Springer, Cham, Switzerland, ISBN 978--3--319--64220--9.

Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{breveCOP}}, \code{\link{composite2COP}}, \code{\link{composite3COP}},
\code{\link{convexCOP}}, \code{\link{glueCOP}}}
\examples{
\dontrun{
alpha <- 0.24; beta <- 0.23; Theta1 <- NA;
# W() does not use a parameter, but show how a parameter would be set if needed.
para  <- list(alpha=alpha, beta=beta, cop1=W, para1=Theta1)
t <- composite1COP(0.4, 0.6, para)
if( t != W(0.4, 0.6)) message("Not equal as expected")

# Next use this as a chance to check logic flow through the various
# "compositing" operators and their as needed dispatch to COP().
my.para <- list(cop1=GHcop, para1=exp(+1.098612) + 1,
                cop2=PLcop, para2=exp(-1.203973),
                alpha=0.5,  beta=0.25,
                kappa=0.1,  gamma=0.1,
                weights=c(0.95, 0.05))
# uses cop1/2, para1/2, only weights
nustarCOP(cop=convexCOP,     para=my.para) # 0.8570434

# uses cop1/2, para1/2, only alpha
nustarCOP(cop=convex2COP,    para=my.para) # 0.2697063

# uses cop1,   para1,   only alpha / beta
nustarCOP(cop=composite1COP, para=my.para) # 0.5103119

# uses cop1/2, para1/2, only alpha / beta
nustarCOP(cop=composite2COP, para=my.para) # 0.0714571

# uses cop1/2, para1/2, only alpha, beta, kappa, gamma
nustarCOP(cop=composite3COP, para=my.para) # 0.0792634 }
}
\keyword{copula composition}
\keyword{copula composition (two compositing parameters)}

