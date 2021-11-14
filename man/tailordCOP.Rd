\encoding{utf8}
\name{tailordCOP}
\alias{tailordCOP}
\title{ The Lower- and Upper-Tail Orders of a Copula }
\description{
Compute the \emph{lower-} and \emph{upper-tail orders} (if they exist), respectively, of a copula \eqn{\mathbf{C}(u,v)} according to Joe (2014, pp. 67--70). The \emph{tail order} is a concept for the strength of dependence in the joint tails of a multivariate distribution. The opposing tails can be compared to assess tail order or \emph{reflection symmetry} (term by Joe (2014) for Nelsen's (2006, p. 36) term \emph{radial symmetry}). Joe (2014) provides extensively analytical details but sufficient for the \pkg{copBasic} package, the tail orders can be numerically explored.

The \emph{lower-tail order} maybe numerically approximated by
\deqn{\kappa^L_\mathbf{C} = \frac{\log[\mathbf{C}(t,t)]}{\log(t)}\mbox{,}}
for some small positive values of \eqn{t}, and similarly the \emph{upper-tail order} maybe numerically approximated by
\deqn{\kappa^U_\mathbf{C} = \frac{\log[\hat\mathbf{C}(t,t)]}{\log(t)}\mbox{,}}
where \eqn{\hat\mathbf{C}(u,v)} is the \emph{survival copula} (\code{\link{surCOP}}). Joe (2014) has potentially(?) conflicting notation in the context of the upper-tail order; the term \dQuote{reflection} is used (p. 67) and \dQuote{lower tail order of the reflected copula is the same as the upper tail order of the original copula} (p. 69), but Joe (2014, p. 67) only uses the joint survival function (\code{\link{surfuncCOP}}) in the definition of \eqn{\kappa^U_\mathbf{C}}.

As a note, the author of this package was not able to get \code{tailordCOP} to function properly for the upper-tail order using the joint survival function as implied on the bottom of Joe (2014, p. 67) and fortunately the fact that \dQuote{reflection} is used in other contexts and used in analytical examples, the \code{tailordCOP} function uses the lower-tail order of the reflection (survival copula). Joe (2014) also defines \emph{tail order parameter} \eqn{\Psi} but that seems to be a result of analytics and not implemented in this package. Lastly, the tail orders are extendable into \eqn{d} dimensions, but only a bivariate (\eqn{d = 2}) is provided in \pkg{copBasic}. The tail orders have various classifications for \eqn{\kappa = \kappa_L = \kappa_U}:
\describe{
\item{\eqn{\bullet}}{\emph{Intermediate tail dependence} for \eqn{1 < \kappa < d} or \eqn{\kappa = 1, \Psi = 0};}
\item{\eqn{\bullet}}{\emph{Strong tail dependence} for \eqn{\kappa = 1} with \eqn{\Psi > 0}; and}
\item{\eqn{\bullet}}{\emph{Tail orthant independence} or \emph{tail quadrant independence} for \eqn{\kappa = d}.}
}
Joe (2014) provides additional properties:
\describe{
\item{\eqn{\bullet}}{\eqn{\kappa_L = \kappa_U = d} for the \eqn{d}-dimensional \emph{independence copula} (\code{\link{P}}; \emph{e.g.} \code{tailordCOP(cop=P)});}
\item{\eqn{\bullet}}{It is not possible for \eqn{\kappa_L < 1} or \eqn{\kappa_U < 1} but each can be \eqn{> 1} for a \eqn{\mathbf{C}(u,v)} having some negative dependence (\emph{e.g.} \code{tailordCOP(cop=PLACKETTcop, para=0.2)}; see \code{\link{PLACKETTcop}}); and}
\item{\eqn{\bullet}}{For the bivariate \emph{\enc{Fréchet}{Frechet}--Hoeffding lower bound copula} (\code{\link{W}}; \emph{countermonotonicity copula}) the \eqn{\kappa_L = \kappa_U} and can be considered \eqn{+\infty}. (A special trap in the \code{tailordCOP} provides consistency on \code{\link{W}} but does not test that the copula is actually that function itself.)}
}
}
\usage{
tailordCOP(cop=NULL, para=NULL, tol=1e-6, plot=FALSE, verbose=FALSE, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{tol}{A tolerance on convergence;}
  \item{plot}{A logical plotting a diagnostic plot of the diagonal derivatives and label the limits;}
  \item{verbose}{Show incremental progress; and}
  \item{...}{Additional arguments to pass to the copula function.}
}
\value{
  An \R list is returned.
  \item{kappaL}{The rounded value of \eqn{\kappa^L_\mathbf{C}};}
  \item{kappaU}{The rounded value of \eqn{\kappa^U_\mathbf{C}};}
  \item{source}{An attribute identifying the computational source: \dQuote{tailordCOP}.}
}
\note{
The algorithm implemented for \code{tailordCOP} is based on halves (or alternatives by the setting of the \code{divisor} argument) and uses the copula function (not an analytical or even numerical derivative of the diagonal, \eqn{\delta_\mathbf{C}'(t)}). Starting from the median or \eqn{t = 0.5}, each limit is respectively computed by successive halving of the distance towards \eqn{0^{+}} and checking the change in computed value against the tolerance \code{tol} argument. After the change becomes less than the the \code{tol}erance, convergence is assumed. Other tests are made for \code{NaN} to aid in breaking the successive halvings. The rounding for the numerical results for \eqn{\kappa^U_\mathbf{C}} and \eqn{\kappa^L_\mathbf{C}} is an order of magnitude larger than the tolerance.

Users are encouraged to plot the results and further verify whether the convergence makes sense. The plot produced when \code{plot=TRUE} shows the probability \eqn{t} transformed into standard normal variates by the \code{qnorm()} function in \R so that the distal reaches of each tail and thus limit are readily seen. The terminal points of each limit computation are shown by a small dot, and the letter \dQuote{L} and \dQuote{U} also are plotted at the terminal points.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{tailconCOP}}, \code{\link{taildepCOP}}}
\examples{
\dontrun{
# Joe (2014, p. 5) names MTCJ = Mardia-Takahasi-Cook-Johnson copula
"MTCJ" <- function(u,v, para) { (u^(-para) + v^(-para) - 1)^(-1/para) }
# The results that follow match those reported by Joe (2014, p. 69) who
# analytically derives KappaL = 1 and KappaU = 2.
# TAIL ORDER:
tailordCOP(cop=MTCJ, para=3, plot=TRUE) # kappaL  = 1.00667, kappaU  = 1.96296
# TAIL DEPENDENCY:
taildepCOP(cop=MTCJ, para=3, plot=TRUE) # lambdaL = 0,       lambdaU = 0.7937
# Joe (2014) reports lambdaL = 2^(-1/para) = 2^(-1/3) = 0.7937005}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{copula (tail characteristics)}
\keyword{copula (tail properties)}
\keyword{Joe (2014) Examples and Exercises}

