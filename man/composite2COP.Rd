\name{composite2COP}
\alias{composite2COP}
\title{ Composition of an Asymmetric Copula from Two Copulas }
\description{
The construction of an asymmetric copula from composition two copulas provide for more sophisticated structures of dependence between variables. Let \eqn{\mathbf{A}} and \eqn{\mathbf{B}} be copulas. Then

\deqn{\mathbf{C}_{\alpha,\beta}(u,v) = \mathbf{A}(u^\alpha,    v^\beta) \cdot 
                              \mathbf{B}(u^{1-\alpha},v^{1-\beta})\mbox{,}}

defines a family of copulas \eqn{\mathbf{C}_{\alpha,\beta}}, with parameters \eqn{\alpha,\beta \in \mathcal{I}:[0,1]}. In particular, if \eqn{\alpha = \beta = 1}, then \eqn{\mathbf{C}_{1,1} = \mathbf{A}}, and, if \eqn{\alpha = \beta = 0}, then \eqn{\mathbf{C}_{0,0} = \mathbf{B}}. For \eqn{\alpha \ne \beta}, the \eqn{\mathbf{C}_{\alpha,\beta}} is, in general, asymmetric, that is \eqn{\mathbf{C}(u,v) \ne \mathbf{C}(v,u)} for some \eqn{(u,v) \in \mathcal{I}^2}.

It is important to stress that copulas \eqn{\mathbf{A}_{\Theta_A}} and \eqn{\mathbf{B}_{\Theta_B}} can be of different families and each parameterized accordingly by the values \eqn{\Theta_A} and \eqn{\Theta_B}. This is an interesting observation in the context of building complex copulitic structures in pursuit of fitting asymmetric measures of dependency such as the L-comoments available in the \pkg{lmomco} package.  The author does not know whether the copulas \eqn{\mathbf{A}} and \eqn{\mathbf{B}} need be symmetric as the reference makes no stated restriction to that effect. (Symmetry of the copula \eqn{\mathbf{C}} is required for the situation that follows.)

It is possible to simplify the construction of an asymmetric copula for a single copula by the following. Let \eqn{\mathbf{C}(u,v)} by a symmetric copula, \eqn{\mathbf{C} \ne \Pi} (for \eqn{\Pi} see \code{\link{P}}). A family of asymmetric copulas \eqn{\mathbf{C}_{\alpha,\beta}}, with parameters \eqn{0 < \alpha,\beta < 1, \alpha \ne \beta}, that includes \eqn{\mathbf{C}(u,v)} as a limiting case, is given by

\deqn{\mathbf{C}_{\alpha,\beta}(u,v) = u^\alpha v^\beta \cdot \mathbf{C}(u^{1-\alpha},v^{1-\beta})\mbox{.}}

The \code{\link{composite2COP}} function is based on the more general result given in the former to provide maximum flexibility. For simpler case given in the later, the \code{\link{composite1COP}} is available.
}
\usage{
composite2COP(u,v,para,...)
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
See p.~266 of the reference and Proposition~C.2 for complete discussion.

The following descriptions list in detail the structure and content of the \samp{para} argument:
\describe{
  \item{\samp{alpha}}{The \eqn{\alpha} parameter,}
  \item{\samp{beta}}{The \eqn{\beta} parameter,}
  \item{\samp{cop1}}{Function of the first copula,}
  \item{\samp{cop2}}{Function of the second copula,}
  \item{\samp{para1}}{Vector of parameters for the first copula, and}
  \item{\samp{para2}}{Vector of parameters for the second copula.}
}

The \samp{para} argument of this function also can be passed to \code{\link{composite1COP}}. Albeit, the second copula and its parameters would of course not be used.
}
\references{
Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007,
Extremes in Nature---An approach using copulas: Springer, 289~p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{composite1COP}}}
\examples{
alpha <- 0.24; beta <- 0.23
Theta1 <- NA; Theta2 <- NA

# The W() and PSP() copulas do not take parameters, but show how
# the parameters (ThetaX) would be set should either or both of the
# copulas require parameters.
para   <- list(alpha=alpha,      beta=beta,
                cop1=W,          cop2=PSP,
                para1=c(Theta1), para2=c(Theta2))
t <- composite2COP(0.4,0.6,para)
print(t)
}
\keyword{distribution}
\keyword{multivariate}
