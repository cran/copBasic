\encoding{utf8}
\name{composite2COP}
\alias{composite2COP}
\title{ Composition of Two Copulas with Two Compositing Parameters }
\description{
The \emph{composition of two copulas} (Salvadori \emph{et al.}, 2007, p. 266, prop. C.3) provides for more sophisticated structures of dependence between variables than many single parameter copula can provide. Further, \emph{asymmetrical copulas} are readily obtained from \emph{symmetrical copulas}. Let \eqn{\mathbf{A}} and \eqn{\mathbf{B}} be copulas with respective parameters \eqn{\Theta_\mathbf{A}} and \eqn{\Theta_\mathbf{B}}, then

\deqn{\mathbf{C}_{\alpha,\beta}(u,v) = \mathbf{A}(u^\alpha,    v^\beta) \cdot
                              \mathbf{B}(u^{1-\alpha},v^{1-\beta})\mbox{,}}

defines a family of copulas \eqn{\mathbf{C}_{\alpha,\beta; \Theta_\mathbf{A}, \Theta_\mathbf{B}}} with \bold{two} \emph{compositing parameters} \eqn{\alpha,\beta \in \mathcal{I}:[0,1]}. In particular if \eqn{\alpha = \beta = 1}, then \eqn{\mathbf{C}_{1,1} = \mathbf{A}}, and if \eqn{\alpha = \beta = 0}, then \eqn{\mathbf{C}_{0,0} = \mathbf{B}}. For \eqn{\alpha \ne \beta}, the \eqn{\mathbf{C}_{\alpha,\beta}} is in general asymmetric that is \eqn{\mathbf{C}(u,v) \ne \mathbf{C}(v,u)} for some \eqn{(u,v) \in \mathcal{I}^2}.

It is important to stress that copulas \eqn{\mathbf{A}_{\Theta_A}} and \eqn{\mathbf{B}_{\Theta_B}} can be of different families and each copula parameterized accordingly by the vector of parameters \eqn{\Theta_A} and \eqn{\Theta_B}. This is an interesting feature in the context of building complex copulatic structures in pursuit of fitting asymmetric measures of dependency such as the \emph{L-comoments} available in the \pkg{lmomco} package.  The author (Asquith) does not know whether the copulas \eqn{\mathbf{A}} and \eqn{\mathbf{B}} need be symmetric as the Salvadori reference makes no stated restriction to that effect. (Symmetry of the copula \eqn{\mathbf{C}} is required for the situation that follows, however.)

It is possible to simplify the construction of an asymmetric copula from a symmetric copula by the following. Let \eqn{\mathbf{C}(u,v)} be a symmetric copula, \eqn{\mathbf{C} \ne \mathbf{\Pi}} (for \eqn{\mathbf{\Pi}} see \code{\link{P}}). A family of asymmetric copulas \eqn{\mathbf{C}_{\alpha,\beta}} with \bold{two} \emph{composition parameters} \eqn{0 < \alpha,\beta < 1, \mbox{and\ } \alpha \ne \beta} that also includes \eqn{\mathbf{C}(u,v)} as a limiting case and is given by

\deqn{\mathbf{C}_{\alpha,\beta}(u,v) = u^\alpha v^\beta \cdot \mathbf{C}(u^{1-\alpha},v^{1-\beta})\mbox{.}}

The \code{composite2COP} function is based on the more general result given in the former rather than the later mathematical definition to provide more flexibility. For simpler case of composition involving only one copula, the \code{\link{composite1COP}} function is available.  A more complex (extended) composition in \code{\link{composite3COP}} is available.
}
\usage{
composite2COP(u,v, para, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A special parameter \code{list} (see \bold{Note}); and}
  \item{...}{Additional arguments to pass to the copulas.}
}
\value{
  Value(s) for the composited copula is returned.
}
\note{
The following descriptions list in detail the structure and content of the \code{para} argument:
\describe{
  \item{\code{alpha}}{--- The \eqn{\alpha} compositing parameter;}
  \item{\code{beta}}{--- The \eqn{\beta} compositing parameter;}
  \item{\code{cop1}}{--- Function of the first copula \eqn{\mathbf{A}};}
  \item{\code{cop2}}{--- Function of the second copula  \eqn{\mathbf{B}};}
  \item{\code{para1}}{--- Vector of parameters \eqn{\Theta_\mathbf{A}} for  \eqn{\mathbf{A}}; and}
  \item{\code{para2}}{--- Vector of parameters \eqn{\Theta_\mathbf{B}} for  \eqn{\mathbf{B}}.}
}

The \code{para} argument of this function also can be passed to \code{\link{composite1COP}}. Albeit, the second copula and its parameters would not be used.  A more complex (extended) composition in \code{\link{composite3COP}} extends this basic parameter structure.
}
\references{
Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{composite1COP}}, \code{\link{composite3COP}}}
\examples{
alpha <- 0.24; beta <- 0.23; Theta1 <- NA; Theta2 <- NA
# The W() and PSP() copulas do not take parameters, but example shows how the
# parameters would be set should either or both of the copulas require parameters.
para <- list(alpha=alpha, beta=beta, cop1=W, cop2=PSP, para1=Theta1, para2=Theta2)
print(composite2COP(0.4,0.6,para)) # 0.2779868

# In this example, the N4212cop uses "3" as its parameter value.
para <- list(alpha=alpha, beta=beta, cop1=W, cop2=N4212cop, para1=Theta1, para2=3)
print(composite2COP(0.4,0.6,para)) # 0.3387506

\dontrun{
# This example does a great job of showing a composited copula with a near singularity,
# but with leakage of chance to the upper left. The example is also critical because
# it shows that gridCOP is returning a matrix in the proper orientation relative to
# the level.curvesCOP and simCOP functions. Example is cross-ref'ed from gridCOP() docs.
layout(matrix(1:2,byrow=TRUE))
para <- list(alpha=0.5, beta=0.90, cop1=M, cop2=N4212cop, para1=NA, para2=1.4)
image(gridCOP(cop=composite2COP, para=para, delta=0.01), col=terrain.colors(30),
      xlab="U, NONEXCEEDANCE PROBABILITY", ylab="V, NONEXCEEDANCE PROBABILITY")
D <- simCOP(n=2000, cop=composite2COP, para=para, ploton=FALSE, pch=4, col=4, cex=0.75)
level.curvesCOP(cop=composite2COP, para=para, ploton=FALSE, delt = 0.05)
mtext("Theoretical composited copula, level curves, and simulation")

emp <- EMPIRgrid(para=D, deluv = 0.05)     # CPU heavy
image(emp$empcop, col=terrain.colors(30) ) # image orientation is correct!
# Depending on balance between sample size, deluv, delu, and delt, one or more:
# Error in uniroot(func, interval = c(0, 1), u = u, LHS = t, cop = cop,  :
#   f() values at end points not of opposite sign
# warnings might be triggered. This is particularly true because of the flat derivative
# above the near singularity in this example composited copula.
points(D$U, D$V, pch=4, col=4, cex=0.75)
level.curvesCOP(cop=EMPIRcop, para=D, ploton=FALSE, delu=0.02, delt = 0.05)
mtext("Empirical copula from n=2000 simulation") #}
}
\keyword{copula composition}
\keyword{copula composition (two compositing parameters)}

