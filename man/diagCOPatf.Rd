\name{diagCOPatf}
\alias{diagCOPatf}
\title{ Numerical Rooting the Diagonal of a Copula }
\description{
Compute a numerical root along the \emph{primary diagonal} (Nelsen, 2006, pp. 12 and 16) of copula \eqn{\mathbf{C}(u,v)} having joint probability \eqn{F}. The diagonals treat the nonexceedance probabilities \eqn{u} and \eqn{v} as equals as shown below. The primary diagonal is defined as
\deqn{F = \mathbf{C}(t,t) \rightarrow t = \delta^{(-1)}(f)\mbox{,}}
where the function solves for \eqn{t}.
}
\usage{
diagCOPatf(f, cop=NULL, para=NULL, verbose=FALSE, ...)
}
\arguments{
  \item{f}{A single value of joint probability as a nonexceedance probability \eqn{F} for which to compute the root \eqn{t};}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters, if needed, to pass to the copula;}
  \item{verbose}{If \code{TRUE} then the whole output of the numerical root is returned; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  An \R \code{list} of the root by \code{uniroot()} is returned if \code{verbose} is \code{TRUE}, otherwise the root for \eqn{F} is returned instead.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{diagCOP}}}
\examples{
diagCOPatf(0.67, cop=PSP) # 0.8023879
diagCOPatf(0.99, cop=M) # 0.99 (now see the example below)
\dontrun{
# Several functions from the lmomco package are needed.
# Suppose we have two phenomena with these log10 L-moments:
lmrA <- lmomco::vec2lmom(c(3.97, 0.485, -0.1178, 0.06857))
lmrB <- lmomco::vec2lmom(c(3.77, 0.475, -0.1377, 0.08280))
# Suppose we think that the Gumbel-Hougaard copula is appropriate
Tau <- 0.45 #  Kendall's Tau between A and B
# Suppose that the F=0.99 for either A and B provides a common risk level when they
# are considered in isolation. But what if A and B are streams that join and joint
# F=0.99 at their union is of interest?
F <- 0.99
parA <- lmomco::lmom2par(lmrA, type="kap")
parB <- lmomco::lmom2par(lmrB, type="kap")
EventA <- lmomco::qlmomco(F, parA)
EventB <- lmomco::qlmomco(F, parB)
ApB <- 10^(EventA) + 10^(EventB) # Purely an additive conceptualization
# The F=0.99 event is assumed to occur simultaneously on both streams, which is
# equivalent to saying that the correlation between the two is absolute 1-to-1.

# Now consider including the association as measured by Kendall's Tau:
Fjoint <- diagCOPatf(F, cop=GHcop, para=GHcop(tau=Tau)$para)
EventAj <- lmomco::qlmomco(Fjoint, parA)
EventBj <- lmomco::qlmomco(Fjoint, parB)
AcB <- 10^(EventAj) + 10^(EventBj) # Joint probability 0.99 at the union

# Now consider the association if the streams are INDEPENDENT:
Fjoint <- diagCOPatf(F, cop=GHcop, para=GHcop(tau=0)$para)
EventAj <- lmomco::qlmomco(Fjoint, parA)
EventBj <- lmomco::qlmomco(Fjoint, parB)
AiB <- 10^(EventAj) + 10^(EventBj) # Joint probability 0.99 at the union

# ApB = 312,000 # The perfectly simultaneous addition makes too little.
# AcB = 323,000 # The copula preserves at least the known association.
# AiB = 330,000 # The independence conceptualization makes too much.
}
}
\keyword{visualization}
\keyword{diagnostics}
\keyword{copula (characteristics)}
