\encoding{utf8}
\name{PARETOcop}
\alias{PARETOcop}
\alias{PAcop}
\title{ The Pareto Copula }
\description{
The \emph{Pareto copula} (Nelsen, 2006, pp. 33) is
\deqn{\mathbf{C}_{\mathrm{PA}}(u,v; \Theta) = \bigl[(1-u)^{-\Theta}+(1-v)^{-\Theta}\bigr]^{-1/\Theta}\mbox{,}}
where \eqn{\Theta \in [0, \infty)}. As \eqn{\Theta \rightarrow 0^{+}}, the copula limits to the \eqn{\mathbf{\Pi}} copula (\code{\link{P}}) and the \eqn{\mathbf{M}} copula (\code{\link{M}}). The parameterization here has assocation increasing with increasing \eqn{\Theta}, which differs from Nelsen (2006), and also the Pareto copula is formed with right-tail increasing reflection of Nelsen's presentation because it is anticipated that \pkg{copBasic} users are more likely to have right-tail dependency situations (say large maxima [right tail] coupling but not small maxima [left tail] coupling).
}
\usage{
PARETOcop(u, v, para=NULL, ...)
PAcop(u, v, para=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (single element) of parameters---the \eqn{\Theta} parameter of the copula; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned.
}
\note{
The Pareto copula is used in a demonstration of \emph{Kendall Function L-moment ratio diagram} construction (see \code{\link{kfuncCOPlmoms}}).
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{M}}, \code{\link{P}}
}
\examples{
\dontrun{
z <- seq(0.01,0.99, by=0.01) # Both copulas have Kendall Tau = 1/3
plot( z, kfuncCOP(z, cop=PAcop, para=1), lwd=2,
                                xlab="z <= Z", ylab="F_K(z)", type="l")
lines(z, kfuncCOP(z, cop=GHcop, para=1.5), lwd=2, col=2) # red line
# All extreme value copulas have the same Kendall Function [F_K(z)], the
# Gumbel-Hougaard is such a copula and the F_K(z) for the Pareto does not
# plot on top and thus is not an extreme value but shares a "closeness."}
}
\keyword{Pareto copula}
\keyword{copula (formulas)}
\keyword{copula}
