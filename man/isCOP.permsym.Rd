\encoding{utf8}
\name{isCOP.permsym}
\alias{isCOP.permsym}
\title{Is a Copula Permutation Symmetric}
\description{
Numerically set a logical whether a copula is \emph{symmetric} (Nelsen, 2006, p. 38), or has \emph{exchangable} variables, or is \emph{permutation symmetric} (Joe, 2014, p. 66). A copula \eqn{\mathbf{C}(u,v)} is permutation symmetric if and only if for any \eqn{\{u,v\} \in [0,1]} the following holds
 \deqn{\mathbf{C}(u,v) = \mathbf{C}(v, u)\mbox{.}}
The computation is (can be) CPU intensive.
}
\usage{
isCOP.permsym(cop=NULL, para=NULL, delta=0.005, tol=1e-4, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters, if needed, to pass to the copula;}
  \item{delta}{The increment of  \eqn{\{u,v\} \mapsto [0+\Delta\delta, 1-\Delta\delta, \Delta\delta]};}
  \item{tol}{A tolerance on the check for symmetry, default 1 part in 10,000, which is the test for the \eqn{\equiv 0} (zero equivalence, see source code); and}
  \item{...}{Additional arguments to pass to the copula.}
}
\value{
  A logical \code{TRUE} or \code{FALSE} is returned.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{isCOP.radsym}}}
\examples{
\dontrun{
isCOP.permsym(cop=GHcop, para=1.3) # TRUE}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{copula (symmetry)}
