\name{isCOP.radsym}
\alias{isCOP.radsym}
\title{ Is a Copula Radially Symmetric }
\description{
Numerically set a logical whether a copula is \emph{radially symmetric} (Nelson, 2006, p. 37). A copula \eqn{\mathbf{C}(u,v)} is radially symmetric if and only if for any \eqn{\{u,v\} \in [0,1]} the following hold
 \deqn{\mathbf{C}(u,v) = u + v - 1 + \mathbf{C}(1-u, 1-v)}
 or
 \deqn{u + v - 1 + \mathbf{C}(1-u, 1-v) - \mathbf{C}(u,v) \equiv 0\mbox{.}}
}
\usage{
isCOP.radsym(cop=NULL, para=NULL, delta=0.005, tol=1e-4, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters, if needed, to pass to the copula;}
  \item{delta}{The increments of  \eqn{\{u,v\} \mapsto [0+\Delta\delta, 1-\Delta\delta, \Delta\delta]};}
  \item{tol}{A tolerance on the check for symmetry, default 1 part in 10,000, which is the test for the \eqn{\equiv 0} (zero equivalence, see source code); and}
  \item{...}{Additional arguments to pass to the copula or derivative of a copula function.}
}
\value{
  A logical \code{TRUE} or \code{FALSE} is returned.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{COP}} }
\examples{
\dontrun{
isCOP.radsym(cop=P) # TRUE
para <- list(cop1=PLACKETTcop, cop2=M, para1=c(.3), para2=NA, alpha=0.8, beta=0.5)
isCOP.radsym(composite2COP, para=para) # FALSE
}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}

