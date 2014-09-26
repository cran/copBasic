\name{simCOPmicro}
\alias{simCOPmicro}
\title{ Simulate V from U through a Copula by Numerical Derivative Method }
\description{
Perform simulation of uniform random but coupled variables \eqn{V} from \eqn{U} through a copula (Nelson, 2006, p. 32) by numerical derivatives of the copula. A more elaborate implementation is available in \code{\link{simCOP}}. The \code{simCOPmicro} function is intended to be an minimalist version for copula simulation---a minimalist version is useful for pedagogic purposes.
}
\usage{
simCOPmicro(u, cop=NULL, para=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters, if needed, to pass to the copula; and}
  \item{...}{Additional arguments to pass should they be needed.}
}
\value{
  Simulated value(s) of nonexceedance probability \eqn{v} are returned based on the nonexceedance probabilities \eqn{u} in argument \code{u}.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{simCOP}}}
\examples{
simCOPmicro(runif(1), cop=W)   # Frechet lower bound copula
simCOPmicro(runif(1), cop=P)   # Independence copula
simCOPmicro(runif(1), cop=M)   # Frechet upper bound copula
simCOPmicro(runif(1), cop=PSP) # The PSP copula

# Now let us get more complicated and mix two Plackett copulas together using the
# composite2COP as a "compositor." The parameter argument becomes more complex, but is
# passed as shown into composite2COP.
para <- list(cop1=PLACKETTcop,cop2=PLACKETTcop, alpha=0.3,beta=0.5, para1=0.1,para2=50)
simCOPmicro(runif(5), cop=composite2COP, para=para)
}
\keyword{copula (simulation)}
