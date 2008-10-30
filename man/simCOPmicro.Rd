\name{simCOPmicro}
\alias{simCOPmicro}
\title{ Simulate a V from an U through a Copula by Numerical Derivative Method }
\description{
Perform a single simulation of \eqn{V} from \eqn{U} of a copula by numerical derivatives of the copula.
}
\usage{
simCOPmicro(u, cop=NULL, para=NULL, ...)
}
\arguments{
  \item{u}{A nonexceedance probability in the horizontal or \eqn{u} direction;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters, if needed, to pass to the copula; and}
  \item{...}{Additional arguments to pass should they be needed.}
}
\value{
  A simulated value of nonexceedance probability in the vertical or \eqn{v} direction is returned.
}
\note{
The function is a micro version of \code{simCOP}.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269~p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{simCOP}}}
\examples{
simCOPmicro(runif(1), cop=W)   # Frechet lower bound
simCOPmicro(runif(1), cop=P)   # Independence copula
simCOPmicro(runif(1), cop=M)   # Frechet upper bound
simCOPmicro(runif(1), cop=PSP) # The PSP copula

# Now let us get more complicated and mix two Plackett copulas
# together using the composite2COP as a "compositor." The 
# parameter argument becomes more complex, but is passed as
# shown into composite2COP.
para <- list(cop1=PLACKETTcop, cop2=PLACKETTcop,
             alpha=0.3, beta=0.5, para1=0.1, para2=50)
simCOPmicro(runif(1), cop=composite2COP, para=para)
}
\keyword{distribution}
\keyword{multivariate}
