\name{simCOP}
\alias{simCOP}
\title{ Simulate a Copula by Numerical Derivative Method }
\description{
Perform a simulation and visualization of a copula by numerical derivatives of the copula (Nelson, 2006, p. 32). The \code{simCOP} function is based on a uniformly simulating nonexceedance probability \eqn{u} and then conditioning the \eqn{v} from the inverse of the sectional derivative for \eqn{V} with respect to \eqn{U} (see \code{\link{derCOPinv}}). The function for speed will only report a warning if at least one of the requested simulations in \code{n} could not be made because of \code{uniroot}'ing problems in \code{\link{derCOPinv}}. The returned \code{data.frame} will be shortened automatically but can be controlled by \code{na.rm}. Failure of a simulation is purely dependent on the derivative inversion, which should be quite robust for continuous to near continuous copulas and even copulas with singularities should be more or less ok. Lastly, the logical combination \code{na.rm=FALSE} and \code{keept=TRUE} could be used to isolate those combinations giving \code{\link{derCOPinv}} problems.
}
\usage{
simCOP(n=100, cop=NULL, para=NULL, na.rm=TRUE, keept=FALSE,
              ploton=TRUE, points=TRUE, ...)
}
\arguments{
  \item{n}{A sample size, default is \eqn{n = 100};}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters, if needed, to pass to the copula;}
  \item{na.rm}{A logical to toggle the removal of \code{NA} entries on the returned \code{data.frame};}
  \item{keept}{Keep the \eqn{t} uniform random variable for the simulation as the last column in the returned \code{data.frame};}
  \item{ploton}{A logical to toggle on the plot;}
  \item{points}{A logical to actually draw the simulations by \code{points()}; and}
  \item{...}{Additional arguments to pass to the \code{points()} function.}
}
\value{
  An \R \code{data.frame} of the simulated values is returned.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{derCOPinv}}}
\examples{
# The simCOP function is oft used in other Examples sections through this package.
simCOP(n=10, cop=W)            # Frechet lower bound copula
simCOP(n=10, cop=P)            # Independence copula
simCOP(n=10, cop=M, col=2)     # Frechet upper bound copula
simCOP(n=10, cop=PSP)          # The PSP copula
\dontrun{
# Now simulate the PSP copula, add the level curves of the copula, and demonstrate
# the uniform distribution of marginals on the correct axes (U [top] and V [left]).
D <- simCOP(n=400, cop=PSP) # store simulated values in D
level.curvesCOP(cop=PSP, ploton=FALSE)
rug(D$U, side=3, col=2); rug(D$V, side=4, col=2)

# Now let us get more complicated and mix two Plackett copulas together using the
# composite2COP as a "compositor." The parameter argument becomes more complex, but
# is passed as shown into composite2COP.
para <- list(cop1=PLACKETTcop,cop2=PLACKETTcop, alpha=0.3,beta=0.5, para1=0.1,para2=50)
D <- simCOP(n=450, cop=composite2COP, para=para, col=rgb(0,0,0,0.1), pch=16)
}
}
\keyword{copula (simulation)}
\keyword{visualization}
