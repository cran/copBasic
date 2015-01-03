\name{simCOP}
\alias{simCOP}
\title{ Simulate a Copula by Numerical Derivative Method }
\description{
Perform a simulation and visualization of a copula by numerical derivatives of the copula (Nelsen, 2006, p. 32). The \code{simCOP} function is based on a uniformly simulating nonexceedance probability \eqn{u} and then conditioning the \eqn{v} from the inverse of the sectional derivative for \eqn{V} with respect to \eqn{U} (see \code{\link{derCOPinv}}). The function for speed will only report a warning if at least one of the requested simulations in \code{n} could not be made because of \code{uniroot}'ing problems in \code{\link{derCOPinv}}. The returned \code{data.frame} will be shortened automatically but can be controlled by \code{na.rm}. Failure of a simulation is purely dependent on the derivative inversion, which should be quite robust for continuous to near continuous copulas and even copulas with singularities should be more or less ok. Lastly, the logical combination \code{na.rm=FALSE} and \code{keept=TRUE} could be used to isolate those combinations giving \code{\link{derCOPinv}} problems. The simulation method herein is known as the \emph{conditional approach} or \emph{Rosenblatt transform} within this copula literature (Joe, 2015, p. 270).
}
\usage{
simCOP(n=100, cop=NULL, para=NULL, na.rm=TRUE, keept=FALSE,
              graphics=TRUE, ploton=TRUE, points=TRUE, snv=FALSE,
              infsnv.rm=TRUE, trapinfsnv=.Machine$double.eps, ...)
}
\arguments{
  \item{n}{A sample size, default is \eqn{n = 100};}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters, if needed, to pass to the copula;}
  \item{na.rm}{A logical to toggle the removal of \code{NA} entries on the returned \code{data.frame};}
  \item{keept}{Keep the \eqn{t} uniform random variable for the simulation as the last column in the returned \code{data.frame};}
  \item{graphics}{A logical that will disable graphics by setting \code{ploton} and \code{points} to \code{FALSE} and overriding whatever their settings were;}
  \item{ploton}{A logical to toggle on the plot (see Examples in \code{\link{vuongCOP}});}
  \item{points}{A logical to actually draw the simulations by \code{points()};}
  \item{snv}{A logical to convert the \eqn{\{u,v\}} to standard normal scores (variates) both for the optional graphics and the returned \code{data.frame} (Curiously, Joe (2015) advocates extensively for use of normal scores, which contrasts to Nelsen (2006) who does not.);}
   \item{infsnv.rm}{A logical that will quitely strip out any occurrences of \eqn{u = 0,1} or \eqn{v = 0,1} from the simulations as these are infinity in magnitude when converted to standard normal variates is to occur. Thus, this logical only impacts logic flow when \code{snv} is \code{TRUE}. The \code{infsnv.rm} is mutually exclusive from \code{trapinfsnv}; }
   \item{trapinfsnv}{If true and presumably small, the numerical value of this argument is used to replace \eqn{u = 0} and \eqn{v = 0} with this value when conversion to standard normal variates is to occur. The setting of \code{trapinfsnv} only is used if \code{snv} is \code{TRUE} and \code{infsnv.rm} is \code{FALSE}; and}
  \item{...}{Additional arguments to pass to the \code{points()} function.}
}
\value{
  An \R \code{data.frame} of the simulated values is returned.
}
\references{
Joe, H., 2015, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
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
D <- simCOP(n=950, cop=composite2COP, para=para, col=rgb(0,0,0,0.2), pch=16, snv=TRUE)
}
}
\keyword{copula (simulation)}
\keyword{visualization}
\concept{conditional approach}
\concept{Rosenblatt transform}
