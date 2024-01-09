\encoding{utf8}
\name{simCOP}
\alias{simCOP}
\alias{rCOP}
\title{Simulate a Copula by Numerical Derivative Method}
\description{
Perform a simulation and visualization of a copula using numerical partial derivatives of the copula (Nelsen, 2006, p. 32). The method is more broadly known as \emph{conditional simulation method}. Because a focus of \pkg{copBasic} is on copula theory for pedagogic purposes, the coupling between simulation and subsequent visualization is emphasized by this function by it providing for both simulation and plotting operations by default.

The \code{simCOP} function is based on a uniformly simulating nonexceedance probability \eqn{u} and then conditioning the \eqn{v} from the inverse of the sectional derivative for \eqn{V} with respect to \eqn{U} (see \code{\link{derCOPinv}}). The function for speed will only report a warning if at least one of the requested simulations in \code{n} could not be made because of \code{uniroot}'ing problems in \code{\link{derCOPinv}}. The returned \code{data.frame} will be shortened automatically, but this can be controlled by \code{na.rm}. Failure of a simulation is purely dependent failure of the derivative inversion. In general, inversion should be quite robust for continuous or near continuous copulas and even copulas with singularities should be more or less okay. Lastly, the logical combination \code{na.rm=FALSE} and \code{keept=TRUE} could be used to isolate those combinations giving \code{\link{derCOPinv}} problems. The implemented simulation method in the \pkg{copBasic} package is known as the \emph{conditional distribution method} (Nelsen, 2006; pp. 40--41), \emph{conditional method}, or \emph{Rosenblatt transform} (Joe, 2014, p. 270).
}
\usage{
simCOP(n=100, cop=NULL, para=NULL, na.rm=TRUE, seed=NULL, keept=FALSE,
              graphics=TRUE, ploton=TRUE, points=TRUE, snv=FALSE,
              infsnv.rm=TRUE, trapinfsnv=.Machine$double.eps,
              resamv01=FALSE, showresamv01=FALSE, ...)
rCOP(n, cop=NULL, para=NULL, na.rm=TRUE, seed=NULL,
              resamv01=FALSE, showresamv01=FALSE, ...)
}
\arguments{
  \item{n}{A sample size, default is \eqn{n = 100};}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters, if needed, to pass to the copula;}
  \item{na.rm}{A logical to toggle the removal of \code{NA} entries should they form on the returned \code{data.frame}. A well implemented copula should accommodate and not return \code{NA} but because this package relies on numerical derivation, it was decided to have a mechanism to handle this;}
    \item{seed}{The integer seed to pass immediately to \code{set.seed()};}
  \item{keept}{Keep the \eqn{t} uniform random variable for the simulation as the last column in the returned \code{data.frame};}
  \item{graphics}{A logical that will disable graphics by setting \code{ploton} and \code{points} to \code{FALSE} and overriding whatever their settings were;}
  \item{ploton}{A logical to toggle on the plot (see \bold{Examples} in \code{\link{vuongCOP}});}
  \item{points}{A logical to actually draw the simulations by the \code{points()} function in \R;}
  \item{snv}{A logical to convert the \eqn{\{u,v\}} to standard normal scores (variates) both for the optional graphics and the returned \R \code{data.frame}. Curiously, Joe (2014) advocates extensively for use of normal scores, which is in contrast to Nelsen (2006) who does not;}
   \item{infsnv.rm}{A logical that will quietly strip out any occurrences of \eqn{u = \{0,1\}} or \eqn{v = \{0,1\}} from the simulations because these are infinity in magnitude when converted to standard normal variates has been selected. Thus, this logical only impacts logic flow when \code{snv} is \code{TRUE}. The \code{infsnv.rm} is mutually exclusive from \code{trapinfsnv};}
   \item{trapinfsnv}{If \code{TRUE} and presumably small, the numerical value of this argument (\eqn{\eta}) is used to replace \eqn{u = \{0,1\}} and \eqn{v = \{0,1\}} with \eqn{u(0) =  v(0) = \eta} or \eqn{u(1) = v(1) = 1 - \eta} as appropriate when conversion to standard normal variates has been selected. The setting of \code{trapinfsnv} only is used if \code{snv} is \code{TRUE} and \code{infsnv.rm} is \code{FALSE};}
  \item{resamv01}{A logical triggering resampling for the elements of \eqn{v=0} and \eqn{v=1} (see \bold{Examples}). This is a relatively late addition to \pkg{copBasic} logic and hence is disabled by default. If this is set to true, then the operations related to \code{infsnv.rm} and \code{trapinfsnv} are never to be involved later down in the functions' logic;}
  \item{showresamv01}{A logical providing a trigger to display a \code{message()} within the resampling loops for \eqn{v <= 0, v >= 1}; and}
  \item{...}{Additional arguments to pass to the \code{points()} function in \R.}
}
\value{
  An \R \code{data.frame} of the simulated values is returned.
}
\note{
Function \code{rCOP} is a light-weight implementation for bivariate copula random variates that dispatches to \code{\link{simCOPmicro}} and bypasses the graphical and other features of \code{simCOP}. Finally, an experimental parallel for the empirical copula is \code{\link{EMPIRsim}}. Note, the equivalent of a \code{resamv01} is not implemented at the level of \code{\link{simCOPmicro}} because it is better to do such at the abstraction level of \code{rCOP} and \code{simCOP}.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{derCOPinv}}, \code{\link{simCOPmicro}}}
\examples{
simCOP(n=5, cop=PARETOcop, para=2.4)

# We can find some unusual simulation combinations for which V == 0 or 1 and might have
# downstream operations simply not able to handle infinite quantiles (say) at the edges.
# We can readily offload the issue back to the rCOP() or simCOP() with resamv01 argument.
nsim <- 800; para <- list(cop=PLcop, para=150, alpha=0.8, beta=0.3)
JK <- rCOP(nsim, cop=composite1COP, para=para, seed=1, resamv01=FALSE)
print(JK[JK[,2] == 1,]) # 189 0.9437248 1.   So, row 189 in this example has V=1, and
UV <- rCOP(nsim, cop=composite1COP, para=para, seed=1, resamv01=TRUE ) # changing the
# resamv01 argument to TRUE, we get this message and no V=1 in the returned data frame.
# rCOP() has some v >= 1, resampling those     <---- This is the message output.

\dontrun{
# The simCOP function is oft used in other Examples sections through this package.
simCOP(n=10, cop=W)            # Frechet lower-bound copula
simCOP(n=10, cop=P)            # Independence copula
simCOP(n=10, cop=M, col=2)     # Frechet upper-bound copula
simCOP(n=10, cop=PSP)          # The PSP copula}

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
D <- simCOP(n=950, cop=composite2COP, para=para, col=grey(0, 0.2), pch=16, snv=TRUE) #}
}
\keyword{copula (simulation)}
\keyword{visualization}
\concept{conditional approach}
\concept{Rosenblatt transform}
