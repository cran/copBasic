\name{simCOP}
\alias{simCOP}
\title{ Simulate a Copula by Numerical Derivative Method }
\description{
Perform a simulation and visualization of a copula by numerical derivatives of the copula.
}
\usage{
simCOP(n=100, cop=NULL, para=NULL, ploton=TRUE, points=TRUE, ...)
}
\arguments{
  \item{n}{A sample size, default is 100;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters, if needed, to pass to the copula;}
  \item{ploton}{A logical to toggle on the plot;}
  \item{points}{A logical to actually draw the simulations by \code{points()}; and}
  \item{...}{Additional arguments to pass to the \code{points()} function.}
}
\value{
  A data frame of the simulated values is returned.
}
\note{
See p. 32 of the reference.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{derCOPinv}}}
\examples{
simCOP(cop=W)                  # Frechet lower bound
simCOP(n=10, cop=P)            # Independence copula
simCOP(cop=M, col=2)           # Frechet upper bound
simCOP(cop=PSP)                # The PSP copula
simCOP(cop=P, cex=0.5, pch=16) # Independence copula

# Now simulate the PSP copula, add the level curves of the
# copula, and demonstrated the uniform distribution of the
# marginals on the correct axes (top for U and bottom for V).
D <- simCOP(n=400, cop=PSP) # store simulated values in D
level.curvesCOP(cop=PSP, ploton=FALSE)
rug(D$U, side=3, col=2); rug(D$V, side=4, col=2)


# Now let us get more complicated and mix two Plackett copulas
# together using the composite2COP as a "compositor." The
# parameter argument becomes more complex, but is passed as
# shown into composite2COP.
para <- list(cop1=PLACKETTcop, cop2=PLACKETTcop,
             alpha=0.3, beta=0.5, para1=0.1, para2=50)
D <- simCOP(n=450, cop=composite2COP,
            para=para, col=rgb(0,0,0,0.1), pch=16)
}
\keyword{distribution}
\keyword{multivariate}
