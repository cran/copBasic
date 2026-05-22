\encoding{utf8}
\name{sectionCOP}
\alias{sectionCOP}
\title{The Sections or Derivative of the Sections of a Copula}
\description{
Compute the \emph{copula sections} or the \emph{(partial) derivatives of copula sections} of a copula (Nelsen, 2006, pp. 12--14). The \emph{horizontal section} at \eqn{V=a} (a constant) is
\deqn{t \mapsto \mathbf{C}(t,a)\mbox{, and}}
the \emph{vertical  section} at \eqn{U=a} (a constant, with respect to \eqn{V} or \code{wrtV=TRUE}) is
\deqn{t \mapsto \mathbf{C}(a,t)\mbox{.}}
The partial derivatives of the copula sections are \emph{conditional cumulative distribution functions} (see \code{\link{derCOP}} and \code{\link{derCOP2}}). The derivatives are constrained as
\deqn{0 \le \frac{\delta}{\delta u}\mathbf{C}(u,v) \le 1\mbox{, and}}
\deqn{0 \le \frac{\delta}{\delta v}\mathbf{C}(u,v) \le 1\mbox{.}}
}
\usage{
sectionCOP(f, cop=NULL,  para=NULL, wrtV=FALSE, dercop=FALSE, delt=0.005,
              ploton=TRUE, lines=TRUE, xlab="NONEXCEEDANCE PROBABILITY", ...)
}
\arguments{
  \item{f}{A single value of nonexceedance probability \eqn{u} or \eqn{v} along the horizontal \eqn{U} axis or vertical \eqn{V} axis of the unit square \eqn{\mathcal{I}^2};}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters, if needed, to pass to the copula;}
  \item{wrtV}{A logical to toggle between with respect to \eqn{v} or \eqn{u} (default). The default provides the vertical section whereas the horizontal comes from \code{wrtV = TRUE};}
  \item{dercop}{A logical that triggers the derivative of the section;}
  \item{delt}{The increment of the level curves to plot, defaults to 5-percent intervals;}
  \item{ploton}{A logical to toggle on the plot;}
  \item{lines}{Draw the lines of diagonal to the current device;}
  \item{xlab}{A label for the x-axis title passed to \code{plot()} in \R; and}
  \item{...}{Additional arguments to pass to the \code{plot()} and \code{lines()} functions in \R.}
}
\value{
  An \R \code{list} is returned.
  \item{t}{The nonexceedance probability along the section. The nomenclature \eqn{t} mimics Nelsen (2006) and is \emph{not} the same as the \eqn{u} or \eqn{v};}
  \item{seccop}{The section of the copula or its derivative;}
  \item{wrt}{A text string declaring what the setting for \code{wrtV} was;}
  \item{fvalue}{The provided value of nonexceedance probability; and}
  \item{isderivative}{A logical stating whether the derivative of the section is \code{seccop}.}
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{diagCOP}}}
\examples{
\dontrun{
# EXAMPLE 1, plot the v=0.55 section and then u=0.55 section, which will overlay
# the other because the PSP is a symmetrical copula
tmp <- sectionCOP(0.55, cop=PSP, ylab="COPULA SECTIONS",  lwd=5, col=2)
tmp <- sectionCOP(0.55, cop=PSP, wrtV=TRUE, ploton=FALSE, lwd=2, col=3)
# now add the v=0.85 section and the u=0.85, again overlay each other
tmp <- sectionCOP(0.85, cop=PSP, ploton=FALSE,             lwd=5, col=2, lty=2)
tmp <- sectionCOP(0.85, cop=PSP, wrtV=TRUE, ploton=FALSE,  lwd=2, col=3, lty=2)#}

\dontrun{
# EXAMPLE 2, v=0.35 section and derivative (the conditional distribution) function
tmp <- sectionCOP(0.35, cop=PSP, ylab="COPULA SECTIONS OR DERIV.", lwd=5, col=3)
tmp <- sectionCOP(0.35, cop=PSP, dercop=TRUE, ploton=FALSE,               col=3)
# The thin green line represents the cumulative distribution function conditional
# on u = 0.35 from the derCOP function.  Then see Example 3}

\dontrun{
# EXAMPLE 3 (random selection commented out)
#para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop, alpha=runif(1), beta=runif(1),
#             para1=10^runif(1,min=-4, max=0), para2=10^runif(1,min= 0, max=4))
para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop, alpha=0.7, beta=0.22,
             para1=0.0155, para2=214.4)
txts <- c("Alpha=",    round(para$alpha,    digits=4),
          "; Beta=",   round(para$beta,     digits=4),
          "; Theta1=", round(para$para1[1], digits=5),
          "; Theta2=", round(para$para2[1], digits=2))
layout(matrix(1:2,byrow=TRUE))
D <- simCOP(n=1000, cop=composite2COP, para=para, cex=0.5, col=rgb(0,0,0,0.2), pch=16)
mtext(paste(txts,collapse=""))
#f <- c(runif(1),runif(1))
f <- c(0.2,0.9) # RED is the horizontal section and BLACK is the vertical section
segments(f[1],0,f[1],1, col=2, lwd=2); segments(0,f[2],1,f[2], lwd=2)
ftxt <- c("Sections (thick) and derivatives (thin) at ", f, " nonexceed. prob.")
tmp <- sectionCOP(f[1],cop=composite2COP,para=para, col=2, lwd=4)
tmp <- sectionCOP(f[1],cop=composite2COP,para=para, dercop=TRUE, ploton=FALSE, col=2)
tmp <- sectionCOP(f[2],cop=composite2COP,para=para,wrtV=TRUE,ploton=FALSE,lwd=4)
tmp <- sectionCOP(f[2],cop=composite2COP,para=para,wrtV=TRUE,ploton=FALSE,dercop=TRUE)
mtext(paste(ftxt, collapse=""))
# The thin lines are the CDFs conditional on the respective values of "f". Carefully
# compare the point densities along and near the sections in the top plot to the
# respective shapes of the CDFs in the bottom plot. If the bottom plot were rotated
# 90 degrees clockwise and then reflected top to bottom, the conditional quantile
# function QDF results. Reflection is needed because, by convention, QDFs are monotonic
# increasing to right---functions derCOPinv() and derCOPinv2() provide the CDF inversion.}
}
\keyword{visualization}
\keyword{diagnostics}
\keyword{copula section}

