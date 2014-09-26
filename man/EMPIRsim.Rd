\name{EMPIRsim}
\alias{EMPIRsim}
\title{ Simulate a Bivariate Empirical Copula  }
\description{
Perform a simulation on a bivariate empirical copula to produce the random variates \eqn{U} and \eqn{V} and return an \R \code{data.frame} of them. This function is an empirical parallel to \code{\link{simCOP}} that is used for parametric copulas. If circumstances require conditional simulation of \eqn{V|U}, then function \code{\link{EMPIRsimv}}, which produces a vector of \eqn{V} from a fixed \eqn{u}, should be used.

For the usual situation in which an individual \eqn{u} during the simulation loops is not a value aligned on the grid, then the bounding conditional quantile functions are solved for each of the \eqn{n} simulations and the following interpolation is made by
\deqn{v = \frac{v_1/w_1 + v_2/w_2}{1/w_1 + 1/w_2}\mbox{,}}
which states that that the weighted mean is computed. The values \eqn{v_1} and \eqn{v_2} are ordinates of the conditional quantile function for the respective grid lines to the left and right of the \eqn{u} value. The values \eqn{w_1} \eqn{=} \eqn{u - u^\mathrm{left}_\mathrm{grid}} and \eqn{w_2} \eqn{=} \eqn{u^\mathrm{right}_\mathrm{grid} - u}.
}
\usage{
EMPIRsim(n=100, empgrid=NULL, kumaraswamy=FALSE, ploton=TRUE, points=TRUE, ...)
}
\arguments{
  \item{n}{A sample size, default is 100;}
  \item{empgrid}{Gridded empirical copula from \code{\link{EMPIRgrid}};}
  \item{kumaraswamy}{A logical to trigger Kumaraswamy smoothing of the conditional quantile function that is passed to \code{\link{EMPIRgridderinv}}.  The Kumaraswamy distribution is a distribution having support \eqn{[0,1]} with an explicit quantile function and takes the place of a Beta distribution (see \pkg{lmomco} function \code{quakur} for more details);}
  \item{ploton}{A logical to toggle on the plot;}
  \item{points}{A logical to actually draw the simulations by the  \code{points()} function in \R; and}
  \item{...}{Additional arguments to pass to the \code{points()} function or to \code{\link{EMPIRgridderinv}}.}
}
\value{
  An \R \code{data.frame} of the simulated values is returned.
}
\author{ W.H. Asquith}
\seealso{\code{\link{EMPIRgrid}}, \code{\link{EMPIRgridderinv}},  \code{\link{EMPIRsimv}} }
\examples{
\dontrun{
pdf("EMPIRsim_experiment.pdf")
nsim <- 5000
para <- list(alpha=0.15, beta=0.65,
             cop1=PLACKETTcop, cop2=PLACKETTcop, para1=0.005, para2=1000)
set.seed(1)
uv <- simCOP(n=nsim, cop=composite2COP, para=para, pch=16, col=rgb(0,0,0,.2))
mtext("A highly complex simulated bivariate relation")
# set.seed(1) # try not resetting the seed
uv.grid <- EMPIRgrid(para=uv, deluv=0.025)

tmp <- EMPIRsim(n=nsim, empgrid=uv.grid, kumaraswamy=FALSE, col=rgb(1,0,0,0.1),pch=16)
mtext("Resimulation without Kumaraswamy smoothing")

tmp <- EMPIRsim(n=nsim, empgrid=uv.grid, kumaraswamy=TRUE,  col=rgb(1,0,0,0.1),pch=16)
mtext("Resimulation but using the Kumaraswamy Distribution for smoothing")
dev.off()
}
# See other examples under EMPIRsimv
}
\keyword{empirical copula}
\keyword{empirical copula (simulation)}

