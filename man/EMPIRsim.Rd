\name{EMPIRsim}
\alias{EMPIRsim}
\title{ Simulate an Empirical Copula  }
\description{
Perform a simulation on an empirical copula to simulate the variates \eqn{U} and \eqn{V} and return a data frame of them. This function is an empirical parallel to \code{\link{simCOP}} for parametric copulas. This behavior differs from that in \code{\link{EMPIRsimv}}, which a vector of \eqn{V} from a fixed \eqn{u} is generated.

For the situation in which an individual \eqn{u} during the simulation loops is not a value on the grid, then the bounding conditional quantile functions are solved for each of the \eqn{n} simulations and the following interpolation is made.
\deqn{v = \frac{v_1/w_1 + v_2/w_2}{1/w_1 + 1/w_2}}
which states that that the weighted mean is computed. The values \eqn{v_1} and \eqn{v_2} are ordinates of the conditional quantile function at for the grid lines to the left and right of the \eqn{u} value. The value \eqn{w_1} \eqn{=} \eqn{u - u^\mathrm{left}_\mathrm{grid}}, and the value \eqn{w_2} \eqn{=} \eqn{u^\mathrm{right}_\mathrm{grid} - u}.
}
\usage{
EMPIRsim(n=100, empgrid=NULL, kumaraswamy=FALSE,
         ploton=TRUE, points=TRUE, ...)
}
\arguments{
  \item{n}{A sample size, default is 100;}
  \item{empgrid}{Gridded empirical copula from \code{\link{EMPIRgrid}};}
  \item{kumaraswamy}{A logical to trigger Kumaraswamy smoothing of the conditional quantile function that is passed to \code{\link{EMPIRgridderinv}};}
  \item{ploton}{A logical to toggle on the plot;}
  \item{points}{A logical to actually draw the simulations by \code{points()}; and}
  \item{...}{Additional arguments to pass to the \code{points()} function or to \code{\link{EMPIRgridderinv}}.}
}
\value{
  A data frame of the simulated values is returned.
}
\author{ W.H. Asquith}
\seealso{\code{\link{EMPIRgrid}}, \code{\link{EMPIRgridderinv}},  \code{\link{EMPIRsimv}} }
\examples{

\dontrun{
pdf("experiment.pdf")
nsim <- 5000
para <- list(alpha=.15, beta=.65,
             cop1=PLACKETTcop, cop2=PLACKETTcop, para1=.005, para2=1000)
set.seed(1)
uv <- simCOP(n=nsim, cop=composite2COP, para=para, pch=16, col=rgb(0,0,0,.2))
# set.seed(1) # try not resetting the seed
uv.grid <- EMPIRgrid(para=uv, deluv=.025)
tmp <- EMPIRsim(n=nsim, empgrid=uv.grid, kumaraswamy=FALSE,
                col=rgb(1,0,0,.1), pch=16)
tmp <- EMPIRsim(n=nsim, empgrid=uv.grid, kumaraswamy=TRUE,
                col=rgb(1,0,0,.1), pch=16)
dev.off()
}


# See examples under EMPIRsimv
}
\keyword{distribution}
\keyword{multivariate}
