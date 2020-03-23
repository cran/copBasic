\encoding{utf8}
\name{EMPIRsim}
\alias{EMPIRsim}
\title{ Simulate a Bivariate Empirical Copula  }
\description{
\emph{EXPERIMENTAL}---Perform a simulation on a \emph{bivariate empirical copula} to produce the random variates \eqn{U} and \eqn{V} and return an \R \code{data.frame} of them.  The method is more broadly known as \emph{conditional simulation method}. This function is an empirical parallel to \code{\link{simCOP}} that is used for parametric copulas. If circumstances require conditional simulation of \eqn{V|U}, then function \code{\link{EMPIRsimv}}, which produces a vector of \eqn{V} from a fixed \eqn{u}, should be used.

For the usual situation in which an individual \eqn{u} during the simulation loops is not a value aligned on the grid, then the bounding conditional quantile functions are solved for each of the \eqn{n} simulations and the following interpolation is made by
\deqn{v = \frac{v_1/w_1 + v_2/w_2}{1/w_1 + 1/w_2}\mbox{,}}
which states that that the weighted mean is computed. The values \eqn{v_1} and \eqn{v_2} are ordinates of the conditional quantile function for the respective grid lines to the left and right of the \eqn{u} value. The values \eqn{w_1} \eqn{=} \eqn{u - u^\mathrm{left}_\mathrm{grid}} and \eqn{w_2} \eqn{=} \eqn{u^\mathrm{right}_\mathrm{grid} - u}.
}
\usage{
EMPIRsim(n=100, empgrid=NULL, kumaraswamy=FALSE, na.rm=TRUE, keept=FALSE,
                graphics=TRUE, ploton=TRUE, points=TRUE, snv=FALSE,
                infsnv.rm=TRUE, trapinfsnv=.Machine$double.eps, ...)
}
\arguments{
  \item{n}{A sample size, default is 100;}
  \item{empgrid}{Gridded empirical copula from \code{\link{EMPIRgrid}};}
  \item{kumaraswamy}{A logical to trigger Kumaraswamy distribution smoothing of the conditional quantile function that is passed to \code{\link{EMPIRgridderinv}}.  The Kumaraswamy distribution is a distribution having support \eqn{[0,1]} with an explicit quantile function and takes the place of a Beta distribution (see \pkg{lmomco} function \code{quakur} for more details);}
    \item{na.rm}{A logical to toggle the removal of \code{NA} entries on the returned \code{data.frame};}
  \item{keept}{Keep the \eqn{t} uniform random variable for the simulation as the last column in the returned \code{data.frame};}
  \item{graphics}{A logical that will disable graphics by setting \code{ploton} and \code{points} to \code{FALSE} and overriding whatever their settings were;}
  \item{ploton}{A logical to toggle on the plot;}
  \item{points}{A logical to actually draw the simulations by the \code{points()} function in \R;}
  \item{snv}{A logical to convert the \eqn{\{u,v\}} to standard normal scores (variates) both for the optional graphics and the returned \code{data.frame}. Joe (2014) advocates extensively for use of normal scores, which is in  contrast to Nelsen (2006) who does not;}
   \item{infsnv.rm}{A logical that will quietly strip out any occurrences of \eqn{u = \{0,1\}} or \eqn{v = \{0,1\}} from the simulations because these are infinity in magnitude when converted to standard normal variates is to occur. Thus, this logical only impacts logic flow when \code{snv} is \code{TRUE}. The \code{infsnv.rm} is mutually exclusive from \code{trapinfsnv}; }
   \item{trapinfsnv}{If \code{TRUE} and presumably small, the numerical value of this argument (\eqn{\eta}) is used to replace \eqn{u = \{0,1\}} and \eqn{v = \{0,1\}} with \eqn{u(0) =  v(0) = \eta} or \eqn{u(1) = v(1) = 1 - \eta} as appropriate when conversion to standard normal variates is to occur. The setting of \code{trapinfsnv} only is used if \code{snv} is \code{TRUE} and \code{infsnv.rm} is \code{FALSE}; and}
  \item{...}{Additional arguments to pass to the \code{points()} function in \R.}
}
\value{
  An \R \code{data.frame} of the simulated values is returned.
}
\author{ W.H. Asquith}
\seealso{\code{\link{EMPIRgrid}}, \code{\link{EMPIRgridderinv}},  \code{\link{EMPIRsimv}} }
\examples{

# See other examples under EMPIRsimv

\dontrun{
pdf("EMPIRsim_experiment.pdf")
  nsim <- 5000
  para <- list(alpha=0.15, beta=0.65,
               cop1=PLACKETTcop, cop2=PLACKETTcop, para1=0.005, para2=1000)
  set.seed(1)
  uv <- simCOP(n=nsim, cop=composite2COP, para=para, snv=TRUE,
               pch=16, col=rgb(0,0,0,.2))
  mtext("A highly complex simulated bivariate relation")
  # set.seed(1) # try not resetting the seed
  uv.grid <- EMPIRgrid(para=uv, deluv=0.025)

  uv2 <- EMPIRsim(n=nsim, empgrid=uv.grid, kumaraswamy=FALSE, snv=TRUE,
                  col=rgb(1,0,0,0.1), pch=16)
  mtext("Resimulation without Kumaraswamy smoothing")

  uv3 <- EMPIRsim(n=nsim, empgrid=uv.grid, kumaraswamy=TRUE, snv=TRUE,
                  col=rgb(1,0,0,0.1),pch=16)
  mtext("Resimulation but using the Kumaraswamy Distribution for smoothing")
dev.off()#}
}
\keyword{empirical copula}
\keyword{empirical copula (simulation)}
