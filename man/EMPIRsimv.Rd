\encoding{utf8}
\name{EMPIRsimv}
\alias{EMPIRsimv}
\title{Simulate a Bivariate Empirical Copula For a Fixed Value of U}
\description{
\emph{EXPERIMENTAL}---Perform a simulation on a \emph{bivariate empirical copula} to extract the random variates \eqn{V} from a given and fixed value for \eqn{u=} constant. The purpose of this function is to return a simple vector of the \eqn{V} simulations. This behavior is similar to \code{\link{simCOPmicro}} but differs from the general 2-D simulation implemented in the other functions: \code{\link{EMPIRsim}} and \code{\link{simCOP}}---these two functions generate \R \code{data.frame}s of simulated random variates \eqn{U} and \eqn{V} and optional graphics as well.

For the usual situation in which \eqn{u} is not a value aligned on the grid, then the bounding conditional quantile functions are solved for each of the \eqn{n} simulations and the following interpolation is made by
\deqn{v = \frac{v_1/w_1 + v_2/w_2}{1/w_1 + 1/w_2}\mbox{,}}
which states that that the weighted mean is computed. The values \eqn{v_1} and \eqn{v_2} are ordinates of the conditional quantile function for the respective grid lines to the left and right of the \eqn{u} value. The values \eqn{w_1} \eqn{=} \eqn{u - u^\mathrm{left}_\mathrm{grid}} and \eqn{w_2} \eqn{=} \eqn{u^\mathrm{right}_\mathrm{grid} - u}.
}
\usage{
EMPIRsimv(u, n=1, empgrid=NULL, kumaraswamy=FALSE, ...)
}
\arguments{
  \item{u}{The fixed probability \eqn{u} on which to perform conditional simulation for a sample of size \eqn{n};}
  \item{n}{A sample size, default is 1;}
  \item{empgrid}{Gridded empirical copula from \code{\link{EMPIRgrid}};}
  \item{kumaraswamy}{A logical to trigger Kumaraswamy distribution smoothing of the conditional quantile function that is passed to \code{\link{EMPIRgridderinv}}. The Kumaraswamy distribution is a distribution having support \eqn{[0,1]} with an explicit quantile function and takes the place of a Beta distribution (see \pkg{lmomco} function \code{quakur()} for more details); and}
  \item{...}{Additional arguments to pass.}
}
\value{
  A vector of simulated \eqn{V} values is returned.
}
\author{W.H. Asquith}
\seealso{\code{\link{EMPIRgrid}}, \code{\link{EMPIRsim}}}
\examples{
\dontrun{
nsim <- 3000
para <- list(alpha=0.15,  beta=0.65,
             cop1=PLACKETTcop, cop2=PLACKETTcop, para1=.005, para2=1000)
set.seed(10)
uv <- simCOP(n=nsim, cop=composite2COP, para=para, pch=16, col=rgb(0,0,0,0.2))
uv.grid <- EMPIRgrid(para=uv, deluv=.1)
set.seed(1)
V1 <- EMPIRsimv(u=0.6, n=nsim, empgrid=uv.grid)
set.seed(1)
V2 <- EMPIRsimv(u=0.6, n=nsim, empgrid=uv.grid, kumaraswamy=TRUE)
plot(V1,V2)
abline(0,1)

invgrid1 <- EMPIRgridderinv(empgrid=uv.grid)
invgrid2 <- EMPIRgridderinv(empgrid=uv.grid, kumaraswamy=TRUE)
att <- attributes(invgrid2); kur <- att$kumaraswamy
# Now draw random variates from the Kumaraswamy distribution using
# rlmomco() and vec2par() provided by the lmomco package.
set.seed(1)
kurpar <- lmomco::vec2par(c(kur$Alpha[7], kur$Beta[7]), type="kur")
Vsim <- lmomco::rlmomco(nsim, kurpar)

print(summary(V1))   # Kumaraswamy not core in QDF reconstruction
print(summary(V2))   # Kumaraswamy core in QDF reconstruction
print(summary(Vsim)) # Kumaraswamy use of the kumaraswamy

# Continuing with a conditional quantile 0.74 that will not land along one of the
# grid lines, a weighted interpolation will be done.
set.seed(1) # try not resetting the seed
nsim <- 5000
V <- EMPIRsimv(u=0.74, n=nsim, empgrid=uv.grid)
# It is important that the uv.grid used to make V is the same grid used in inversion
# with kumaraswamy=TRUE to guarantee that the correct Kumaraswamy parameters are
# available if a user is doing cut and paste and exploring these examples.
set.seed(1)
V1 <- lmomco::rlmomco(nsim, lmomco::vec2par(c(kur$Alpha[8], kur$Beta[8]), type="kur"))
set.seed(1)
V2 <- lmomco::rlmomco(nsim, lmomco::vec2par(c(kur$Alpha[9], kur$Beta[9]), type="kur"))

plot( lmomco::pp(V),  sort(V), type="l", lwd=4, col=8) # GREY is empirical from grid
lines(lmomco::pp(V1), sort(V1), col=2, lwd=2) # Kumaraswamy at u=0.7 # RED
lines(lmomco::pp(V2), sort(V2), col=3, lwd=2) # Kumaraswamy at u=0.8 # GREEN

W1 <- 0.74 - 0.7; W2 <- 0.8 - 0.74
Vblend <- (V1/W1 + V2/W2) / sum(1/W1 + 1/W2)
lines(lmomco::pp(Vblend), sort(Vblend), col=4, lwd=2) # BLUE LINE
# Notice how the grey line and the blue diverge for about F < 0.1 and F > 0.9.
# These are the limits of the grid spacing and linear interpolation within the
# grid intervals is being used and not direct simulation from the Kumaraswamy.}
}
\keyword{empirical copula}
\keyword{empirical copula (simulation)}

