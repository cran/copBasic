\name{EMPIRsimv}
\alias{EMPIRsimv}
\title{ Simulate an Empirical Copula For a Fixed Value of U  }
\description{

Perform a simulation on an empirical copula to extract the variates \eqn{V} from a given and fixed value for \eqn{u=}constant. The purpose of this function is to return a simple vector of the \eqn{V} simulations. This behavior differs from that in \code{\link{EMPIRsim}} and by \code{\link{simCOP}}; these later two functions generate data frames of simulated \eqn{U} and \eqn{V} and optional graphics as well.

For the situation in which \eqn{u} is not a value on the grid, then the bounding conditional quantile functions are solved for each of the \eqn{n} simulations and the following interpolation is made.
\deqn{v = \frac{v_1/w_1 + v_2/w_2}{1/w_1 + 1/w_2}}
which states that that the weighted mean is computed. The values \eqn{v_1} and \eqn{v_2} are ordinates of the conditional quantile function at for the grid lines to the left and right of the \eqn{u} value. The value \eqn{w_1} \eqn{=} \eqn{u - u^\mathrm{left}_\mathrm{grid}}, and the value \eqn{w_2} \eqn{=} \eqn{u^\mathrm{right}_\mathrm{grid} - u}.
}
\usage{
EMPIRsimv(u, n=1, empgrid=NULL, kumaraswamy=FALSE, ...)
}
\arguments{
  \item{u}{The fixed probability \eqn{u} on which to perform conditional simulation for a sample of size \eqn{n};}
  \item{n}{A sample size, default is 1;}
  \item{empgrid}{Gridded empirical copula from \code{\link{EMPIRgrid}};}
  \item{kumaraswamy}{A logical to trigger Kumaraswamy smoothing of the conditional quantile function that is passed to \code{\link{EMPIRgridderinv}};}
  \item{...}{Additional arguments to pass.}
}
\value{
  A vector of simulated \eqn{V} values is returned.
}
\author{ W.H. Asquith}
\seealso{\code{\link{EMPIRgrid}}, \code{\link{EMPIRsim}} }
\examples{

\dontrun{
nsim <- 3000
para   <- list(alpha=.15,  beta=.65,
               cop1=PLACKETTcop, cop2=PLACKETTcop,
               para1=.005, para2=1000)
set.seed(1)
uv <- simCOP(n=nsim, cop=composite2COP, para=para,
             pch=16, col=rgb(0,0,0,.2))
set.seed(1) # try not resetting the seed
uv.grid <- EMPIRgrid(para=uv, deluv=.1)
V <- EMPIRsimv(u=0.6, n=nsim, empgrid=uv.grid)

invgrid <- EMPIRgridderinv(empgrid=uv.grid)
att <- attributes(invgrid)
kur <- att$kumaraswamy
Vsim <- rlmomco(nsim, vec2par(c(kur$Alpha[7], kur$Beta[7]), type="kur"))

print(summary(V)) # kumaraswamy not core in QDF reconstruction
print(summary(Vsim)) # use of the kumaraswamy

# continuing
set.seed(1) # try not resetting the seed
nsim <- 5000
V <- EMPIRsimv(u=0.74, n=nsim, empgrid=uv.grid)
set.seed(1) # try not resetting the seed
V1 <- rlmomco(nsim, vec2par(c(kur$Alpha[8], kur$Beta[8]), type="kur"))
set.seed(1) # try not resetting the seed
V2 <- rlmomco(nsim, vec2par(c(kur$Alpha[9], kur$Beta[9]), type="kur"))
plot(pp(V), sort(V), type="l", lwd=4, col=8)
lines(pp(V1), sort(V1), col=2, lwd=2)
lines(pp(V2), sort(V2), col=3, lwd=2)

W1 <- 0.74 - 0.7; W2 <- 0.8 - 0.74
Vblend <- (V1/W1 + V2/W2) / sum(1/W1 + 1/W2)
lines(pp(Vblend), sort(Vblend), col=4, lwd=2) # BLUE LINE

# Notice how the grey line and the blue diverge for F < 0.1
# and F > 0.9. These are the limits of the grid spacing and
# linear interpolation within the grid resolution is being
# used and not direct simulation from the kumaraswamy.

}

}
\keyword{distribution}
\keyword{multivariate}
