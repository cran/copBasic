\name{EMPIRgrid}
\alias{EMPIRgrid}
\title{ Grid of the Bivariate Emprical Copula }
\description{
Generate a gridded representation of the \emph{bivariate empirical copula} (see \code{\link{EMPIRcop}}, Salvadori \emph{et al.}, 2007, p. 140).  This function has the primary intention of supporting 3-D renderings or 2-D images of the \emph{copulatic surface}, but many empirical copula functions in \pkg{copBasic} rely on the grid of the empirical copula---unlike the functions support parametric copulas.
}
\usage{
EMPIRgrid(para=NULL, deluv=0.05, verbose=FALSE, ...)
}
\arguments{
  \item{para}{A vector (single element) of parameters---the U-statistics of the data (see example);}
  \item{deluv}{A delta value of the both the \eqn{u} and \eqn{v}  axes for copula estimation from \code{\link{EMPIRcop}};}
  \item{verbose}{A logical controlling whether the progress during grid building is to be shown; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  An \R \code{list} of the gridded values of \eqn{u}, \eqn{v}, and \eqn{\mathbf{C}_{n}(u,v)} values of the bivariate empirical copula is returned. (Well only \eqn{\mathbf{C}_{n}(u,v)} is in the form of a grid as an \R \code{matrix}.) The \code{deluv} used to generated the grid is also returned.
}
\note{
The extensive suite of examples is included here because the various ways that algorithms involving empirical copulas can be tested. The figures also provide excellent tools for education on copulas.
}
\references{
Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{ W.H. Asquith }
\seealso{ \code{\link{EMPIRcop}}, \code{\link{EMPIRcopdf}} }
\examples{
\dontrun{
# EXAMPLE 1:
psp <- simCOP(n=490, cop=PSP, ploton=FALSE, points=FALSE) * 150
# Pretend psp is real data, the * 150 is to clearly get into an arbitrary unit system.

# The sort=FALSE is critical in the following two calls to pp() from lmomco.
fakeU <- lmomco::pp(psp[,1], sort=FALSE)   # Weibull plotting position i/(n+1)
fakeV <- lmomco::pp(psp[,2], sort=FALSE)   # Weibull plotting position i/(n+1)
uv <- data.frame(U=fakeU, V=fakeV) # our U-statistics

# The follow function is used to make 3-D --> 2-D transformation
# From R Graphics by Murrell (2005, p.112)
"trans3d" <- function(x,y,z, pmat) {
   tmat <- cbind(x,y,z,1) \%*\% pmat; return(tmat[,1:2] / tmat[,4])
}

the.grid <- EMPIRgrid(para=uv)
cop.diag <- diagCOP(cop=EMPIRcop, para=uv, ploton=FALSE, lines=FALSE)
empcop   <- EMPIRcopdf(para=uv) # data.frame of all points

# EXAMPLE 1: PLOT NUMBER 1
the.persp <- persp(the.grid$empcop, theta=-25, phi=20,
                   xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")

# EXAMPLE 1: PLOT NUMBER 2 (see change in interaction with variable 'the.grid')
the.persp <- persp(x=the.grid$u, y=the.grid$v, z=the.grid$empcop, theta=-25, phi=20,
                   xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")

the.diag <- trans3d(cop.diag$t, cop.diag$t, cop.diag$diagcop, the.persp)
lines(the.diag, lwd=4, col=3, lty=2)

points(trans3d(empcop$u, empcop$v, empcop$empcop, the.persp),
       col=rgb(0,1-sqrt(empcop$empcop),1,sqrt(empcop$empcop)), pch=16)
# the sqrt() is needed to darken or enhance the colors

S <- sectionCOP(cop=PSP, 0.2, ploton=FALSE, lines=FALSE)
thelines <- trans3d(rep(0.2, length(S$t)), S$t, S$seccop, the.persp)
lines(thelines, lwd=2, col=6)
S <- sectionCOP(cop=PSP, 0.2, ploton=FALSE, lines=FALSE, dercop=TRUE)
thelines <- trans3d(rep(0.2, length(S$t)), S$t, S$seccop, the.persp)
lines(thelines, lwd=2, col=6, lty=2)

S <- sectionCOP(cop=PSP, 0.85, ploton=FALSE, lines=FALSE, wrtV=TRUE)
thelines <- trans3d(S$t, rep(0.85, length(S$t)), S$seccop, the.persp)
lines(thelines, lwd=2, col=2)
S <- sectionCOP(cop=PSP, 0.85, ploton=FALSE, lines=FALSE, dercop=TRUE)
thelines <- trans3d(S$t, rep(0.85, length(S$t)), S$seccop, the.persp)
lines(thelines, lwd=2, col=2, lty=2)

empder <- EMPIRgridder(empgrid=the.grid)
thelines <- trans3d(rep(0.2, length(the.grid$v)), the.grid$v, empder[3,], the.persp)
lines(thelines, lwd=4, col=6) #}

\dontrun{
# EXAMPLE 2:
# An asymmetric example to demonstrate that the grid is populated with the
# correct orientation---meaning U is the horizontal and V is the vertical.
"MOcop" <- function(u,v, para=NULL) { # Marshall-Olkin copula
   alpha <- para[1]; beta  <- para[2]; return(min(v*u^(1-alpha), u*v^(1-beta)))
}
# EXAMPLE 2: PLOT NUMBER 1 # Note the asymmetry!
uv <- simCOP(1000, cop=MOcop, para=c(0.4,0.9)) # The parameters cause asymmetry.
mtext("Simulation from a defined Marshall-Olkin Copula")
the.grid <- EMPIRgrid(para=uv, deluv=0.025)

# EXAMPLE 2: PLOT NUMBER 2
# The second plot by image() will show a "hook" of sorts along the singularity.
image(the.grid$empcop, col=terrain.colors(40)) # Second plot is made
mtext("Image of gridded Empirical Copula")

# EXAMPLE 2: PLOT NUMBER 3
empcop <- EMPIRcopdf(para=uv) # data.frame of all points
# The third plot is the 3-D version overlain with the data points.
the.persp <- persp(x=the.grid$u, y=the.grid$v, z=the.grid$empcop, theta=240, phi=40,
                   xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")
points(trans3d(empcop$u, empcop$v, empcop$empcop, the.persp),
       col=rgb(0,1-sqrt(empcop$empcop),1,sqrt(empcop$empcop)), pch=16)
mtext("3-D representation of gridded empirical copula with data points")

# EXAMPLE 2: PLOT NUMBER 4
# The fourth plot shows a simulation and the quasi-emergence of the singularity
# that of course the empirical perspective "knows" nothing about. Do not use
# the Kumaraswamy smoothing because in this case the singularity because
# too smoothed out relative to the raw empirical, but of course the sample size
# is large enough to see such things. (Try kumaraswamy=TRUE)
empsim <- EMPIRsim(n=1000, empgrid = the.grid, kumaraswamy=FALSE)
mtext("Simulations from the Empirical Copula") #}

\dontrun{
# EXAMPLE 3:
psp <- simCOP(n=4900, cop=PSP, ploton=FALSE, points=FALSE) * 150
# Pretend psp is real data, the * 150 is to clearly get into an arbitrary unit system.

# The sort=FALSE is critical in the following two calls to pp() from lmomco.
fakeU <- lmomco::pp(psp[,1], sort=FALSE)   # Weibull plotting position i/(n+1)
fakeV <- lmomco::pp(psp[,2], sort=FALSE)   # Weibull plotting position i/(n+1)
uv <- data.frame(U=fakeU, V=fakeV) # our U-statistics

# EXAMPLE 3: # PLOT NUMBER 1
deluv <- 0.0125 # going to cause long run time with large n
# The small deluv is used to explore solution quality at U=0 and 1.
the.grid <- EMPIRgrid(para=uv, deluv=deluv)
the.persp <- persp(x=the.grid$u, y=the.grid$v, z=the.grid$empcop, theta=-25, phi=20,
                   xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")

S <- sectionCOP(cop=PSP, 1, ploton=FALSE, lines=FALSE)
thelines <- trans3d(rep(1, length(S$t)), S$t, S$seccop, the.persp)
lines(thelines, lwd=2, col=2)

S <- sectionCOP(cop=PSP, 0, ploton=FALSE, lines=FALSE)
thelines <- trans3d(rep(0, length(S$t)), S$t, S$seccop, the.persp)
lines(thelines, lwd=2, col=2)

S <- sectionCOP(cop=PSP, 1, ploton=FALSE, lines=FALSE, dercop=TRUE)
thelines <- trans3d(rep(1, length(S$t)), S$t, S$seccop, the.persp)
lines(thelines, lwd=2, col=2, lty=2)

S <- sectionCOP(cop=PSP, 2*deluv, ploton=FALSE, lines=FALSE, dercop=TRUE)
thelines <- trans3d(rep(2*deluv, length(S$t)), S$t, S$seccop, the.persp)
lines(thelines, lwd=2, col=2, lty=2)

empder <- EMPIRgridder(empgrid=the.grid)
thelines <- trans3d(rep(2*deluv,length(the.grid$v)),the.grid$v,empder[3,],the.persp)
lines(thelines, lwd=4, col=5, lty=2)

pdf("conditional_distributions.pdf")
  ix <- 1:length(attributes(empder)$rownames)
  for(i in ix) {
     u <- as.numeric(attributes(empder)$rownames[i])
     S <- sectionCOP(cop=PSP, u, ploton=FALSE, lines=FALSE, dercop=TRUE)
     # The red line is the true.
     plot(S$t, S$seccop, lwd=2, col=2, lty=2, type="l", xlim=c(0,1), ylim=c(0,1),
          xlab="V, NONEXCEEDANCE PROBABILITY", ylab="V, VALUE")
     lines(the.grid$v, empder[i,], lwd=4, col=5, lty=2) # empirical
     mtext(paste("Conditioned on U=",u," nonexceedance probability"))
  }
dev.off()#}
}
\keyword{empirical copula}
\keyword{empirical copula (utility)}
\keyword{copulatic surface}
