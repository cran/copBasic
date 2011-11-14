\name{EMPIRgrid}
\alias{EMPIRgrid}
\title{ Grid of the Bivariate Emprical Copula }
\description{
Generate a gridded representation of the bivariate empirical copula.
}
\usage{
EMPIRgrid(para=NULL, deluv=0.05, ...)
}
\arguments{
  \item{para}{A vector (single element) of parameters---the U-statistics of the data (see example),}
  \item{deluv}{A delta value of the x and y (\eqn{u} and \eqn{v}) axis for copula estimation from \code{\link{EMPIRcop}}, and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The gridded values of \eqn{u}, \eqn{v}, and \eqn{\mathbf{C}_{n}(u,v)} values of the bivariate empirical copula are returned. (Well only \eqn{\mathbf{C}_{n}(u,v)} is in the form of a grid or \code{matrix}.) The \code{deluv} used to generated the grid is also returned.
}
\note{
See p. 140 of the reference.
}
\references{
Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007,
Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{ W.H. Asquith }
\seealso{ \code{\link{EMPIRcop}}, \code{\link{EMPIRcopdf}} }
\examples{
psp <- simCOP(n=500, cop=PSP, ploton=FALSE, points=FALSE) *150;
# Pretend psp is real data, the use of *150 is to clearly get the
# probabilities from simCOP into some other arbitrary unit system.

# The sort=FALSE is critical in the following two calls
fakeU <- pp(psp[,1], sort=FALSE); # Weibull plotting position i/(n+1)
fakeV <- pp(psp[,2], sort=FALSE); # Weibull plotting position i/(n+1)

uv <- data.frame(U=fakeU, V=fakeV); # our U-statistics

# The follow function is used to make 3D-->2D transformation
# From R Graphics by Murrell (2005, p.112)
"trans3d" <- function(x,y,z, pmat) {
   tmat <- cbind(x,y,z,1) %*% pmat;
   return(tmat[,1:2] / tmat[,4]);
}

the.grid <- EMPIRgrid(para=uv)
cop.diag <- diagCOP(cop=EMPIRcop, para=uv, ploton=FALSE, lines=FALSE)
empcop   <- EMPIRcopdf(para=uv) # data frame of all points

# PLOT NUMBER ONE
the.persp <- persp(the.grid$empcop, theta=-25, phi=20,
              xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")

# PLOT NUMBER TWO (note difference in interaction with the.grid)
the.persp <- persp(x=the.grid$u, y=the.grid$v, z=the.grid$empcop,
                   theta=-25, phi=20,
              xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")

the.diag <- trans3d(cop.diag$t, cop.diag$t, cop.diag$diagcop, the.persp)
lines(the.diag, lwd=4, col=3, lty=2)

points(trans3d(empcop$u, empcop$v, empcop$empcop, the.persp),
       col=rgb(0,1-sqrt(empcop$empcop),1,sqrt(empcop$empcop)), pch=16)
# the sqrt() is need to darken or enhance the colors

S <- sectionCOP(cop=PSP, .2, ploton=FALSE, lines=FALSE)
some.lines <- trans3d(rep(0.2, length(S$t)),
                      S$t, S$seccop, the.persp)
lines(some.lines, lwd=2, col=6) 
S <- sectionCOP(cop=PSP, .2, ploton=FALSE, lines=FALSE, dercop=TRUE)
some.lines <- trans3d(rep(0.2, length(S$t)),
                      S$t, S$seccop, the.persp)
lines(some.lines, lwd=2, col=6, lty=2) 


S <- sectionCOP(cop=PSP, .85, ploton=FALSE, lines=FALSE, wrtV=TRUE)
some.lines <- trans3d(S$t, rep(0.85, length(S$t)), S$seccop, the.persp)
lines(some.lines, lwd=2, col=2) 
S <- sectionCOP(cop=PSP, .85, ploton=FALSE, lines=FALSE, dercop=TRUE)
some.lines <- trans3d(S$t, rep(0.85, length(S$t)), S$seccop, the.persp)
lines(some.lines, lwd=2, col=2, lty=2) 




empder <- EMPIRgridder(empgrid=the.grid)
some.lines <- trans3d(rep(0.2, length(the.grid$v)), the.grid$v,
                      empder[3,], the.persp)
lines(some.lines, lwd=4, col=6) 



\dontrun{
# PLOT NUMBER THREE
psp <- simCOP(n=3000, cop=PSP, ploton=FALSE, points=FALSE) *150;
# Pretend psp is real data, the use of *150 is to clearly get the
# probabilities from simCOP into some other arbitrary unit system.

# The sort=FALSE is critical in the following two calls
fakeU <- pp(psp[,1], sort=FALSE); # Weibull plotting position i/(n+1)
fakeV <- pp(psp[,2], sort=FALSE); # Weibull plotting position i/(n+1)

uv <- data.frame(U=fakeU, V=fakeV); # our U-statistics

deluv <- 0.0125
the.grid <- EMPIRgrid(para=uv, deluv=deluv)
the.persp <- persp(x=the.grid$u, y=the.grid$v, z=the.grid$empcop,
                   theta=-25, phi=20,
              xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")


S <- sectionCOP(cop=PSP, 1, ploton=FALSE, lines=FALSE)
some.lines <- trans3d(rep(1, length(S$t)), S$t, S$seccop, the.persp)
lines(some.lines, lwd=2, col=2) 

S <- sectionCOP(cop=PSP, 0, ploton=FALSE, lines=FALSE)
some.lines <- trans3d(rep(0, length(S$t)), S$t, S$seccop, the.persp)
lines(some.lines, lwd=2, col=2)

S <- sectionCOP(cop=PSP, 1, ploton=FALSE, lines=FALSE, dercop=TRUE)
some.lines <- trans3d(rep(1, length(S$t)), S$t, S$seccop, the.persp)
lines(some.lines, lwd=2, col=2, lty=2) 

S <- sectionCOP(cop=PSP, 2*deluv, ploton=FALSE, lines=FALSE, dercop=TRUE)
some.lines <- trans3d(rep(2*deluv, length(S$t)), S$t, S$seccop, the.persp)
lines(some.lines, lwd=2, col=2, lty=2)

empder <- EMPIRgridder(empgrid=the.grid)
some.lines <- trans3d(rep(2*deluv, length(the.grid$v)), the.grid$v,
                      empder[3,], the.persp)
lines(some.lines, lwd=4, col=5, lty=2) 

pdf("test.pdf")
  ix <- 1:length(attributes(empder)$rownames)
  for(i in ix) {
    u <- as.numeric(attributes(empder)$rownames[i])
    S <- sectionCOP(cop=PSP, u, ploton=FALSE,
                    lines=FALSE, dercop=TRUE)
    plot(S$t, S$seccop, lwd=2, col=2, lty=2, type="l",
         xlab="V, NONEXCEEDANCE PROBABILITY",
         ylab="V, VALUE", xlim=c(0,1), ylim=c(0,1))
    lines(the.grid$v, empder[i,], lwd=4, col=5, lty=2)
    mtext(paste("Conditioned on U=",u," nonexceedance probability"))
  }
dev.off()

}
}
\keyword{distribution}
\keyword{multivariate}
