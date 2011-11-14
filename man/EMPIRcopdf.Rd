\name{EMPIRcopdf}
\alias{EMPIRcopdf}
\title{ Dataframe of the Bivariate Emprical Copula }
\description{
Generate a \code{data.frame} representation of the bivariate empirical copula using the coordinates as preserved in the raw data in the parameter object of the empirical copula.
}
\usage{
EMPIRcopdf(para=NULL, ...)
}
\arguments{
  \item{para}{A vector (single element) of parameters---the U-statistics of the data (see example) to pass to \code{\link{EMPIRcop}}, and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The \eqn{u}, \eqn{v}, and \eqn{\mathbf{C}_{n}(u,v)} values of the bivariate empirical copula are returned.
}
\note{
See p. 140 of the reference.
}
\references{
Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007,
Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{ W.H. Asquith }
\seealso{ \code{\link{EMPIRcop}} }
\examples{
psp <- simCOP(n=250, cop=PSP, ploton=FALSE, points=FALSE) *150;
# Pretend psp is real data, the use of *150 is to clearly get the
# probabilities from simCOP into some other arbitrary unit system.

# The sort=FALSE is critical in the following two calls
fakeU <- pp(psp[,1], sort=FALSE); # Weibull plotting position i/(n+1)
fakeV <- pp(psp[,2], sort=FALSE); # Weibull plotting position i/(n+1)

uv <- data.frame(U=fakeU, V=fakeV); # our U-statistics

empcop <- EMPIRcopdf(para=uv)
plot(empcop$u, empcop$v, col=rgb(0,0,0,empcop$v),
     xlab="U NONEXCEEDANCE PROBABILITY",
     ylab="V NONEXCEEDANCE PROBABILITY")

plot(empcop$u, empcop$v, cex=empcop$v,
     xlab="U NONEXCEEDANCE PROBABILITY",
     ylab="V NONEXCEEDANCE PROBABILITY")

}
\keyword{distribution}
\keyword{multivariate}
