\encoding{utf8}
\name{EMPIRcopdf}
\alias{EMPIRcopdf}
\title{ Dataframe of the Bivariate Empirical Copula }
\description{
Generate an \R \code{data.frame} representation of the \emph{bivariate empirical copula} (Salvadori \emph{et al.}, 2007, p. 140) using the coordinates as preserved in the raw data in the parameter object of the bivariate empirical copula.
}
\usage{
EMPIRcopdf(para=NULL, ...)
}
\arguments{
  \item{para}{A vector (single element) of parameters---the U-statistics of the data (see example) to pass to \code{\link{EMPIRcop}}; and}
  \item{...}{Additional arguments to pass to \code{\link{EMPIRcop}}.}
}
\value{
  An \R \code{data.frame} of \eqn{u}, \eqn{v}, and \eqn{\mathbf{C}_{n}(u,v)} values of the bivariate empirical copula is returned.
}
\references{
Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{ W.H. Asquith }
\seealso{ \code{\link{EMPIRcop}} }
\examples{
\dontrun{
psp <- simCOP(n=39, cop=PSP, ploton=FALSE, points=FALSE) * 150
# Pretend psp is real data, the * 150 is to clearly get into an arbitrary unit system.

# The sort=FALSE is critical in the following two calls to pp() from lmomco.
fakeU <- lmomco::pp(psp[,1], sort=FALSE) # Weibull plotting position i/(n+1)
fakeV <- lmomco::pp(psp[,2], sort=FALSE) # Weibull plotting position i/(n+1)
uv <- data.frame(U=fakeU, V=fakeV) # our U-statistics

empcop <- EMPIRcopdf(para=uv)
plot(empcop$u, empcop$v, cex=1.75*empcop$empcop, pch=16,
     xlab="U NONEXCEEDANCE PROBABILITY", ylab="V NONEXCEEDANCE PROBABILITY")
# Dot size increases with joint probability (height of the copulatic surface).
points(empcop$u, empcop$v, col=2) # red circles}
}
\keyword{empirical copula}
\keyword{empirical copula (utility)}


