\name{EMPIRqua.regress2}
\alias{EMPIRqua.regress2}
\title{ Derivatives of the Grid of the Bivariate Emprical Copula }
\description{
Generate quantile regression from the gridded inversion of the bivariate empirical copula.
}
\usage{
EMPIRqua.regress2(F=0.5, V=seq(0.01,0.99, by=0.01), empinv=NULL,
                  lowess=FALSE, f=1/5, ...)
}
\arguments{
  \item{F}{The nonexceedance probability \eqn{F} to perform regression at and defaults to median regression \eqn{F=1/2},}
  \item{V}{A vector of \eqn{v} nonexceedance probabilities,}
  \item{empinv}{The grid from \code{\link{EMPIRgridderinv}},}
  \item{lowess}{Perform \code{lowess} smooth on the quantile regression using the smooth factor of \code{f=1/3} or other,}
  \item{f}{Smooth factor of same argument name fed to \code{lowess},}
  \item{...}{Additional arguments to pass.}
}
\value{
  The gridded values of
}
\author{ W.H. Asquith }
\seealso{ \code{\link{EMPIRgridderinv2}}, \code{\link{EMPIRqua.regress}}, \code{\link{EMPIRmed.regress}}, \code{\link{EMPIRmed.regress2}}
}
\examples{
# See examples under EMPIRqua.regress
}
\keyword{distribution}
\keyword{multivariate}
