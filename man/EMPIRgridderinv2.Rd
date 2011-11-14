\name{EMPIRgridderinv2}
\alias{EMPIRgridderinv2}
\title{ Derivatives of the Grid of the Bivariate Emprical Copula }
\description{
Generate a gridded representation of the bivariate empirical copula.
}
\usage{
EMPIRgridderinv2(empgrid=NULL, kumaraswamy=FALSE, ...)
}
\arguments{
  \item{empgrid}{The grid from \code{\link{EMPIRgrid}}, and}
  \item{kumaraswamy}{A logical to trigger Kumaraswamy smoothing of the conditional quantile function;}
  \item{...}{Additional arguments to pass.}
}
\value{
  The gridded values of
}
\author{ W.H. Asquith }
\seealso{ \code{\link{EMPIRcop}}, \code{\link{EMPIRcopdf}}, \code{\link{EMPIRgrid}}, \code{\link{EMPIRgridder2}}, \code{\link{EMPIRgridderinv}}, \code{\link{EMPIRgridderinv2}} }
\examples{
# See examples under EMPIRgridderinv
}
\keyword{distribution}
\keyword{multivariate}
