\name{EMPIRgridder2}
\alias{EMPIRgridder2}
\title{ Derivatives of the Grid of the Bivariate Empirical Copula for U with respect to V }
\description{
Generate derivatives of \eqn{U} with respect to \eqn{V} of a gridded representation of the \emph{bivariate empirical copula} (see \code{\link{EMPIRcop}}).  This function is the empirical analog to \code{\link{derCOP2}}.
}
\usage{
EMPIRgridder2(empgrid=NULL, ...)
}
\arguments{
  \item{empgrid}{The grid from \code{\link{EMPIRgrid}}; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The gridded values of the derivatives of the bivariate empirical copula.
}
\author{ W.H. Asquith }
\seealso{ \code{\link{EMPIRcop}}, \code{\link{EMPIRcopdf}}, \code{\link{EMPIRgrid}}, \code{\link{EMPIRgridder}} }
\examples{
# See examples under EMPIRgridder
}
\keyword{empirical copula}
\keyword{empirical copula (derivatives)}
\keyword{derivative}
