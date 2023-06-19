\encoding{utf8}
\name{EMPIRqua.regress2}
\alias{EMPIRqua.regress2}
\title{Quantile Regression of the Grid of the Bivariate Empirical Copula for U with respect to V}
\description{
Generate quantile regression from the gridded inversion of the \emph{bivariate empirical copula} of \eqn{U} with respect to \eqn{V}.
}
\usage{
EMPIRqua.regress2(f=0.5, v=seq(0.01,0.99, by=0.01), empinv=NULL,
                  lowess=FALSE, f.lowess=1/5, ...)
}
\arguments{
  \item{f}{The nonexceedance probability \eqn{F} to perform regression at and defaults to median regression \eqn{F=1/2};}
  \item{v}{A vector of \eqn{v} nonexceedance probabilities;}
  \item{empinv}{The grid from \code{\link{EMPIRgridderinv}};}
  \item{lowess}{Perform \code{lowess} smooth on the quantile regression using the smooth factor of \code{f=f.lowess};}
  \item{f.lowess}{Smooth factor of almost the same argument name fed to the \code{lowess()} function in \R;}
  \item{...}{Additional arguments to pass.}
}
\value{
  The gridded values of the quantile regression of \eqn{U} with respect to \eqn{V}.
}
\author{W.H. Asquith}
\seealso{\code{\link{EMPIRgridderinv2}}, \code{\link{EMPIRqua.regress}}, \code{\link{EMPIRmed.regress}}, \code{\link{EMPIRmed.regress2}}
}
\examples{
# See examples under EMPIRqua.regress
}
\keyword{empirical copula}
\keyword{empirical copula (quantile regression)}
