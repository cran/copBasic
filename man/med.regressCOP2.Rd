\encoding{utf8}
\name{med.regressCOP2}
\alias{med.regressCOP2}
\title{Perform Median Regression using a Copula by Numerical Derivative Method for U with respect to V}
\description{
Perform \emph{median regression} of a copula (Nelsen, 2006, pp. 217--218) by inversion of numerical derivatives of the copula (\code{\link{derCOPinv2}}). The documentation for \code{\link{qua.regressCOP2}} provides mathematical details.
}
\usage{
med.regressCOP2(v=seq(0.01,0.99, by=0.01), cop=NULL, para=NULL, level=NA, ...)
}
\arguments{
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{level}{The level of the prediction interval to compute. For example, \code{level=0.95} will compute the 95-percent prediction interval as will \code{level=0.05} because internally a reflection check is made; and}
  \item{...}{Additional arguments to pass such \code{\link{qua.regressCOP2}} and \code{\link{derCOPinv2}} that are called in succession.}
}
\value{
  An \R \code{data.frame} of the median regressed probabilities of \eqn{U} and provided \eqn{V} values is returned. Note: if \code{level} is used, the column ordering of the returned \code{data.frame} changes---please access the columns by the named idiom. The lower- and upper-prediction interval bounds are contained in the columns repectively titled \code{Ulwr} and \code{Uupr} to mimic nomenclature somewhat of function \code{predict.lm()} in \R.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{med.regressCOP}}, \code{\link{qua.regressCOP2}}, \code{\link{qua.regressCOP.draw}}
}
\examples{
# See examples under med.regressCOP
}
\keyword{median regression}
\keyword{quantile regression}
\concept{quantile regression}
