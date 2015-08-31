\name{qua.regressCOP.draw}
\alias{qua.regressCOP.draw}
\title{ Draw Quantile Regressions using a Copula by Numerical Derivative Method for V with respect to U or U with respect to V}
\description{
Draw a suite of lines for specified nonexceedance probabilities representing the \emph{quantile regression} (Nelsen, 2006, pp. 217--218) of either \eqn{V} with respect to \eqn{U} or \eqn{U} with respect to \eqn{V} depending upon an argument setting.
}
\usage{
qua.regressCOP.draw(f=seq(0.1, 0.9, by=0.1), fs=0.5, cop=NULL, para=NULL,
                    ploton=TRUE, wrtV=FALSE, col=c(4,2), lwd=c(1,2), lty=1,...)
}
\arguments{
  \item{f}{Nonexceedance probability \eqn{F} to perform quantile regression at and defaults to a 10-percent-interval sequence. This vectorization of \code{f} for this function differs from that in \code{\link{qua.regressCOP}} and \code{\link{qua.regressCOP2}};}
  \item{fs}{A special value of nonexceedance probability to draw with second values to arguments \code{col} and \code{lwd} and defaults to the median (\eqn{F = 1/2});}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{ploton}{A logical to toggle on the plot;}
  \item{wrtV}{If \code{wrtV=FALSE} call \code{\link{qua.regressCOP}} and perform quantile regression of \eqn{V} with respect to \eqn{U} and if \code{wrtV=TRUE} call \code{\link{qua.regressCOP2}} and perform regression of \eqn{U} with respect to \eqn{V};}
  \item{col}{A vector of two values for the color of the line to draw, where the first value is used for the \code{f} probabilities and the second value is used for the \code{fs} probability;}
  \item{lwd}{A vector of two values for the line width of the line to draw, where the first value is used for the \code{f} probabilities and the second value is used for the \code{fs} probability;}
  \item{lty}{The line type to draw; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  No values are returned, this function is used for its side effects.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{qua.regressCOP}}, \code{\link{qua.regressCOP2}}}
\examples{
# See example in qua.regressCOP documentation
}
\keyword{quantile regression}
\keyword{visualization}

