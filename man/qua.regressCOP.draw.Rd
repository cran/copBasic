\name{qua.regressCOP.draw}
\alias{qua.regressCOP.draw}
\title{ Draw Quantile Regressions using a Copula by Numerical Derivative Method for V with respect to U or U with respect to V}
\description{
Draw a suite of lines for specified nonexceedance probabilities representing the quantile regress of either \eqn{V} with respect to \eqn{U} or \eqn{U} with respect to \eqn{V}.
}
\usage{
qua.regressCOP.draw(F=c(seq(0.1, 0.9, by=0.1)),
                    Fs=0.5, cop=NULL, para=NULL, ploton=TRUE,
                    swap=FALSE, col=c(4,2), lwd=c(1,2), lty=1, ...)
}
\arguments{
  \item{F}{Vector of nonexceedance probabilities \eqn{F} to perform quantile regression at and defaults to a 10-percentile interval;}
  \item{Fs}{A special value of probability to draw with second values to arguments \code{col} and \code{lwd} and defaults to the median;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{ploton}{A logical to toggle on the plot;}
  \item{swap}{If \code{swap=FALSE} call \code{\link{qua.regressCOP}} and perform quantile regression of \eqn{V} with respect to \eqn{U} and if \code{swap=TRUE} call \code{\link{qua.regressCOP2}} and perform regression of \eqn{U} with respect to \eqn{V};}
  \item{col}{A vector of two values for the color of the line to draw, where the first value is used for the \code{F} probabilities and the second value is used for the \code{Fs} probability;}
  \item{lwd}{A vector of two values for the line width of the line to draw, where the first value is used for the \code{F} probabilities and the second value is used for the \code{Fs} probability; }
  \item{lty}{The line type to draw; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  No values are returned, this function is used for its side effects.
}
\author{ W.H. Asquith}
\seealso{\code{\link{qua.regressCOP}}}
\examples{
# See example in qua.regressCOP documentation
}
\keyword{misc}
