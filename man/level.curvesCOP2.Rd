\encoding{utf8}
\name{level.curvesCOP2}
\alias{level.curvesCOP2}
\title{Compute and Plot Level Curves of a Copula U with respect to V}
\description{
Compute and plot \emph{level curves} or \emph{level sets} of a copula for \eqn{U} with respect to \eqn{V} (Nelsen, 2006, pp. 12--13). The level curves at a levels \eqn{t \mapsto [0+\Delta t, 1-\Delta t, \Delta t]} are defined for \eqn{V \mapsto [0+\Delta v, 1-\Delta v, \Delta v]} by
\deqn{t = \mathbf{C}(u, v=V)\mbox{,}}
and solving for \eqn{u}. Plotting is provided by this function because level curves are such an important visual attribute of a copula and highly useful for pedagogic  purposes. The above equation is implemented by the \emph{inverse of a copula} using \code{\link{COPinv2}}.
}
\usage{
level.curvesCOP2(cop=NULL, para=NULL, ploton=TRUE, lines=TRUE,
                 plotMW=FALSE, ramp=TRUE, delv=0.001, delt=0.10,
                 getlevel=NULL, silent=TRUE, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{ploton}{A logical to toggle on the plot;}
  \item{lines}{A logical to toggle calls to the \code{lines()} function in \R to draw the lines;}
  \item{plotMW}{A logical to toggle to use \code{abline()} function in \R to plot cross lines for the \eqn{\mathbf{M}} (\code{\link{M}}) and  \eqn{\mathbf{W}} (\code{\link{W}}) copulas;}
  \item{ramp}{A logical to toggle whether the level curves are ramped in thickness according to the probability of the line;}
  \item{delv}{The increment of \eqn{\Delta v}. The default is 1 part in 1,000, which should often provide enough smoothness for many copulas in practice;}
  \item{delt}{The increment of \eqn{\Delta t} for the level curves to plot, defaults to 10-percent intervals;}
  \item{getlevel}{If defined and level exists upon stepping through using \code{delt}, then the level curve at the \code{getlevel} is returned in an \R \code{list} data structure;}
  \item{silent}{The argument of the same name given over to \code{try()} wrapping the \code{try()} operation on forming sequences of \eqn{t} for the curves (see sources); and}
  \item{...}{Additional arguments to pass to the \code{lines()} function in \R.}
}
\value{
  Typically no values are returned because this function is used for its side effects, but the arguments can be such that the \eqn{\{u, v\}} for \eqn{\mathbf{C}(u,v) = t} are returned within an \R \code{list}.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{COPinv2}}, \code{\link{level.curvesCOP}}, \code{\link{level.setCOP2}}, \code{\link{joint.curvesCOP2}}
}
\examples{
\dontrun{
level.curvesCOP2(cop=M, para=NULL, delt=0.02) # Upper bounds copula}
}
\keyword{visualization}
\keyword{diagnostics}
\concept{level curves}
\concept{level curve}
\concept{level sets}
\concept{level set}
\concept{level contours}
\concept{level contour}
