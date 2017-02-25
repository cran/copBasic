\name{level.curvesCOP}
\alias{level.curvesCOP}
\title{ Compute and Plot Level Curves of a Copula V with respect to U }
\description{
Compute and plot \emph{level curves} or \emph{level sets} of a copula for \eqn{V} with respect to \eqn{U} (Nelsen, 2006, pp. 12--13). The level curves at levels \eqn{t \mapsto [0+\Delta t, 1-\Delta t, \Delta t]} are defined for \eqn{U \mapsto [0+\Delta u, 1-\Delta u, \Delta u]} by
\deqn{t \mapsto \mathbf{C}(u=U, v)\mbox{,}}
and solving for \eqn{v}. Plotting is provided by this function because level curves are such an important visual attribute of a copula and highly useful for pedagogic purposes. The above equation is implemented by the \emph{inverse of a copula} using \code{\link{COPinv}}.
}
\usage{
level.curvesCOP(cop=NULL, para=NULL, ploton=TRUE, lines=TRUE,
                plotMW=FALSE, ramp=TRUE, delu=0.001, delt=0.10,
                getlevel=NULL, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{ploton}{A logical to toggle on the plot;}
  \item{lines}{A logical to toggle calls to the \code{lines()} function in \R to draw the lines;}
  \item{plotMW}{A logical to toggle to use the \code{abline()} function in \R to plot cross lines for the \eqn{\mathbf{M}} (\code{\link{M}}) and  \eqn{\mathbf{W}} (\code{\link{W}}) copulas;}
  \item{ramp}{A logical to toggle whether the level curves are ramped in thickness according to the probability of the line;}
  \item{delu}{The increment for \eqn{\Delta u}. The default is 1 part in 1,000, which should often provide enough smoothness for many copulas in practice;}
  \item{delt}{The increment \eqn{\Delta t} for the level curves to plot, defaults to 10-percent intervals. If \code{delt=0.5}, then only the median plus the consequences of a defined \code{getlevel} is used. If \code{NULL}, then a sequence of \eqn{t} values is not made and only \code{getlevel} is used (if available);}
  \item{getlevel}{If defined, then it is inserted into the sequence of levels \eqn{t} and that level \eqn{t} \eqn{=} \code{getlevel} is returned in an \R \code{list} data structure. If more than one level is desired, then instead of repeated calls to this function, the \code{\link{joint.curvesCOP}} function could be considered; and}
  \item{...}{Additional arguments to pass to the \code{lines()} function in \R.}
}
\value{
  Typically no values are returned because this function is used for its side effects. But the arguments can be such that the \eqn{\{u, v\}} for \eqn{\mathbf{C}(u,v) = t} are returned within an \R \code{list}.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{COPinv}}, \code{\link{level.curvesCOP2}}, \code{\link{level.setCOP}}, \code{\link{joint.curvesCOP}}
}
\examples{
\dontrun{
level.curvesCOP(cop=M, para=NULL, delt=.02) # Upper bounds copula}
\dontrun{
D <- level.curvesCOP(cop=P,   getlevel=0.56)
str(D) # empty
D <- level.curvesCOP(cop=P,   getlevel=0.5)
str(D) # contains stuff
D <- level.curvesCOP(cop=PSP, getlevel=0.8)
str(D) # contains stuff}
}
\keyword{visualization}
\keyword{diagnostics}
\concept{level curves}
\concept{level curve}
\concept{level sets}
\concept{level set}
\concept{level contours}
\concept{level contour}
