\encoding{utf8}
\name{level.setCOP2}
\alias{level.setCOP2}
\title{ Compute a Level Set of a Copula U with respect to V }
\description{
Compute a \emph{level curve} or \emph{level set} of a copula for \eqn{U} with respect to \eqn{V} (Nelsen, 2006, pp. 12--13). The level curve at level \eqn{t} is defined for \eqn{V \mapsto [0+\Delta v, 1-\Delta v, \Delta v]} by
\deqn{t \mapsto \mathbf{C}(u, v{=}V)\mbox{,}}
and solving for \eqn{u}. The function is largely a dispatcher to features of \code{\link{level.curvesCOP2}}.
}
\usage{
level.setCOP2(cop=NULL, para=NULL, getlevel=NULL, delv=0.001, lines=FALSE, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{getlevel}{The level set for \eqn{t};}
  \item{delv}{The increment for \eqn{\Delta v}. The default is 1 part in 1,000, which should often in practice provide enough smoothness for many copulas;}
  \item{lines}{A logical that matches the argument of the same name in \code{\link{level.curvesCOP2}}; and}
  \item{...}{Additional arguments to pass to the \code{lines()} function in \R.}
}
\value{
  The level set for \eqn{t} \eqn{=} \code{getlevel} is returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{level.setCOP}}, \code{\link{level.curvesCOP2}}
}
\examples{
\dontrun{
set <- level.setCOP2(cop=N4212cop, para=3.1, getlevel=0.23, delu=0.005)
level.curvesCOP2(cop=N4212cop, para=3.1, delv=0.001, delt=0.02)
lines(set$U, set$V, col=2, lwd=2) # manually draw the 23rd percentile
set <- level.setCOP2(cop=N4212cop, para=3.1, getlevel=0.17, lines=TRUE, col=4, lwd=4)
# Notice the change in the lines argument and using levelsetCOP2 to draw.
mtext("Level Curves and Special Level Sets for N4212 copula") #}
}
\keyword{visualization}
\keyword{diagnostics}
\concept{level curves}
\concept{level curve}
\concept{level sets}
\concept{level set}
\concept{level contours}
\concept{level contour}
