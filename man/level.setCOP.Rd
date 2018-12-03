\encoding{utf8}
\name{level.setCOP}
\alias{level.setCOP}
\title{ Compute a Level Set of a Copula V with respect to U }
\description{
Compute a \emph{level curve} or \emph{level set} of a copula for \eqn{V} with respect to \eqn{U} (Nelsen, 2006, pp. 12--13). The level curve at level \eqn{t} is defined for \eqn{U \mapsto [0+\Delta u, 1-\Delta u, \Delta u]} by
\deqn{t \mapsto \mathbf{C}(u{=}U, v)\mbox{,}}
and solving for \eqn{v}. The function is largely a dispatcher to features implemented in \code{\link{level.curvesCOP}}.
}
\usage{
level.setCOP(cop=NULL, para=NULL, getlevel=NULL, delu=0.001, lines=FALSE, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{getlevel}{The level set for \eqn{t};}
  \item{delu}{The increment for \eqn{\Delta u}. The default is 1 part in 1,000, which should often provide enough smoothness for many copulas in practice;}
  \item{lines}{A logical that matches the argument of the same name in \code{\link{level.curvesCOP}}; and}
  \item{...}{Additional arguments to pass to the \code{lines()} function in \R.}
}
\value{
  The level set for \eqn{t} \eqn{=} \code{getlevel} is returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{level.setCOP2}}, \code{\link{level.curvesCOP}}
}
\examples{
\dontrun{
set <- level.setCOP(cop=PSP, getlevel=0.23, delu=0.005)
level.curvesCOP(cop=PSP)
lines(set$U, set$V, col=2, lwd=2) # manually draw the 23rd percentile
set <- level.setCOP(cop=PSP, para=3.1, getlevel=0.67, lines=TRUE, col=4, lwd=4)
# Notice the change in the lines argument and using levelsetCOP2 to draw.
mtext("Level Curves and Special Level Sets for PSP copula") # }
}
\keyword{visualization}
\keyword{diagnostics}
\concept{level curves}
\concept{level curve}
\concept{level sets}
\concept{level set}
\concept{level contours}
\concept{level contour}
