\encoding{utf8}
\name{COPinv2}
\alias{COPinv2}
\title{The Inverse of a Copula for U with respect to V}
\description{
Compute the \emph{inverse of a copula} for \eqn{U} with respect to \eqn{V} given \eqn{t} or
\deqn{t = \mathbf{C}(u,v{=}V) \rightarrow \mathbf{C}^{(-1)}(v{=}V, t) = u\mbox{,}}
and solving for \eqn{u}.  Nelsen (2006, p. 12) does not so name this function as an \dQuote{inverse.} The \code{COPinv2} function is internally used by \code{\link{level.curvesCOP2}}. A common misapplication that will puzzle the user (including the developer after long breaks from package use) is that the following call and error message are often seen:
\preformatted{
  COPinv2(.2, .25, cop=PSP)
  Error in uniroot(func, interval = c(lo, 1), u = u, LHS = t, cop = cop,  :
    f() values at end points not of opposite sign
  [1] NA
}
This is a harmless error in the sense that \code{COPinv2} is functioning properly. One can  not invert a copula for \eqn{v < t} and for \eqn{v = t} the \eqn{u = 1} because of fundamental copula properties. Also because of fundamental theory being involved, the author perceives that it is better to not silently trap the error but let errors cascade up the chain in a user's application.  This might represent suboptimal design and suggestions for improvement are invited.
}
\usage{
COPinv2(cop=NULL, v, t, para=NULL, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{t}{Nonexceedance probability in \eqn{t};}
  \item{para}{Vector of parameters or other data structures, if needed, to pass to the copula; and}
  \item{...}{Additional arguments to pass to the copula.}
}
\value{
  Value(s) for \eqn{u} are returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{COPinv}}, \code{\link{level.curvesCOP}}, \code{\link{level.curvesCOP2}}
}
\examples{
# See those for COPinv as they are the same by analogy.
}
\keyword{copula (utility)}
\keyword{copula (inverse)}
\keyword{copula operator}

