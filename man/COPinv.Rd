\encoding{utf8}
\name{COPinv}
\alias{COPinv}
\title{ The Inverse of a Copula for V with respect to U }
\description{
Compute the \emph{inverse of a copula} for \eqn{V} with respect to \eqn{U} given \eqn{t} or
\deqn{t = \mathbf{C}(u{=}U,v) \rightarrow \mathbf{C}^{(-1)}(u{=}U, t) = v\mbox{,}}
and solving for \eqn{v}.  Nelsen (2006, p. 12) does not so name this function as an \dQuote{inverse.} The \code{COPinv} function is internally used by \code{\link{level.curvesCOP}}. A common misapplication that will puzzle the user is that the following call and error message are often seen:
\preformatted{
  COPinv(.2, .25, cop=PSP)
  Error in uniroot(func, interval = c(lo, 1), u = u, LHS = t, cop = cop,  :
    f() values at end points not of opposite sign
  [1] NA
}
This is a harmless error in the sense that \code{COPinv} is functioning properly. One can  not invert a copula for \eqn{u < t} and for \eqn{u = t} the \eqn{v = 1} because of fundamental copula properties. Also because of fundamental theory involved, the author perceives that it is better to not silently trap the error but let errors cascade up the chain in a user's application. This might represent suboptimal design and suggestions for improvement are invited.
}
\usage{
COPinv(cop=NULL, u, t, para=NULL, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{t}{Nonexceedance probability level \eqn{t};}
  \item{para}{Vector of parameters or other data structures, if needed, to pass to the copula; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for \eqn{v} are returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{COP}},
         \code{\link{COPinv2}},
         \code{\link{level.curvesCOP}}
}
\examples{
COPinv(cop=N4212cop, para=2, u=0.5, t=0.2)
}
\keyword{copula (utility)}
\keyword{copula (inverse)}
\keyword{copula operator}


