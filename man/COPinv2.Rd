\name{COPinv2}
\alias{COPinv2}
\title{ The Inverse of a Copula for U with respect to V }
\description{
Compute the \emph{inverse of a copula} for \eqn{U} with respect to \eqn{V} or
\deqn{t = \mathbf{C}(u,v=V)\mbox{,}}
and solving for \eqn{u}.  Nelsen (2006, p. 12) does not so name this function as an \dQuote{inverse.} The \code{COPinv2} function is internally used by \code{\link{level.curvesCOP2}}.
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
\seealso{\code{\link{COP}}, \code{\link{COPinv}}, \code{\link{level.curvesCOP2}}
}
\examples{
# See those for COPinv as they are the same by analogy.
}
\keyword{copula (utility)}
\keyword{copula (inverse)}
\keyword{copula operator}

