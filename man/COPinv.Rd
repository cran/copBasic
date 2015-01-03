\name{COPinv}
\alias{COPinv}
\title{ The Inverse of a Copula for V with respect to U }
\description{
Compute the \emph{inverse of a copula} for \eqn{V} with respect to \eqn{U} or
\deqn{t = \mathbf{C}(u=U,v)\mbox{,}}
and solving for \eqn{v}.  Nelsen (2006, p. 12) does not so name this function as an \dQuote{inverse.} The \code{COPinv} function is internally used by \code{\link{level.curvesCOP}}.
}
\usage{
COPinv(cop=NULL, u, t, para=NULL, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{t}{Nonexceedance probability level \eqn{t};}
  \item{para}{Vector of parameters or other data structures, if needed, to pass to the copula, and}
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


