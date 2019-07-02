\encoding{utf8}
\name{tailconCOP}
\alias{tailconCOP}
\title{ The Tail Concentration Function of a Copula }
\description{
Compute the \emph{tail concentration function} (\eqn{q_\mathbf{C}}) of a copula \eqn{\mathbf{C}(u,v)} (\code{\link{COP}}) or diagnonal (\code{\link{diagCOP}}) of a copula \eqn{\delta_\mathbf{C}(t) = \mathbf{C}(t,t)}  according to Durante and Semp (2015, p. 74):
\deqn{q_\mathbf{C}(t) = \frac{\mathbf{C}(t,t)}{t} \cdot \mathbf{1}_{[0,0.5)}  + \frac{1 - 2t + \mathbf{C}(t,t)}{1-t} \cdot \mathbf{1}_{[0.5, 1]}\mbox{\quad or}}
\deqn{q_\mathbf{C}(t) = \frac{\delta_\mathbf{C}(t)}{t} \cdot \mathbf{1}_{[0,0.5)} + \frac{1 - 2t + \delta_\mathbf{C}(t)}{1-t}   \cdot \mathbf{1}_{[0.5, 1]}\mbox{,}}
where \eqn{t} is a nonexceedance probability on the margins and \eqn{\mathbf{1}(.)} is an \emph{indicator function} scoring 1 if condition is true otherwise zero on what interval \eqn{t} resides: \eqn{t \in [0,0.5)} or \eqn{t \in [0.5,1]}. The \eqn{q_\mathbf{C}(t; \mathbf{M}) = 1} for all \eqn{t} for the \code{\link{M}} copula and  \eqn{q_\mathbf{C}(t; \mathbf{W}) = 0} for all \eqn{t} for the \code{\link{W}} copula. Lastly, the function is related to the \emph{Blomqvist Beta} (\eqn{\beta_\mathbf{C}}; \code{\link{blomCOP}}) by
\deqn{q_\mathbf{C}(0.5) = (1 + \beta_\mathbf{C})/2\mbox{,}}
where \eqn{\beta_\mathbf{C} = 4\mathbf{C}(0.5, 0.5) - 1}. Lastly, the \eqn{q_\mathbf{C}(t)} for \eqn{0,1 = t} is \code{NaN} and no provision for alternative return is made.
}
\usage{
tailconCOP(t, cop=NULL, para=NULL, ...)
}
\arguments{
  \item{t}{Nonexceedance probabilities \eqn{t};}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula; and}
  \item{...}{Additional arguments to pass to the copula function.}
}
\value{
  Value(s) for \eqn{q_\mathbf{C}} are returned.
}
\references{
Durante, F., and Sempi, C., 2015, Principles of copula theory: Boca Raton, CRC Press, 315 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{taildepCOP}}, \code{\link{tailordCOP}}}
\examples{
tailconCOP(0.5, cop=PSP) == (1+blomCOP(cop=PSP))/2 # TRUE
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{copula (tail characteristics)}
\keyword{copula (tail properties)}

