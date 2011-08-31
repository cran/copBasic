\name{derCOP}
\alias{derCOP}
\title{ The Numeric Derivative of a Copula }
\description{
Compute the numeric derivative of a copula according to Nelson (2006)
\deqn{\mathrm{Pr}[V \le v\mid U=u] = \lim_{\Delta u \rightarrow 0}\frac{\mathbf{C}(u+\Delta u, v) - \mathbf{C}(u,v)}{\Delta u}}
which is to read at the probability that \eqn{V \le v} given that \eqn{U = u} and corresponds to the \\ \code{derdir="left"} mode of the function. For \samp{derdir="right"}, we have
\deqn{\mathrm{Pr}[V \le v\mid U=u] = \lim_{\Delta u \rightarrow 0}\frac{\mathbf{C}(u,v) - \mathbf{C}(u-\Delta u, v)}{\Delta u}}
and for \code{derdir="center"} (the usual method of computing a derivative), we have
\deqn{\mathrm{Pr}[V \le v\mid U=u] = \lim_{\Delta u \rightarrow 0}\frac{\mathbf{C}(u+\Delta u,v) - \mathbf{C}(u-\Delta u, v)}{2 \Delta u}}
}
\usage{
derCOP(cop=NULL, u, v, delu=.Machine$double.eps^0.50,
       derdir=c("left","right","center"), ...)
}
\arguments{
  \item{cop}{A copula function,}
  \item{u}{A nonexceedance probability in X direction,}
  \item{v}{A nonexceedance probability in Y direction,}
  \item{delu}{The delta u interval for the derivative,}
  \item{derdir}{The direction of the derivative as described above. Default is left, and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for the copula is returned.
}
\note{
See pp. 13 and 40--41 of the reference. A known weakness of the current implementation of the Copula derivative is that this is a small probability that the \eqn{\Delta u} will span a singularity or discontinuous portion of a copula shown it have that property. The \code{delu} is chosen small to the probability is mitigated to a small value. It is not known whether a derivative from the positive side (\code{dir="left"}), when failing should switch over to a computation from the negative side (\code{dir="right"}). The distinction is important for the computation of the inverse of the derivative \code{\link{derCOPinv}} because the solution finder needs a sign reversal.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{derCOPinv}}, \code{\link{W}}, \code{\link{P}}, \code{\link{M}}, \code{\link{PSP}}}
\examples{
derCOP(cop=W,0.4,0.6)
derCOP(cop=P,0.4,0.6)
derCOP(cop=M,0.4,0.6)
derCOP(cop=PSP,0.4,0.6)

lft <- derCOP(cop=P,0.4,0.6,derdir="left")
rgt <- derCOP(cop=P,0.4,0.6,derdir="right")
cnt <- derCOP(cop=P,0.4,0.6,derdir="center")
\dontrun{stopifnot(all.equal(lft,rgt), all.equal(lft,cnt))}


# Let us contrive a singularity through this NOT A COPULA
# in the function "afunc"
"afunc" <- function(u,v, ...) {
  if(u <= 0.5) return(sqrt(u^2+v^2))
  return(P(u,v,...)) # The product copula
}
lft <- derCOP(cop=afunc,0.5,runif(1),derdir="left")
# The following is correct for given v.
rgt <- derCOP(cop=afunc,0.5,runif(1),derdir="right")
cnt <- derCOP(cop=afunc,0.5,runif(1),derdir="center")
cat(c(lft,rgt,cnt,"\n"))

}
\keyword{distribution}
\keyword{multivariate}
