\name{derCOP2}
\alias{derCOP2}
\title{ The Numeric Derivative of a Copula }
\description{
Compute the numeric derivative of a copula according to Nelson (2006), but with respect to \eqn{v} and not \eqn{u}
\deqn{\mathrm{Pr}[U \le u\mid V=v] = \lim_{\Delta v \rightarrow 0}\frac{\mathbf{C}(u, v+\Delta v) - \mathbf{C}(u,v)}{\Delta v}}
which is to read at the probability that \eqn{U \le u} given that \eqn{V = v} and corresponds to the \cr \samp{derdir="left"} mode of the function. For \code{derdir="right"}, we have
\deqn{\mathrm{Pr}[U \le u\mid V=v] = \lim_{\Delta v \rightarrow 0}\frac{\mathbf{C}(u,v) - \mathbf{C}(u, v-\Delta v)}{\Delta v}}
and for \code{derdir="center"} (the usual method of computing a derivative), we have
\deqn{\mathrm{Pr}[U \le u\mid V=v] = \lim_{\Delta v \rightarrow 0}\frac{\mathbf{C}(u,v+\Delta v) - \mathbf{C}(u, v-\Delta v)}{2 \Delta v}}
}
\usage{
derCOP2(cop=NULL, u, v, delv=.Machine$double.eps^0.50,
        derdir=c("left","right","center"), ...)
}
\arguments{
  \item{cop}{A copula function,}
  \item{u}{A nonexceedance probability in X direction,}
  \item{v}{A nonexceedance probability in Y direction,}
  \item{delv}{The delta v interval for the derivative,}
  \item{derdir}{The direction of the derivative as described above. Default is left, and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for the copula is returned.
}
\note{
See pp. 13 and 40--41 of the reference. A known weakness of the current implementation of the Copula derivative is that this is a small probability that the \eqn{\Delta v} will span a singularity or discontinuous portion of a copula shown it have that property. The \code{delv} is chosen small to the probability is mitigated to a small value. It is not known whether a derivative from the positive side (\code{dir="left"}), when failing should switch over to a computation from the negative side (\code{dir="right"}). The distinction is important for the computation of the inverse of the derivative \code{\link{derCOPinv2}} because the solution finder needs a sign reversal.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{derCOPinv2}}, \code{\link{W}}, \code{\link{P}}, \code{\link{M}}, \code{\link{PSP}}}
\examples{
derCOP2(cop=W,0.4,0.6)
derCOP2(cop=P,0.4,0.6)
derCOP2(cop=M,0.4,0.6)
derCOP2(cop=PSP,0.4,0.6)

lft <- derCOP2(cop=P,0.4,0.6, derdir="left")
rgt <- derCOP2(cop=P,0.4,0.6, derdir="right")
cnt <- derCOP2(cop=P,0.4,0.6, derdir="center")
\dontrun{stopifnot(all.equal(lft,rgt), all.equal(lft,cnt))}


# Let us contrive a singularity though this NOT A COPULA
# in the function "afunc"
"afunc" <- function(u,v, ...) {
  if(u <= 0.5) return(sqrt(u^2+v^2))
  return(P(u,v,...)) # The product copula
}
lft <- derCOP2(cop=afunc,0.5,runif(1),derdir="left")
# The following is correct for given v.
rgt <- derCOP2(cop=afunc,0.5,runif(1),derdir="right")
cnt <- derCOP2(cop=afunc,0.5,runif(1),derdir="center")
cat(c(lft,rgt,cnt,"\n"))

}
\keyword{distribution}
\keyword{multivariate}
