\encoding{utf8}
\name{derCOP}
\alias{derCOP}
\title{ Numerical Derivative of a Copula for V with respect to U }
\description{
Compute the numerical partial derivative of a copula, which is a \emph{conditional distribution function}, according to Nelsen (2006, pp. 13, 40--41) with respect to \eqn{u}:

\deqn{0 \le \frac{\delta}{\delta u} \mathbf{C}(u,v) \le 1\mbox{,}}

or

\deqn{\mathrm{Pr}[V \le v\mid U=u] = \mathbf{C}_{2|1}(v|u) = \lim_{\Delta u \rightarrow 0}\frac{\mathbf{C}(u+\Delta u, v) - \mathbf{C}(u,v)}{\Delta u}\mbox{,}}

which is to read as the probability that \eqn{V \le v} given that \eqn{U = u} and corresponds to the \cr \code{derdir="left"} mode of the function. For \code{derdir="right"}, we have
\deqn{\mathrm{Pr}[V \le v\mid U=u] = \lim_{\Delta u \rightarrow 0}\frac{\mathbf{C}(u,v) - \mathbf{C}(u-\Delta u, v)}{\Delta u}\mbox{,}}
and for \code{derdir="center"} (the usual method of computing a derivative), the following results
\deqn{\mathrm{Pr}[V \le v\mid U=u] = \lim_{\Delta u \rightarrow 0}\frac{\mathbf{C}(u+\Delta u,v) - \mathbf{C}(u-\Delta u, v)}{2 \Delta u}\mbox{.}}
The \dQuote{\emph{with respect to \eqn{V}}} versions are available under \code{\link{derCOP2}}.

Copula derivatives (\eqn{\delta \mathbf{C}/\delta u} or say  \eqn{\delta \mathbf{C}/\delta v} \code{\link{derCOP2}}) are non-decreasing functions meaning that if \eqn{v_1 \le v_2}, then \eqn{\mathbf{C}(u, v_2) - \mathbf{C}(u,v_1)} is a non-decreasing function in \eqn{u}, thus
\deqn{\frac{\delta\bigl(\mathbf{C}(u, v_2) - \mathbf{C}(u,v_1)\bigr)}{\delta u}}
is non-negative, which means
\deqn{\frac{\delta\mathbf{C}(u, v_2)}{\delta u} \ge \frac{\delta\mathbf{C}(u, v_1)}{\delta u}\mbox{\ for\ } v_2 \ge v_1\mbox{.}}
}
\usage{
derCOP(cop=NULL, u, v, delu=.Machine$double.eps^0.50,
       derdir=c("left", "right", "center"), ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction. If the length of \code{u} is unity, then the length of \code{v} can be arbitrarily long. If the length of \code{u} is not unity, then the length of \code{v} should be the same, and if not, then  only the first value in \code{v} is silently used;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction (see previous comment on \code{u});}
  \item{delu}{The \eqn{\Delta u} interval for the derivative;}
  \item{derdir}{The direction of the derivative as described above. Default is \code{left} but internally any setting can be temporarily suspended to avoid improper computations (see source code); and}
  \item{...}{Additional arguments to pass such as the parameters often described in \code{para} arguments of other copula functions. (The lack of \code{para=NULL} for \code{derCOP} and \code{\link{derCOP2}} was either design oversight or design foresight but regardless it is too late to enforce package consistency in this matter.)}
}
\value{
  Value(s) for the partial derivative are returned.
}
\note{
A known caveat of the current implementation of the copula derivative is that there is a chance that the \eqn{\Delta u} will span a singularity or discontinuous (or nearly so) portion of a copula should it have a property of singularity (or nearly so). The \code{delu} is chosen small so the chance is mitigated to a small value. It is not decided whether a derivative from the positive side (\code{dir="left"}), when failing should switch over to a computation from the negative side (\code{dir="right"}). The distinction is important for the computation of the inverse of the derivative \code{\link{derCOPinv}} because the solution finder needs a sign reversal to work.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{derCOPinv}}, \code{\link{derCOP2}}}
\examples{
derCOP(cop=W,0.4,0.6); derCOP(cop=P,0.4,0.6); derCOP(cop=M,0.4,0.6)

lft <- derCOP(cop=PSP,   0.4, 0.6, derdir="left"  )
rgt <- derCOP(cop=PSP,   0.4, 0.6, derdir="right" )
cnt <- derCOP(cop=PSP,   0.4, 0.6, derdir="center")
cat(c(lft,rgt,cnt,"\n"))
#stopifnot(all.equal(lft,rgt), all.equal(lft,cnt))

# Let us contrive a singularity through this NOT A COPULA in the function "afunc".
"afunc" <- function(u,v, ...) return(ifelse(u <= 0.5, sqrt(u^2+v^2), P(u,v,...)))
lft <- derCOP(cop=afunc, 0.5, 0.67, derdir="left"  )
rgt <- derCOP(cop=afunc, 0.5, 0.67, derdir="right" )
cnt <- derCOP(cop=afunc, 0.5, 0.67, derdir="center")
cat(c(lft,rgt,cnt,"\n")) # The "right" version is correct.
}
\keyword{copula (utility)}
\keyword{copula (derivative)}
\keyword{conditional cumulative distribution function}
\keyword{copula operator}
\keyword{derivative}
\keyword{copula (conditional distribution)}

