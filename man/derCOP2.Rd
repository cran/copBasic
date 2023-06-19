\encoding{utf8}
\name{derCOP2}
\alias{derCOP2}
\title{Numerical Derivative of a Copula for U with respect to V}
\description{
Compute the numerical partial derivative of a copula, which is a \emph{conditional distribution function}, according to Nelsen (2006, pp. 13, 40--41) with respect to \eqn{v}:

\deqn{0 \le \frac{\delta}{\delta v} \mathbf{C}(u,v) \le 1\mbox{,}}

or

\deqn{\mathrm{Pr}[U \le u\mid V=v] = \mathbf{C}_{1|2}(u|v) =  \lim_{\Delta v \rightarrow 0}\frac{\mathbf{C}(u, v+\Delta v) - \mathbf{C}(u,v)}{\Delta v}\mbox{,}}

which is to read as the probability that \eqn{U \le u} given that \eqn{V = v} and corresponds to the \cr \code{derdir="left"} mode of the function. For \code{derdir="right"}, the following results
\deqn{\mathrm{Pr}[U \le u\mid V=v] = \lim_{\Delta v \rightarrow 0}\frac{\mathbf{C}(u,v) - \mathbf{C}(u, v-\Delta v)}{\Delta v}\mbox{,}}
and for \code{derdir="center"} (the usual method of computing a derivative), the following results
\deqn{\mathrm{Pr}[U \le u\mid V=v] = \lim_{\Delta v \rightarrow 0}\frac{\mathbf{C}(u,v+\Delta v) - \mathbf{C}(u, v-\Delta v)}{2 \Delta v}\mbox{.}}
The \dQuote{\emph{with respect to \eqn{U}}} versions are under \code{\link{derCOP}}.

Copula derivatives (\eqn{\delta \mathbf{C}/\delta v} or say  \eqn{\delta \mathbf{C}/\delta u} \code{\link{derCOP}}) are non-decreasing functions meaning that if \eqn{u_1 \le u_2}, then \eqn{\mathbf{C}(u_2, v) - \mathbf{C}(u_1,v)} is a non-decreasing function in \eqn{v}, thus
\deqn{\frac{\delta\bigl(\mathbf{C}(u_2, v) - \mathbf{C}(u_1,v)\bigr)}{\delta v}}
is non-negative, which means
\deqn{\frac{\delta\mathbf{C}(u_2, v)}{\delta v} \ge \frac{\delta\mathbf{C}(u_1, v)}{\delta v}\mbox{\ for\ } u_2 \ge u_1\mbox{.}}
}
\usage{
derCOP2(cop=NULL, u, v, delv=.Machine$double.eps^0.50,
        derdir=c("left", "right", "center"), ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction. If the length of \code{u} is unity, then the length of \code{v} can be arbitrarily long. If the length of \code{u} is not unity, then the length of \code{v} should be the same and if not only the first value in \code{v} will be silently used;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction (see previous comment on \code{u});}
  \item{delv}{The \eqn{\Delta v} interval for the derivative;}
  \item{derdir}{The direction of the derivative as described above. Default is \code{left} but internally any setting can be temporarily suspended to avoid improper computations (see source code); and}
  \item{...}{Additional arguments to pass such as the parameters often described in \code{para} arguments of other copula functions. (The lack of \code{para=NULL} for \code{\link{derCOP}} and \code{derCOP2} was either design oversight or design foresight but regardless it is too late to enforce package consistency in this matter.)}
}
\value{
  Value(s) for the partial derivative are returned.
}
\note{
A known caveat of the current implementation of the copula derivative is that there is a chance that the \eqn{\Delta v} will span a singularity or discontinuous (or nearly so) portion of a copula should it have a property of singularity (or nearly so). The \code{delv} is chosen small so the chance is mitigated to be a small change and certainly seems to work throughout the examples herein. It is not decided whether a derivative from the positive side (\code{dir="left"}), when failing should switch over to a computation from the negative side (\code{dir="right"}). The distinction is important for the computation of the inverse of the derivative \code{\link{derCOPinv2}} because the solution finder needs a sign reversal to work.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{derCOPinv2}}, \code{\link{derCOP}}}
\examples{
derCOP2(cop=W, 0.4, 0.6); derCOP2(cop=P, 0.4, 0.6); derCOP2(cop=M, 0.4, 0.6)

lft <- derCOP2(cop=P,     0.4,  0.6, derdir="left"  )
rgt <- derCOP2(cop=P,     0.4,  0.6, derdir="right" )
cnt <- derCOP2(cop=P,     0.4,  0.6, derdir="center")
cat(c(lft, rgt, cnt,"\n"))
# stopifnot(all.equal(lft, rgt), all.equal(lft, cnt))

# Let us contrive a singularity though this NOT A COPULA in the function "afunc".
"afunc" <- function(u,v, ...) return(ifelse(u <= 0.5, sqrt(u^2+v^2), P(u,v,...)))
lft <- derCOP2(cop=afunc, 0.67, 0.5, derdir="left"  )
rgt <- derCOP2(cop=afunc, 0.67, 0.5, derdir="right" )
cnt <- derCOP2(cop=afunc, 0.67, 0.5, derdir="center")
cat(c(lft,rgt,cnt,"\n")) # For this example, all are correct (see derCOP examples)
}
\keyword{copula (utility)}
\keyword{copula (derivative)}
\keyword{conditional cumulative distribution function}
\keyword{copula operator}
\keyword{derivative}
\keyword{copula (conditional distribution)}
