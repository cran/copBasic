\encoding{utf8}
\name{surCOP}
\alias{surCOP}
\title{The Survival Copula}
\description{
Compute the \emph{survival copula} from a copula (Nelsen, 2006, pp. 32--34), which is defined as

\deqn{\hat{\mathbf{C}}(1-u,1-v) = \hat{\mathbf{C}}(u',v') = \mathrm{Pr}[U > u, V > v] = u' + v' - 1 + \mathbf{C}(1-u', 1-v')\mbox{,}}
where \eqn{u'} and \eqn{v'} are exceedance probabilities and \eqn{\mathbf{C}(u,v)} is the copula (\code{\link{COP}}). The \emph{survivial copula} is a reflection of both \eqn{U} and \eqn{V}.

The \emph{survival copula} is an expression of the joint probability that both \eqn{U > v} and \eqn{U > v} when the arguments \eqn{a} and \eqn{b} to \eqn{\hat{\mathbf{C}}(a,b)} are exceedance probabilities as shown. This is unlike a copula that has \eqn{U \le u} and \eqn{V \le v} for nonexceedance probabilities \eqn{u} and \eqn{v}. Alternatively, the joint probability that both \eqn{U > u} and \eqn{V > v} can be solved using just the copula \eqn{1 - u - v + \mathbf{C}(u,v)}, as shown below where the arguments to \eqn{\mathbf{C}(u,v)} are nonexceedance probabilities. The later formula is the \emph{joint survival function} \eqn{\overline{\mathbf{C}}(u,v)} (\code{\link{surfuncCOP}}) defined for a copula (Nelsen, 2006, p. 33) as
\deqn{\overline{\mathbf{C}}(u,v) = \mathrm{Pr}[U > u, V > v] = 1 - u - v + \mathbf{C}(u,v)\mbox{.}}
Users are directed to the collective documentation in \code{\link{COP}} and \code{\link{simCOPmicro}} for more details on copula reflection.
}
\usage{
surCOP(u, v, cop=NULL, para=NULL, exceedance=TRUE, ...)
}
\arguments{
  \item{u}{Exceedance probability \eqn{u' = 1 - u} (\eqn{u} nonexceedance based on \code{exceedance}) in the \eqn{X} direction;}
  \item{v}{Exceedance probability \eqn{v' = 1 - v} (\eqn{v} nonexceedance based on \code{exceedance}) in the \eqn{Y} direction;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{exceedance}{A logical affirming whether \code{u} and \code{v} are really in exceedance probability or not? If \code{FALSE}, then the complements of the two are made internally and the nonexceedances can thus be passed; and}
  \item{...}{Additional arguments to pass (such as parameters, if needed, for the copula in the form of an \R \code{list}).}
}
\value{
  Value(s) for the survival copula are returned.
}
\note{
The author (Asquith) finds the use of exceedance probabilities delicate in regards to Nelsen's notation. This function and \code{\link{coCOP}} have the \code{exceedance} argument to serve as a reminder that the survival copula as usually defined uses exceedance probabilities as its arguments.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{coCOP}}, \code{\link{duCOP}}, \code{\link{surfuncCOP}}, \code{\link{simCOPmicro}}}
\examples{
u  <-  0.26; v  <- 0.55   # nonexceedance probabilities
up <- 1 - u; vp <- 1 - v  #    exceedance probabilities
surCOP(up, vp,   cop=PSP, exceedance=TRUE)  # 0.4043928
surCOP(u, v,     cop=PSP, exceedance=FALSE) # 0.4043928 (same because of symmetry)
surfuncCOP(u, v, cop=PSP)                   # 0.4043928
# All three examples show joint prob. that U > u and V > v.

\dontrun{
# A survival copula is a copula so it increases to the upper right with increasing
# exceedance probabilities. Let us show that by hacking the surCOP function into
# a copula for feeding back into the algorithmic framework of copBasic.
UsersCop <- function(u,v, para=NULL) {
     afunc <- function(u,v, theta=para) { surCOP(u, v, cop=N4212cop, para=theta)}
     return(asCOP(u,v, f=afunc)) }
image(gridCOP(cop=UsersCop, para=1.15), col=terrain.colors(20),
      xlab="U, EXCEEDANCE PROBABILITY", ylab="V, EXCEEDANCE PROBABILITY") #}
}
\keyword{copula operator}
\keyword{joint probability}
\keyword{copula (reflection)}

