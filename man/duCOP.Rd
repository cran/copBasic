\name{duCOP}
\alias{duCOP}
\title{ The Dual of a Copula Function }
\description{
Compute the \emph{dual of a copula (function)} from a copula (Nelson, 2006, pp. 33--34), which is defined as

\deqn{\mathrm{Pr}[U \le v \mathrm{\ or\ } V \le v] = \tilde{\mathbf{C}}(u,v) = u + v - \mathbf{C}(u,v)\mbox{,}}

where \eqn{\tilde{\mathbf{C}}(u,v)} is the dual of a copula and \eqn{u} and \eqn{v} are nonexceedance probabilities. The dual of a copula function is the expression for the probability that either \eqn{U \le u} or \eqn{V \le v}, which is unlike the \emph{co-copula function} (see \code{\link{coCOP}}) that provides \eqn{\mathrm{Pr}[U > u \mathrm{\ or\ } V > v]}. The dual of a copula is a function and not in itself a copula. Some rules of copulas mean that

\deqn{\hat\mathbf{C}(u',v') + \tilde{\mathbf{C}}(u,v) \equiv 1\mbox{,}}

where \eqn{\hat\mathbf{C}(u',v')} is the \emph{survival copula} in terms of exceedance probabilities \eqn{u'} and \eqn{v'} or in \pkg{copBasic} code that the functions \code{surCOP} + \code{duCOP} equal unity.
}
\usage{
duCOP(u, v, cop=NULL, para=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula; and}
  \item{...}{Additional arguments to pass (such as parameters, if needed, for the copula in the form of a list.}
}
\value{
  Value(s) for the dual of a copula are returned.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{coCOP}}, \code{\link{surCOP}} }
\examples{
u <- runif(1); t <- runif(1)
duCOP(cop=W,u,t); duCOP(cop=P,u,t); duCOP(cop=M,u,t); duCOP(cop=PSP,u,t)

# Next demonstrate COP + duCOP = unity.
"MOcop.formula" <- function(u,v, para=para, ...) {
   alpha <- para[1]; beta <- para[2]; return(min(v*u^(1-alpha), u*v^(1-beta)))
}
"MOcop" <- function(u,v, ...) { asCOP(u,v, f=MOcop.formula, ...) }
u <- 0.2; v <- 0.75; ab <- c(1.5, 0.3)
surCOP(1-u,1-v, cop=MOcop, para=ab) + duCOP(u,v, cop=MOcop, para=ab) # UNITY
}
\keyword{copula operator}

