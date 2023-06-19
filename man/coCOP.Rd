\encoding{utf8}
\name{coCOP}
\alias{coCOP}
\title{The Co-Copula Function}
\description{
Compute the \emph{co-copula (function)} from a copula (Nelsen, 2006, pp. 33--34), which is defined as

\deqn{\mathrm{Pr}[U > u \mathrm{\ or\ } V > v] = \mathbf{C}^{\star}(u',v') = 1 - \mathbf{C}(u',v')\mbox{,}}

where \eqn{\mathbf{C}^{\star}(u',v')} is the co-copula and \eqn{u'} and \eqn{v'} are exceedance probabilities and are equivalent to \eqn{1-u} and \eqn{1-v} respectively. The co-copula is the expression for the probability that either \eqn{U > u} \bold{or} \eqn{V > v} when the arguments to \eqn{\mathbf{C}^{\star}(u',v')} are exceedance probabilities, which is unlike the \emph{dual of a copula (function)} (see \code{\link{duCOP}}) that provides \eqn{\mathrm{Pr}[U \le u \mathrm{\ or\ } V \le v]}.

The co-copula is a function and not in itself a copula. Some rules of copulas mean that \eqn{\mathbf{C}(u,v) + \mathbf{C}^{\star}(u',v') \equiv 1} or in \pkg{copBasic} syntax that the functions \code{COP(u,v)} + \code{coCOP(u,v)} equal unity if the \code{exceedance} argument to \code{coCOP} is set to \code{FALSE}.

The function \code{coCOP} gives \dQuote{risk} against failure if failure is defined as either hazard source \eqn{U} or \eqn{V} occuring by themselves or if both occurred at the same time. Expressing this in terms of an annual probability of occurrence (\eqn{q}), one has
\deqn{q = 1 - \mathrm{Pr}[U > u \mathrm{\ or\ } V > v] =  \mathbf{C}^{\star}(u',v') \mbox{\ or}}
in \R code \code{q <- coCOP(u,v, exceedance=FALSE, ...)}. So, in yet other words and as a mnemonic: \emph{A co-copula is the probabililty of exceedance if the hazard sources \bold{collaborate} or \bold{cooperate} to cause failure.} Also, \eqn{q} can be computed by \code{q <- }\code{coCOP(1 - u, 1 - v,} \code{exceedance=TRUE, ...)}.
}
\usage{
coCOP(u, v, cop=NULL, para=NULL, exceedance=TRUE, ...)
}
\arguments{
  \item{u}{Exceedance probability (\eqn{u' = 1-u}) in the \eqn{X} direction;}
  \item{v}{Exceedance probability (\eqn{v' = 1-v}) in the \eqn{Y} direction;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{exceedance}{A logical controlling the probability direction. Are \code{u} and \code{v} values given really \eqn{u'} and \eqn{v'}, respectively? If \code{FALSE}, then the complements of the two are made internally and the nonexceedances can thus be passed; and}
  \item{...}{Additional arguments to pass to the copula.}
}
\value{
  The value(s) for the co-copula are returned.
}
\note{
The author (Asquith) finds the use of exceedance probabilities delicate in regards to Nelsen's notation. The \code{coCOP} function and \code{\link{surCOP}} have the \code{exceedance} argument to serve as a reminder that the co-copula as defined in the literature uses \emph{exceedance probabilities} as its arguments, although the arguments as code \code{u} and \code{v} do not mimic the overline nomenclature (\eqn{\,\overline{\cdots}\,}) of the exceedance (survival) probabilities.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{surCOP}}, \code{\link{duCOP}}}
\examples{
u <- 1 - runif(1); v <- 1 - runif(1) # as exceedance, in order to reinforce the
# change to exceedance instead of nonexceedance that otherwise dominates this package
message("Exceedance probabilities u' and v' are ", u, " and ", v)
coCOP(u,v,cop=PLACKETTcop, para=10) # Positive association Plackett

# computation using  manual  manipulation to nonexceedance probability
1 - COP(cop=PSP,(1-u),(1-v))
# computation using internal manipulation to nonexceedance probability
  coCOP(cop=PSP,   u,    v)

# Next demonstrate COP + coCOP = unity.
"MOcop.formula" <- function(u,v, para=para, ...) { # Marshall-Olkin copula
   alpha <- para[1]; beta <- para[2]; return(min(v*u^(1-alpha), u*v^(1-beta)))
}
"MOcop" <- function(u,v, ...) { asCOP( u,  v, f=MOcop.formula, ...) }
u <- 0.2; v <- 0.75; ab <- c(1.5, 0.3)
COP(u,v, cop=MOcop, para=ab) + coCOP(1-u,1-v, cop=MOcop, para=ab) # UNITY
}
\keyword{copula operator}
\keyword{joint probability}
