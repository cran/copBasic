\encoding{utf8}
\name{surfuncCOP}
\alias{surfuncCOP}
\title{ The Joint Survival Function }
\description{
Compute the \emph{joint survival function} for a copula (Nelsen, 2006, p. 33), which is defined as
\deqn{\overline{\mathbf{C}}(u,v) = \mathrm{Pr}[U > u, V > v] = 1 - u - v + \mathbf{C}(u,v) = \hat\mathbf{C}(1-u, 1-v)\mbox{,}}
where \eqn{\hat\mathbf{C}(u',v')} is the \emph{survival copula} (see \code{\link{surCOP}}), which is defined by
\deqn{\hat{\mathbf{C}}(u',v') = \mathrm{Pr}[U > u, V > v] =  u' + v' - 1 + \mathbf{C}(1-u',1-v')\mbox{.}}

Although the joint survival function is an expression of the probability that both \eqn{U > v} and \eqn{U > v}, \eqn{\overline{\mathbf{C}}(u,v)} not a copula.
}
\usage{
surfuncCOP(u, v, cop=NULL, para=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula; and}
  \item{...}{Additional arguments to pass (such as parameters, if needed, for the copula in the form of a list.}
}
\value{
  Value(s) for the joint survival function are returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{surCOP}}  }
\examples{
"MOcop.formula" <- function(u,v, para=para, ...) {
   alpha <- para[1]; beta <- para[2]; return(min(v*u^(1-alpha), u*v^(1-beta)))
}
"MOcop" <- function(u,v, ...) { asCOP(u,v, f=MOcop.formula, ...) }
u <- 0.2; v <- 0.75; ab <- c(1.5, 0.3)
# U **and** V are less than or equal to a threshold +
# U **or**  V are less than or equal to a threshold
surCOP(1-u,1-v, cop=MOcop, para=ab) + duCOP(u,v, cop=MOcop, para=ab) # UNITY
surfuncCOP(u,v, cop=MOcop, para=ab) + duCOP(u,v, cop=MOcop, para=ab) # UNITY
\dontrun{
# The joint survival function is not a copula so it does not increases to the
# upper right with increasing exceedance probabilities. Let us show that by
# hacking the surCOP function into a copula for feeding back into the algorithmic
# framework of copBasic.
UsersCop <- function(u,v, para=NULL) {
     afunc <- function(u,v, theta=para) { surfuncCOP(u, v, cop=N4212cop, para=theta)}
     return(asCOP(u,v, f=afunc)) }
image(gridCOP(cop=UsersCop, para=1.15), col=terrain.colors(20),
      xlab="U, NONEXCEEDANCE PROBABILITY", ylab="V, NONEXCEEDANCE PROBABILITY")#}
      
\dontrun{
# Conditional return period (Salvadori et al., 2007, p. 159)
UV <- simCOP(n=100000, cop=PLACKETTcop, para=5, graphics=FALSE)
u <- 0.5; v <- 0.99; cd <- UV$V[UV$U > u]
by.counting <- length(cd[cd > v])/length(cd)                  # 0.0172
by.theo     <- surfuncCOP(u,v, cop=PLACKETTcop, para=5)/(1-u) # 0.0166
by.ec       <- surfuncCOP(u,v, cop=EMPIRcop, para=UV)/(1-u)   # 0.0189
print(1/by.theo) # conditional return period for V > 0.99 given U > 0.5 }
}
\keyword{copula operator}
\keyword{joint probability}
\keyword{conditional return period}
