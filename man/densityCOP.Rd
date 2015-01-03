\encoding{utf8}
\name{densityCOP}
\alias{densityCOP}
\title{ Density of a Copula }
\description{
The function numerically estimates the \emph{copula density} for a sequence of \eqn{u} and \eqn{v} probabilities for which the sequence has a step of \eqn{\Delta(uv)}. The density \eqn{c(u,v)} of a copula \eqn{\mathbf{C}(u,v)} can be computed by
\deqn{c(u,v) = [\mathbf{C}(u_2,v_2) - \mathbf{C}(u_2,v_1) - \mathbf{C}(u_1,v_2) + \mathbf{C}(u_1,v_1)]/[\Delta(uv)\times\Delta(uv)]\mbox{,}}
where \eqn{c(u,v) \ge 0} (see Nelsen (2006, p. 10)).
}
\usage{
densityCOP(u,v, cop=NULL, para=NULL, deluv=.Machine$double.eps^0.25, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{deluv}{The change in \eqn{\Delta(uv)} probability; and}
  \item{...}{Additional arguments to pass to the copula function.}
}
\value{
  Value(s) for \eqn{c(u,v)} are returned.
}
\references{
Joe, H., 2015, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{simCOP}}, \code{\link{densityCOPplot}}, \code{\link{kullCOP}}}
\examples{
# Joe (2015, p. 164) shows the closed form copula density for the Plackett.
"dPLACKETTcop" <- function(u,v,para) {
   uv <- u*v; upv <- u + v; eta <- para - 1
   A <- para*(1+eta*(upv - 2*uv)); B <- ((1+eta*upv)^2 - 4*para*eta*uv)^(3/2)
   return(A/B)
}
dPLACKETTcop(0.32, 0.74, para=1.3)                # 0.9557124
densityCOP(0.32, 0.74, cop=PLACKETTcop, para=1.3) # 0.9557153


# Joe (2015, p. 170) shows the closed form copula density for "Bivariate Joe/B5" copula
"JOEB5cop" <- function(u,v,para) {
   up <- (1-u)^para; vp <- (1-v)^para
   return(1 - (up + vp - up*vp)^(1/para))
}
"dJOEB5cop" <- function(u,v,para) {
   up <- (1-u)^para; vp <- (1-v)^para; eta <- para - 1
   A <- (up + vp - up*vp); B <- (1-u)^eta * (1-v)^eta; C <- (eta + A)
   return(A^(-2 + 1/para) * B * C)
}
densityCOP(0.32, 0.74, cop=JOEB5cop, para=1.3) # 0.9410653
dJOEB5cop(0.32, 0.74, para=1.3)                # 0.9410973
}
\keyword{visualization}
\keyword{copula (density)}
\keyword{Joe (2015) Examples and Exercises}


