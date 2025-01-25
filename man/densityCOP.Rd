\encoding{utf8}
\name{densityCOP}
\alias{densityCOP}
\title{Density of a Copula}
\description{
Numerically estimate the \emph{copula density} for a sequence of \eqn{u} and \eqn{v} probabilities for which each sequence has equal steps that are equal to \eqn{\Delta(uv)}. The density \eqn{c(u,v)} of a copula \eqn{\mathbf{C}(u,v)} is numerically estimated by
\deqn{c(u,v) = \bigl[\mathbf{C}(u_2,v_2) - \mathbf{C}(u_2,v_1) - \mathbf{C}(u_1,v_2) + \mathbf{C}(u_1,v_1)\bigr]\, /\, \bigl[\Delta(uv)\times\Delta(uv)\bigr]\mbox{,}}
where \eqn{c(u,v) \ge 0} (see Nelsen, 2006, p. 10; \code{\link{densityCOPplot}}). The \emph{joint density} can be defined by the coupla density for continuous variables and is the ratio of the joint density funcion \eqn{f(x,y)} for random variables \eqn{X} and \eqn{Y} to the product of the marginal densities (\eqn{f_x(x)} and \eqn{f_y(y)}):

\deqn{c\bigl(F_x(x), F_y(y)\bigr) = \frac{f(x,y)}{f_x(x)f_y(y)}\mbox{,}}

where \eqn{F_x(x)} and  \eqn{F_y(y)} are the respective cumulative distribution functions of \eqn{X} and \eqn{Y}, and lastly \eqn{u = F_x(x)} and \eqn{v = F_y(y)}.
}
\usage{
densityCOP(u,v, cop=NULL, para=NULL, deluv=.Machine$double.eps^0.25,
                truncate.at.zero=TRUE, the.zero=0, sumlogs=FALSE, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{deluv}{The change in the sequences \eqn{\{u, v\} \mapsto \delta, \ldots, 1-\delta; \delta = \Delta(uv)} probabilities;}
  \item{truncate.at.zero}{A density must be \eqn{c(u,v) \ge 0}, but because this computation is based on a rectangular approximation and not analytical, there exists a possibility that very small rectangles could result in numerical values in \R that are less than zero. This usually can be blamed on rounding. This logical if \code{TRUE} truncates computed densities to zero, and the default assumes that the user is providing a proper copula. A \code{FALSE} value is used by the function \code{\link{isfuncCOP}};}
  \item{the.zero}{The value for \dQuote{the zero} where a small number might be useful for pseudo-maximum likelihood estimation using \code{sumlogs};}
  \item{sumlogs}{Return the \eqn{\sum{\log c(u,v; \Theta)}} where \eqn{\Theta} are the parameters in \code{para} and this feature is provided for \code{\link{mleCOP}}; and}
  \item{...}{Additional arguments to pass to the copula function.}
}
\value{
  Value(s) for \eqn{c(u,v)} are returned.
}
\note{
The \pkg{copBasic} package does not currently have copula densities as analytical solutions implemented. This is because initial design decisions were entirely on cumulative distribution function (CDF) representations of the copula.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{simCOP}}, \code{\link{densityCOPplot}}, \code{\link{kullCOP}}, \code{\link{mleCOP}}}
\examples{
\dontrun{
  # Joe (2014, p. 164) shows the closed form copula density for the Plackett.
  "dPLACKETTcop" <- function(u,v,para) {
     uv <- u*v; upv <- u + v; eta <- para - 1
     A <- para*(1+eta*(upv - 2*uv)); B <- ((1+eta*upv)^2 - 4*para*eta*uv)^(3/2)
     return(A/B)
  }
  dPLACKETTcop(0.32, 0.74,            para=1.3) # 0.9557124
  densityCOP(  0.32, 0.74, cop=PLcop, para=1.3) # 0.9557153 }

\dontrun{
  # Joe (2014, p. 165) shows the corner densities of the Plackett as Theta.
  # copBasic uses numerical density estimation and not analytical formula.
  eps <- .Machine$double.eps
  densityCOP(0,0, cop=PLcop, para=4) # 3.997073  (default eps^0.25)
  densityCOP(1,1, cop=PLcop, para=4) # 3.997073  (default eps^0.25)
  densityCOP(1,1, cop=PLcop, para=4, deluv=eps)     # 0 (silent failure)
  densityCOP(1,1, cop=PLcop, para=4, deluv=eps^0.5) # 4.5
  densityCOP(1,1, cop=PLcop, para=4, deluv=eps^0.4) # 4.002069
  densityCOP(1,1, cop=PLcop, para=4, deluv=eps^0.3) # 3.999513
  # So, we see that the small slicing does have an effect, the default of 0.25 is
  # intented for general application by being away enough from machine limits.}

\dontrun{
  # Joe (2014, p. 170) shows a closed form copula density for "Bivariate Joe/B5" copula
  "dJOEB5cop" <- function(u, v, para) {
     up <- (1-u)^para; vp <- (1-v)^para; eta <- para - 1
     A <- (up + vp - up*vp); B <- (1-u)^eta * (1-v)^eta; C <- (eta + A)
     return(A^(-2 + 1/para) * B * C)
  }
  densityCOP(0.32, 0.74, cop=JOcopB5, para=1.3)  # 0.9410653
  dJOEB5cop( 0.32, 0.74, para=1.3)               # 0.9410973 }
}
\keyword{visualization}
\keyword{copula (density)}
\keyword{Joe (2014) Examples and Exercises}


