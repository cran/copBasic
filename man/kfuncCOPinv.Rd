\encoding{utf8}
\name{kfuncCOPinv}
\alias{kfuncCOPinv}
\title{The Inverse Kendall Function of a Copula}
\description{
Compute the (numerical) inverse \eqn{F^{(-1)}_K(z) \equiv z(F_K)} of the \emph{Kendall Function} \eqn{F_K(z; \mathbf{C})} (\code{\link{kfuncCOP}}) of a copula \eqn{\mathbf{C}(u,v)} given nonexceedance probability \eqn{F_K}. The \eqn{z} is the joint probability of the random variables \eqn{U} and \eqn{V} coupled to each other through the copula \eqn{\mathbf{C}(u,v)} and the nonexceedance probability of the probability \eqn{z} is \eqn{F_K}---statements such as ``probabilities of probabilities'' are rhetorically complex so pursuit of word precision is made herein.
}
\usage{
kfuncCOPinv(f, cop=NULL, para=NULL, ...)
}
\arguments{
  \item{f}{Nonexceedance probability \eqn{(0 \le F_K \le 1)};}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value(s) for \eqn{z(F_K)} are returned.
}
\note{
The L-moments of Kendall Functions appear to be unresearched. Therefore, the \code{\link{kfuncCOPlmom}} and \code{\link{kfuncCOPlmoms}} functions were written. These compute L-moments on the CDF \eqn{F_K(z)} and not the quantile function \eqn{z(F_K)} and thus are much faster than trying to use \code{\link{kfuncCOPinv}} in the more common definitions of L-moments. A demonstration of the mean (first L-moment) of the Kendall Function numerical computation follows:
\preformatted{
  # First approach
  "afunc" <- function(f) kfuncCOPinv(f, cop=GHcop, para=pi)
  integrate(afunc, 0, 1) # 0.4204238 with absolute error < 2.5e-05
  # Second approach
  kfuncCOPlmom(1, cop=GHcop, para=pi)  # 0.4204222
}
where the first approach uses \eqn{z(F_K)}, whereas the second method uses integration for the mean on \eqn{F_K(z)}.
}
\references{
Asquith, W.H., 2011, Distributional analysis with L-moment statistics using the R environment for statistical computing: Createspace Independent Publishing Platform, ISBN 978--146350841--8.

Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{kfuncCOP}}}
\examples{
\dontrun{
Z <- c(0,0.25,0.50,0.75,1) # Joint probabilities of a N412cop
kfuncCOPinv(kfuncCOP(Z, cop=N4212cop, para=4.3), cop=N4212cop, para=4.3)
# [1] 0.0000000 0.2499984 0.5000224 0.7500112 1.0000000}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{Kendall inverse distribution function}
\keyword{Kendall quantile function}
