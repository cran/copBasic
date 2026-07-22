\encoding{utf8}
\name{LMRuvCOP}
\alias{LMRuvCOP}
\title{Conditional L-moments of the Distribution of U given V}
\description{
Compute the \emph{conditional L-moments} of \eqn{U} given a \eqn{V} (the \eqn{Y} direction) through the \emph{conditional distribution function} \eqn{G(Y)} using the appropriate \emph{partial derivative} of a copula (\eqn{\mathbf{C}(u,v)}) with respect to \eqn{V}. The inversion of the partial derivative is the \emph{conditional quantile function}.
}
\usage{
LMRuvCOP(v=seq(0.01, 0.99, by=0.01), cop=NULL, para=NULL, nsim=1E5,
    subdivisions=200L, rel.tol=.Machine$double.eps^0.25, abs.tol=rel.tol, ...)
}
\arguments{
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{cop}{A copula function with vectorization as in \code{asCOP};}
  \item{para}{Vector of parameters or other data structures, if needed, to pass to the copula;}
  \item{nsim}{Number of simulations for \emph{Monte Carlo integration} if numerical integration were to fail;}
  \item{subdivisions}{Argument of same name passed to \code{integrate()};}
  \item{rel.tol}{Argument of same name passed to \code{integrate()};}
  \item{abs.tol}{Argument of same name passed to \code{integrate()}; and}
  \item{...}{Additional arguments to pass to \code{\link{derCOP2}}.}
}
\value{
  Value(s) in a list for the median and conditional L-moments given \eqn{V=v} are returned including
  \item{median}{The conditional median from \code{\link{med.regressCOP2}};}
  \item{L1}{The conditional mean \eqn{\lambda_1|V=v} from \code{\link{EuvCOP}};}
  \item{L2}{The conditional L-scale \eqn{\lambda_2|V=v};}
  \item{T3}{The conditional L-skew \eqn{\tau_3|V=v};}
  \item{T4}{The conditional L-kurtosis \eqn{\tau_4|V=v};}
}
\author{W.H. Asquith}
\seealso{\code{\link{EuvCOP}}, \code{\link{LMRvuCOP}}, \code{\link{derCOP2}}}
\examples{
# Please refer to Examples under LMRvuCOP().
}
\keyword{copula (utility)}
\keyword{copula operator}
\keyword{copula (conditional distribution)}
\keyword{conditional expectation}
\keyword{L-moments (conditional) of U given V}
\keyword{Conditional L-moments of U given V}
