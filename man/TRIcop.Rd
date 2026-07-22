\encoding{utf8}
\name{TRIcop}
\alias{TRIcop}
\title{The Triangular Copula}
\description{
The \emph{Triangular copula} (Nelsen, 2006, exam. 3.3, pp. 59--60; exer. 3.7, pp. 64--65) is
\deqn{\mathbf{C}_{\Theta}(u,v) = \mathbf{TRI}(u,v)} for \eqn{\Theta \in (0,-1)}. The copula, as \eqn{\Theta \rightarrow 0} limits, to the \emph{countermonotonicity coupla} (\eqn{\mathbf{W}(u,v)}; \code{\link{W}}), as \eqn{\Theta \rightarrow 1} limits to the \emph{comonotonicity copula} (\eqn{\mathbf{M}(u,v)};  \code{\link{M}}). The parameter \eqn{\Theta} is readily computed from a \emph{Spearman Rho} (\code{\link{rhoCOP}}) or \emph{Kendall Tau} (\code{\link{tauCOP}}) by \eqn{\rho_\mathbf{C} = \tau_\mathbf{C} = 2\Theta-1}. However in general for finite samples, the sample versions of Spearman Rho and Kendall Tau will not be equal and different \eqn{\Theta} wil result.
}
\usage{
TRIcop(u, v, para=NULL, rhotau=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (single element) of parameters---the \eqn{\Theta} parameter of the copula;}
  \item{rhotau}{Optional Spearman Rho or Kendall Tau; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned. Otherwise if \code{rhotau} is given, then the \eqn{\Theta} is computed and a \code{list} having
  \item{para}{The parameter \eqn{\Theta}, and}
  \item{rhotau}{Spearman Rho or Kendall Tau.}
and if \code{para=NULL} and \code{rhotau=NULL}, then the values within \code{u} and \code{v} are used to compute Spearman Rho or Kendall Tau and then compute the parameter, and these are returned in the aforementioned list.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{M}}, \code{\link{W}}
}
\examples{
# Concerning Nelsen (2006, exer. 5.6, p. 171)
rhoCOP(cop=TRIcop, para=0.25) # -0.4999916
tauCOP(cop=TRIcop, para=0.25) # -0.5

\dontrun{
  for(i in seq_len(4)) {
    para <- list(cop=TRIcop, para=0.25, reflect=i)
    uv <- simCOP(100, cop=COP, para=para, seed=1, col=i, ploton=! as.logical(i-1))
  } # }
}
\keyword{Triangular copula}
\keyword{copula (formulas)}
\keyword{copula}
\concept{Triangular copula}
\keyword{Nelsen (2006) Examples and Exercises}
