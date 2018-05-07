\encoding{utf8}
\name{CLcop}
\alias{CLcop}
\title{ The Clayton Copula}
\description{
The \emph{Clayton copula} (Joe, 2014, p. 168) is
\deqn{\mathbf{C}_{\Theta}(u,v) = \mathbf{CL}(u,v) = \mathrm{max}[(u^{-\Theta}+v^{-\Theta}-1; 0)]^{-1/\Theta}\mbox{,}}
where \eqn{\Theta \in [-1,\infty), \Theta \ne 0}
The copula as \eqn{\Theta \rightarrow -1^{+}} limits to the \emph{countermonotonicity coupla} (\eqn{\mathbf{W}(u,v)};  \code{\link{W}}), as \eqn{\Theta \rightarrow 0} limits to \emph{independence copula} (\eqn{\mathbf{\Pi}(u,v)}; \code{\link{P}}), and as \eqn{\Theta \rightarrow \infty} limits to the \emph{comonotonicity copula} (\eqn{\mathbf{M}(u,v)};  \code{\link{M}}). Finally, the parameter \eqn{\Theta} is readily computed from \emph{Kendall's Tau} (\code{\link{tauCOP}}) by \eqn{\tau_\mathbf{C} = \Theta/(\Theta+2)}.
}
\usage{
CLcop(u, v, para=NULL, tau=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (single element) of parameters---the \eqn{\Theta} parameter of the copula;}
  \item{tau}{Optional Kendall's Tau; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned. Otherwise if \code{tau} is given, then the \eqn{\Theta} is computed and a \code{list} having
  \item{para}{The parameter \eqn{\Theta}, and}
  \item{tau}{Kendall's tau.}
and if \code{para=NULL} and \code{tau=NULL}, then the values within \code{u} and \code{v} are used to compute Kendall Tau and then compute the parameter, and these are returned in the aforementioned list.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{M}}, \code{\link{P}}, \code{\link{W}}
}
\examples{
# Lower tail dependency of Theta = pi --> 2^(-1/pi) = 0.8020089 (Joe, 2014, p. 168)
taildepCOP(cop=CLcop, para=pi)$lambdaL # 0.80201
}
\keyword{Clayton copula}
\keyword{copula (formulas)}
\keyword{copula}
\concept{Clayton copula}
\keyword{Joe (2014) Examples and Exercises}
