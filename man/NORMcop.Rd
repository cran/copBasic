\encoding{utf8}
\name{NORMcop}
\alias{NORMcop}
\title{The Normal (Gaussian) Copula}
\description{
The \emph{Normal copula} (\emph{Gaussian copula}) (Salvadori \emph{et al.}, 2007, pp. 255--256) is

\deqn{\mathbf{C}_{\Theta}(u,v) = \mathbf{NORM}(u,v; \Theta) =
\int_{-\infty}^{\Phi^{(-1)}(u)}\!\!\!\!\int_{-\infty}^{\Phi^{(-1)}(v)}\!\!\!\!\!\!\!\!
\frac{1}{2\pi\sqrt{1-\Theta^2}}
\mathrm{exp}\biggl(-\frac{s^2 - 2\Theta s t + t^2}{ 2(1-\Theta^2) } \biggr)\,\mathrm{d}s\,\mathrm{d}t\mbox{,}}

where \eqn{\Theta \in [-1,1]} and \eqn{\Phi^{(-1)}(x)} is the quantile function of the \emph{univariate normal distribution}. The copula, as \eqn{\Theta \rightarrow -1^{+}}, limits to the \emph{countermonotonicity copula} (\eqn{\mathbf{W}(u,v)}; \code{\link{W}}), as \eqn{\Theta \rightarrow 0} limits, to the \emph{independence coupla} (\eqn{\mathbf{P}(u,v)}; \code{\link{P}}), and as \eqn{\Theta \rightarrow 1^{-}}, limits to the \emph{comonotonicity copula} (\eqn{\mathbf{M}(u,v)};  \code{\link{M}}). The copula has \emph{lower-tail} and \emph{upper-tail dependency parameters} equal to zero, but such is not true for the closely related \emph{t-Student copula} (\code{\link{Tcop}}). The \emph{Spearman Rho} (\code{\link{rhoCOP}}) is \eqn{\rho_\mathbf{C} = (6/\pi)\cdot\mathrm{asin}(\Theta/2)} and \emph{Kendall Tau} (\code{\link{tauCOP}}) is \eqn{\tau_\mathbf{C} = (2/\pi)\cdot\mathrm{asin}(\Theta)}. The parameter \eqn{\Theta} is readily computed by \eqn{\Theta = 2\cdot\mathrm{sin}(\pi\cdot\rho_\mathbf{C}/6)} or by \eqn{\Theta = \mathrm{sin}(\pi\cdot\tau_\mathbf{C}/2)}.
}
\usage{
NORMcop(u, v, para=NULL, rho=NULL, tau=NULL, fit=c("rho", "tau"), ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (single element) of parameters---the \eqn{\Theta} parameter of the copula;}
  \item{rho}{Optional Spearman Rho from which the parameter will be estimated and presence of \code{rho} trumps \code{tau};}
  \item{tau}{Optional Kendall Tau from which the parameter will be estimated;}
  \item{fit}{If \code{para}, \code{rho}, and \code{tau} are all \code{NULL}, then the \code{u} and \code{v} represent the sample. The measure of association by the \code{fit} declaration will be computed and the parameter estimated subsequently. The \code{fit} has no other utility than to trigger which measure of association is computed internally by the \code{cor} function in \R; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned. Otherwise if either \code{rho} or \code{tau} is given, then the \eqn{\Theta} is computed and a \code{list} having
  \item{para}{The parameter \eqn{\Theta};}
  \item{rho}{Spearman Rho if the \code{rho} is given; and}
  \item{tau}{Kendall Tau if the \code{tau} is given but also if both \code{rho} and \code{tau} are \code{NULL} as mentioned next.}
and if \code{para=NULL} and \code{rho} and \code{tau=NULL}, then the values within \code{u} and \code{v} are used to compute Spearman Rho (\code{fit="rho"}) or Kendall Tau (\code{fit="tau"}) and then compute the parameter, and this is returned in the aforementioned list.
}
\note{
A mimic of the wrappers on \code{mvtnorm::pmvnorm()} from the \pkg{copula} package (v1.1-6) are made within \code{NORMcop}. The rest of the implementation is unique to \pkg{copBasic}. An implementation of a function in the \pkg{copBasic} style that would natively interconnect to \pkg{copula} package version of the copula could be as follows:
\preformatted{
  "NORMcop" <-         # pCoupla() from package copula is analogous to COP()
  function(u,v, para=NULL, ...) {
    if(length(u) == 1) u <- rep(u, length(v)) # see asCOP() for reasoning of
    if(length(v) == 1) v <- rep(v, length(u)) # this "vectorization" hack
    para <- copula::normalCopula(                 c(para), dim=2)
    return( copula::pCopula(matrix(c(u,v), ncol=2), para) )
  }
}
}
\references{
Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{Tcop}}
}
\examples{
\dontrun{
  para <- NORMcop(para=NULL, rho=0.9, taildep=0.3) # 0.907981
  UV <- simCOP(1000, cop=NORMcop, para=para$para)  # compare to similar Tcop example }
}
\keyword{Normal copula}
\keyword{Gaussian copula}
\keyword{copula (formulas)}
\keyword{copula}
