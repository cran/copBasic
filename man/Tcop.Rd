\encoding{utf8}
\name{Tcop}
\alias{Tcop}
\title{The t-Student Copula}
\description{
The \emph{t-Student copula} (Salvadori \emph{et al.}, 2007, pp. 255--256) is

\deqn{\mathbf{C}_{\Theta,\nu}(u,v) = \mathbf{T}(u,v; \Theta,\nu) =
\int_{-\infty}^{t_\nu^{(-1)}(u)}\!\!\!\!\int_{-\infty}^{t_\nu^{(-1)}(v)}\!\!\!\!\!\!
\frac{1}{2\pi\sqrt{1-\Theta^2}}
\mathrm{exp}\biggl(-\frac{s^2 - 2\Theta s t + t^2}{ \nu(1-\Theta^2) } \biggr)^{-(\nu+2)/2}\!\!\!\!\mathrm{d}s\,\mathrm{d}t\mbox{,}}

where \eqn{\Theta \in [-1,1]}, \eqn{\nu \ge 1} (integer), and \eqn{t_\nu^{(-1)}(x)} is the quantile function of the \emph{univariate t-distribution}. The copula, as \eqn{\Theta \rightarrow -1^{+}}, limits to the \emph{countermonotonicity copula} (\eqn{\mathbf{W}(u,v)}; \code{\link{W}}), as \eqn{\Theta \rightarrow 0} limits, to the \emph{independence coupla} (\eqn{\mathbf{P}(u,v)} if \eqn{\nu} becomes large; \code{\link{P}}), and as \eqn{\Theta \rightarrow 1^{-}}, limits to the \emph{comonotonicity copula} (\eqn{\mathbf{M}(u,v)}; \code{\link{M}}). The copula has \emph{lower-tail dependency} and \emph{upper-tail dependency parameters} that are nonzero if \eqn{\Theta > 0} and both parameters equal to

\deqn{\lambda_{L|U} = 2t_{\nu+1}\biggl(-\frac{\sqrt{\nu+1}\sqrt{1-\Theta}}{\sqrt{1+\Theta}}\biggr)\mathrm{,}}

which tend to zero as \eqn{\nu \rightarrow \infty}, which are those for the \emph{Normal copula} (\code{\link{NORMcop}}). The \emph{Spearman Rho} (\code{\link{rhoCOP}}) is \eqn{\rho_\mathbf{C} = (6/\pi)\cdot\mathrm{asin}(\Theta/2)} and \emph{Kendall Tau} (\code{\link{tauCOP}}) is \eqn{\tau_\mathbf{C} = (2/\pi)\cdot\mathrm{asin}(\Theta)} and are the same as for \code{\link{NORMcop}}. The parameter \eqn{\Theta} is readily computed by \eqn{\Theta = 2\cdot\mathrm{sin}(\pi\cdot\rho_\mathbf{C}/6)} or by \eqn{\Theta = \mathrm{sin}(\pi\cdot\tau_\mathbf{C}/2)}.
}
\usage{
Tcop(u, v, para=NULL, rho=NULL, tau=NULL, taildep=NULL, fit=c("rho", "tau"), ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (two element) of parameters---\eqn{\Theta} and \eqn{\nu} parameters of the copula and note that \eqn{\nu} will be silently cast as an integer after rounding to zero digits internally for \code{mvtnorm::pmvt()};}
  \item{rho}{Optional Spearman Rho from which the parameter will be estimated and presence of \code{rho} trumps \code{tau};}
  \item{tau}{Optional Kendall Tau from which the parameter will be estimated;}
  \item{taildep}{Optional lower/upper tail dependency coefficient to try to fit the parameters for the give \code{rho} or \code{tau};}
  \item{fit}{If \code{para}, \code{rho}, and \code{tau} are all \code{NULL}, then the \code{u} and \code{v} represent the sample. The measure of association by the \code{fit} declaration will be computed and the parameter estimated subsequently. The \code{fit} has no other utility than to trigger which measure of association is computed internally by the \code{cor} function in \R; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned. Otherwise if either \code{rho} or \code{tau} is given, then the \eqn{\Theta} and \eqn{\nu} are computed and a \code{list} having
  \item{para}{The parameters \eqn{\Theta} and \eqn{\nu} (if computable, refer to \code{message}) and note that \eqn{\nu} will be silently cast as an integer after rounding to zero digits internally for \code{ mvtnorm::pmvt()};}
  \item{rho}{Spearman Rho if the \code{rho} is given; and}
  \item{tau}{Kendall Tau if the \code{tau} is given but also if both \code{rho} and \code{tau} are \code{NULL} as mentioned next.}
  \item{lambdaUL}{Computed tail dependency (if computable);}
  \item{message}{Helpful message;}
  \item{lambdaUL_if_nu_were_eqone}{Computed tail dependency if \eqn{\nu} were to equal 1;}
and if \code{para=NULL} and \code{rho} and \code{tau=NULL}, then the values within \code{u} and \code{v} are used to compute Spearman Rho (\code{fit="rho"}) or Kendall Tau (\code{fit="tau"}) and then compute the \eqn{\Theta} and then attempt to compute the \eqn{\nu} from the \code{taildep}, and these are returned in the aforementioned list.
}
\note{
A mimic of the wrappers on \code{mvtnorm::pmvt()} from the \pkg{copula} package (v1.1-6) are made within \code{Tcop}. The rest of the implementation is unique to \pkg{copBasic}. An implementation of a function in the \pkg{copBasic} style that would natively interconnect to \pkg{copula} package version of the copula could be as follows:
\preformatted{
  "Tcop" <-         # pCoupla() from package copula is analogous to COP()
  function(u,v, para=NULL) {
    if(length(u) == 1) u <- rep(u, length(v)) # see asCOP() for reasoning of
    if(length(v) == 1) v <- rep(v, length(u)) # this "vectorization" hack
    para <- copula::tCopula(c(para[1]), dim=2, df=as.integer(para[2]))
    return( copula::pCopula(matrix(c(u,v), ncol=2), para) )
  }
}
}
\references{
Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{NORMcop}}
}
\examples{
\dontrun{
  para <- Tcop(para=NULL, rho=0.9, taildep=0.3) # 0.907981 22.000000
  UV <- simCOP(1000, cop=Tcop, para=para$para)  # see the tail dependency }
}
\keyword{t-Student copula}
\keyword{Student t}
\keyword{copula (formulas)}
\keyword{copula}
