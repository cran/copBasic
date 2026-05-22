\encoding{utf8}
\name{RFcop}
\alias{RFcop}
\title{The Raftery Copula}
\description{
The \emph{Raftery copula} (Nelsen, 2006, p. 172) is

\deqn{\mathbf{C}_{\Theta}(u,v) = \mathbf{RF}(u,v) = \mathbf{M}(u,v) + \frac{1-\Theta}{1+\Theta}\biggl(uv\biggr)^{1/(1-\Theta)}\biggl[1-\bigl(\mathrm{max}\{u,v\}\bigr)^{-(1+\Theta)/(1-\Theta)}\biggr]\mbox{,}}

where \eqn{\Theta \in (0,1)}. The copula, as \eqn{\Theta \rightarrow 0^{+}} limits, to the \emph{independence coupla} (\eqn{\mathbf{P}(u,v)}; \code{\link{P}}), and as \eqn{\Theta \rightarrow 1^{-}}, limits to the \emph{comonotonicity copula} (\eqn{\mathbf{M}(u,v)};  \code{\link{M}}). The parameter \eqn{\Theta} is readily computed from \emph{Spearman Rho} (\code{\link{rhoCOP}}) by \eqn{\rho_\mathbf{C} = \Theta(4-3\Theta)/(2-\Theta)^2} or from \emph{Kendall Tau} (\code{\link{tauCOP}}) by \eqn{\tau_\mathbf{C} = 2\Theta/(3-\Theta)}. However, this copula like others within the \pkg{copBasic} package can be reflected (rotated) at will with the \code{\link{COP}} abstraction layer to acquire negative or inverse dependency (\emph{countermonotonicity}) (see the \bold{Examples}).
}
\usage{
RFcop(u, v, para=NULL, rho=NULL, tau=NULL, fit=c("rho", "tau"), ...)
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
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{M}}, \code{\link{P}}
}
\examples{
# Lower tail dependency of Theta = 0.5 --> 2*(0.5)/(1+0.5) = 2/3 (Nelsen, 2006, p. 214)
taildepCOP(cop=RFcop, para=0.5)$lambdaL # 0.66667

\dontrun{
  # Simulate for a Spearman Rho of 0.7, then extract estimated Theta that internally
  # is based on Kendall Tau of U and V, then convert estimate to equivalent Rho.
  set.seed(1)
  UV <- simCOP(1000, cop=RFcop, RFcop(rho=0.7)$para)
  Theta <- RFcop(UV$U, UV$V, fit="tau")$para # 0.607544
  Rho   <- Theta*(4-3*Theta)/(2-Theta)^2     # 0.682255 (nearly 0.7) #}

\dontrun{
  set.seed(1)
  UV <- simCOP(1000, cop=COP, para=list(cop=RFcop, para=RFcop(rho=0.5)$para, reflect=3))
  cor(UV$U, UV$V, method="spearman") # -0.492677 as expected with reversal of V #}
}
\keyword{Raftery copula}
\keyword{copula (formulas)}
\keyword{copula}
\concept{Raftery copula}
\keyword{Nelsen (2006) Examples and Exercises}
\keyword{Theory Examples (reflection/rotation)}
