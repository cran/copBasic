\encoding{utf8}
\name{rhobevCOP}
\alias{rhobevCOP}
\title{ A Dependence Measure for a Bivariate Extreme Value Copula based on the Expectation of the Product of Negated Log-Transformed Random Variables U and V }
\description{
Compute a dependence measure based on the expectation of the product of transformed random variables \eqn{U} and \eqn{V}, which unnamed by Joe (2014, pp. 383--384) but symbolically is \eqn{\rho_E}, having a \emph{bivariate extreme value copula} \eqn{\mathbf{C}_{BEV}(u,v)} by
\deqn{\rho_E = \mathrm{E}[(-\log U) \times (-\log V)] - 1 = \int_0^1 [B(w)]^{-2}\,\mathrm{d}w - 1\mbox{,}}
where \eqn{B(w) = A(w, 1-w)}, \eqn{B(0) = B(1) = 1}, \eqn{B(w) \ge 1/2}, and \eqn{0 \le w \le 1}, and where only bivariate extreme value copulas can be written as
\deqn{\mathbf{C}_{BEV}(u,v) = \mathrm{exp}[-A(-\log u, -\log v)]\mbox{,}}
and thus in terms of the coupla
\deqn{B(w) = -\log[\mathbf{C}_{BEV}(\mathrm{exp}[-w], \mathrm{exp}[w-1])]\mbox{.}}

Joe (2014, p. 383) states that \eqn{\rho_E} is the correlation of the \dQuote{survival function of a bivariate min-stable exponential distribution,} which can be assembled as a function of \eqn{B(w)}. Joe (2014, p. 383) also shows the following expression for \emph{Spearman Rho}
\deqn{\rho_S = 12 \int_0^1 [1 + B(w)]^{-2}\,\mathrm{d}w - 3}
in terms of \eqn{B(w)}. This expression, in conjunction with \code{\link{rhoCOP}}, was used to confirm the prior expression shown here for \eqn{B(w)} in terms of \eqn{\mathbf{C}_{BEV}(u,v)}. Lastly, for \emph{independence} (\eqn{uv = \mathbf{\Pi}}; \code{\link{P}}), \eqn{\rho_E = 0} and for the \emph{\enc{Fréchet}{Frechet}--Hoeffding upper bound copula} (perfect positive association), \eqn{\rho_E = 1}.
}
\usage{
rhobevCOP(cop=NULL, para=NULL, as.sample=FALSE, brute=FALSE, delta=0.002, ...)
}
\arguments{
  \item{cop}{A bivariate extreme value copula function---the function \code{rhobevCOP} makes \bold{no provision} for verifying whether the copula in \code{cop} is actually an \emph{extreme value copula};}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
    \item{as.sample}{A logical controlling whether an optional \R \code{data.frame} in \code{para} is used to compute a \eqn{\hat\rho_E} by \code{mean()} of the product of negated \code{log()}'s in \R. The user is required to cast \code{para} into estimated probabilities (see \bold{Examples});}
  \item{brute}{Should brute force be used instead of two nested \code{integrate()} functions in \R to perform the double integration;}
  \item{delta}{The \eqn{\mathrm{d}w} for the brute force (\code{brute=TRUE}) integration; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for \eqn{\rho_E} is returned.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{rhoCOP}}, \code{\link{tauCOP}} }
\examples{
Theta <- GHcop(tau=1/3)$para     # Gumbel-Hougaard copula with Kendall Tau = 1/3
rhobevCOP(cop=GHcop, para=Theta) # 0.3689268 (RhoE after Joe [2014])
rhoCOP(   cop=GHcop, para=Theta) # 0.4766613 (Spearman's Rho)
\dontrun{
set.seed(394)
simUV <- simCOP(n=30000, cop=GHcop, para=Theta, graphics=FALSE) # large simulation
samUV <- simUV * 150; n <- length(samUV[,1]) # convert to fake unit system
samUV[,1] <- rank(simUV[,1]-0.5)/n; samUV[,2] <- rank(simUV[,2]-0.5)/n # hazen
rhobevCOP(para=samUV, as.sample=TRUE) # 0.3708275}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{copula (properties for extreme value)}
