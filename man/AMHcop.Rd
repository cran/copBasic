\encoding{utf8}
\name{AMHcop}
\alias{AMHcop}
\title{The Ali--Mikhail--Haq Copula}
\description{
The \emph{Ali--Mikhail--Haq copula} (Nelsen, 2006, p. 92--93, 172) is
\deqn{\mathbf{C}_{\Theta}(u,v) = \mathbf{AMH}(u,v) = \frac{uv}{1 - \Theta(1-u)(1-v)}\mbox{,}}
where \eqn{\Theta \in [-1,+1)}, where the right boundary,
\eqn{\Theta = 1}, can sometimes be considered valid according to \enc{Mächler}{Machler} (2014). The copula \eqn{\Theta \rightarrow 0} becomes the \emph{independence copula} (\eqn{\mathbf{\Pi}(u,v)}; \code{\link{P}}), and the parameter \eqn{\Theta} is readily computed from a \emph{Kendall Tau} (\code{\link{tauCOP}}) by
\deqn{\tau_\mathbf{C} = \frac{3\Theta - 2}{3\Theta} -
                        \frac{2(1-\Theta)^2\log(1-\Theta)}{3\Theta^2}\mbox{,}}
and by \emph{Spearman Rho} (\code{\link{rhoCOP}}), through \enc{Mächler}{Machler} (2014), by
\deqn{\rho_\mathbf{C} = \sum_{k=1}^\infty \frac{3\Theta^k}{{k + 2 \choose 2}^2}\mbox{.}}

The support of \eqn{\tau_\mathbf{C}} is \eqn{[(5 - 8\log(2))/3, 1/3] } \eqn{\approx} \eqn{[-0.1817258, 0.3333333]} and the \eqn{\rho_\mathbf{C}} is \eqn{[33 - 48\log(2), 4\pi^2 - 39]} \eqn{\approx} \eqn{[-0.2710647, 0.4784176]}, which shows that this copula has a limited range of dependency. The infinite summation is easier to work with than
Nelsen (2006, p. 172) definition of
\deqn{\rho_\mathbf{C} = \frac{12(1+\Theta)}{\Theta^2}\mathrm{dilog}(1-\Theta)-
                        \frac{24(1-\Theta)}{\Theta^2}\log(1-\Theta)-
                        \frac{3(\Theta+12)}{\Theta}\mbox{,}}
where the \eqn{\mathrm{dilog(x)}} is the dilogarithm function defined by
\deqn{\mathrm{dilog}(x) = \int_1^x \frac{\log(t)}{1-t}\,\mathrm{d}t\mbox{.}}
The integral version has more nuances with approaches toward \eqn{\Theta = 0} and \eqn{\Theta = 1} than the infinite sum version.
}
\usage{
AMHcop(u, v, para=NULL, rho=NULL, tau=NULL, fit=c("rho", "tau"), ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (single element) of parameters---the \eqn{\Theta} parameter of the copula. However, if a second parameter is present, it is treated as a logical to reverse the copula (\eqn{u + v - 1 + \mathbf{AMH}(1-u,1-v; \Theta)});}
  \item{rho}{Optional Spearman Rho from which the parameter will be estimated and presence of \code{rho} trumps \code{tau};}
  \item{tau}{Optional Kendall Tau from which the parameter will be estimated;}
  \item{fit}{If \code{para}, \code{rho}, and \code{tau} are all \code{NULL}, the the \code{u} and \code{v} represent the sample. The measure of association by the \code{fit} declaration will be computed and the parameter estimated subsequently. The \code{fit} has not other utility than to trigger which measure of association is computed internally by the \code{cor} function in \R; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned. Otherwise if \code{tau} is given, then the \eqn{\Theta} is computed and a \code{list} having
  \item{para}{The parameter \eqn{\Theta}, and}
  \item{tau}{Kendall Tau.}
and if \code{para=NULL} and \code{tau=NULL}, then the values within \code{u} and \code{v} are used to compute Kendall Tau and then compute the parameter, and these are returned in the aforementioned list.
}

\note{
\enc{Mächler}{Machler} (2014) reports on accurate computation of \eqn{\tau_\mathbf{C}} and \eqn{\rho_\mathbf{C}} for this copula for conditions of \eqn{\Theta \rightarrow 0} and in particular derives the following equation, which does not have \eqn{\Theta} in the denominator:
\deqn{\rho_\mathbf{C} = \sum_{k=1}^{\infty} \frac{3\Theta^k}{{k+2 \choose 2}^2}\mbox{.}}
The \pkg{copula} package provides a Taylor series expansion for \eqn{\tau_\mathbf{C}} for small \eqn{\Theta} in the \code{copula::tauAMH()}. This is demonstrated here between the implementation of \eqn{\tau = 0} for parameter estimation in the \pkg{copBasic} package to that in the more sophisticated implementation in the \pkg{copula} package.
\preformatted{
  copula::tauAMH(AMHcop(tau=0)$para) # theta = -2.313076e-07
}
It is seen that the numerical approaches yield quite similar results for small \eqn{\tau_\mathbf{C}}, and finally, a comparison to the \eqn{\rho_\mathbf{C}} is informative:
\preformatted{
  rhoCOP(AMHcop, para=1E-9)    # 3.333333e-10 (two nested integrations)
  copula:::.rhoAmhCopula(1E-9) # 3.333333e-10 (cutoff based)
  theta <- seq(-1,1, by=.0001)
  RHOa <- sapply(theta, function(t) rhoCOP(AMHcop, para=t))
  RHOb <- sapply(theta, function(t) copula:::.rhoAmhCopula(t))
  plot(10^theta, RHOa-RHOb, type="l", col=2)
}
The plot shows that the apparent differences are less than 1 part in 100 million---The \pkg{copBasic} computation is radically slower though, but \code{\link{rhoCOP}} was designed for generality of copula family.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

\enc{Mächler}{Machler}, Martin, 2014, Spearman's Rho for the AMH copula---A beautiful formula: copula package vignette, accessed on April 7, 2018, at \url{https://CRAN.R-project.org/package=copula} under the vignette \emph{rhoAMH-dilog.pdf}.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Pranesh, Kumar, 2010, Probability distributions and estimation of Ali--Mikhail--Haq copula: Applied Mathematical Sciences, v. 4, no. 14, p. 657--666.
}
\author{W.H. Asquith}
\seealso{\code{\link{P}}
}
\examples{
\dontrun{
t <- 0.9 # The Theta of the copula and we will compute Spearman Rho.
di <- integrate(function(t) log(t)/(1-t), lower=1, upper=(1-t))$value
A <- di*(1+t) - 2*log(1-t) + 2*t*log(1-t) - 3*t # Nelsen (2007, p. 172)
rho <- 12*A/t^2 - 3    # 0.4070369
rhoCOP(AMHcop, para=t) # 0.4070369
sum(sapply(1:100, function(k) 3*t^k/choose(k+2, 2)^2)) # Machler (2014)
# 0.4070369 (see Note section, very many tens of terms are needed) }

\dontrun{
  layout(matrix(1:2, byrow=TRUE)) # Note: Kendall Tau is same on reversal.
  s <- 2; set.seed(s); nsim <- 10000
  UVn <- simCOP(nsim, cop=AMHcop, para=c(-0.9, "FALSE" ), col=4)
  mtext("Normal definition [default]") # '2nd' parameter could be skipped
  set.seed(s) # seed used to keep Rho/Tau inside attainable limits
  UVr <- simCOP(nsim, cop=AMHcop, para=c(-0.9, "TRUE"),   col=2)
  mtext("Reversed definition")
  AMHcop(UVn[,1], UVn[,2], fit="rho")$rho # -0.2581653
  AMHcop(UVr[,1], UVr[,2], fit="rho")$rho # -0.2570689
  rhoCOP(cop=AMHcop, para=-0.9)           # -0.2483124
  AMHcop(UVn[,1], UVn[,2], fit="tau")$tau # -0.1731904
  AMHcop(UVr[,1], UVr[,2], fit="tau")$tau # -0.1724820
  tauCOP(cop=AMHcop, para=-0.9)           # -0.1663313 }
}
\keyword{Ali--Mikhail--Haq copula}
\keyword{copula (formulas)}
\keyword{copula}
\concept{Ali-Mikhail-Haq copula}
\keyword{package copula (comparison to)}

