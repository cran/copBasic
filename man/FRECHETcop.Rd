\encoding{utf8}
\name{FRECHETcop}
\alias{FRECHETcop}
\title{ The \enc{Fréchet}{Frechet} Family Copula }
\description{
The \emph{\enc{Fréchet}{Frechet} Family copula} (Durante, 2007, pp. 256--259) is
\deqn{\mathbf{C}_{\alpha, \beta}(u,v) = \mathbf{FF}(u,v) = \alpha\mathbf{M}(u,v) + (1-\alpha-\beta)\mathbf{\Pi}(u,v)+\beta\mathbf{W}(u,v)\mbox{,}}
where \eqn{\alpha, \beta \ge 0} and \eqn{\alpha + \beta \le 1}. The \enc{Fréchet}{Frechet} Family copulas are \emph{convex combinations} of the fundamental copulas \eqn{\mathbf{W}} (\emph{\enc{Fréchet}{Frechet}--Hoeffding lower bound copula}; \code{\link{W}}), \eqn{\mathbf{\Pi}} (independence; \code{\link{P}}), and \eqn{\mathbf{M}} (\emph{\enc{Fréchet}{Frechet}--Hoeffding upper bound copula}; \code{\link{M}}). The copula is \emph{comprehensive} because both \eqn{\mathbf{W}} and \eqn{\mathbf{M}} can be obtained.   The parameters are readily estimated using \emph{Spearman Rho} (\eqn{\rho_\mathbf{C}}; \code{\link{rhoCOP}}) and \emph{Kendall's Tau} (\eqn{\tau_\mathbf{C}}; \code{\link{tauCOP}}) by

\deqn{\tau_\mathbf{C} = \frac{(\alpha - \beta)(\alpha + \beta + 2)}{3}\mbox{\ and\ } \rho_\mathbf{C} = \alpha - \beta\mbox{.}}

The \enc{Fréchet}{Frechet} Family copula virtually always has a visible \emph{singular component} unless \eqn{\alpha, \beta = 0}. The copula has respective \emph{lower-} and \emph{upper-tail dependency parameters} of \eqn{\lambda^L = \alpha} and \eqn{\lambda^U = \alpha} (\code{\link{taildepCOP}}). Durante (2007, p. 257) reports that the \enc{Fréchet}{Frechet} Family copula can approximate any bivariate copula in a \dQuote{unique way} and the error bound can be estimated.
}
\usage{
FRECHETcop(u,v, para=NULL, rho=NULL, tau=NULL, par2rhotau=FALSE, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (two element) of parameters \eqn{\alpha} and \eqn{\beta};}
  \item{rho}{Spearman's \eqn{\rho_\mathbf{C}} from which to estimate the parameters;}
  \item{tau}{Kendall's \eqn{\tau_\mathbf{C}} from which to estimate the parameters;}
  \item{par2rhotau}{A logical that if \code{TRUE} will return an \R \code{list} of the \eqn{\rho} and \eqn{\tau} for the parameters; and }
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned using the \eqn{\alpha} and \eqn{\beta} as set by argument \code{para}; however, if \code{para=NULL} and \code{rho} and \code{tau} are set and compatible with the copula, then \eqn{\{\rho_\mathbf{C}, \tau_\mathbf{C}\} \rightarrow \{\alpha, \beta\}}, parameter estimation made, and an \R \code{list} is returned.
}
\details{
The function will check the consistency of the parameters whether given by argument or computed from \eqn{\rho_\mathbf{C}} and \eqn{\tau_\mathbf{C}}. The term \dQuote{Family} is used with this particular copula in \pkg{copBasic} so as to draw distinction to the \enc{Fréchet}{Frechet} lower and upper bound copulas as the two limiting copulas are called.

For no other reason than that it can be easily done and makes a nice picture, loop through a nest of \eqn{\rho} and \eqn{\tau} for the \enc{Fréchet}{Frechet} Family copula and plot the domain of the resulting parameters:
\preformatted{
  ops <- options(warn=-1) # warning supression because "loops" are dumb
  taus <- rhos <- seq(-1,1, by=0.01)
  plot(NA, NA, type="n", xlim=c(0,1), ylim=c(0,1),
       xlab="Frechet Copula Parameter Alpha",
       ylab="Frechet Copula Parameter Beta")
  for(tau in taus) {
    for(rho in rhos) {
      fcop <- FRECHETcop(rho=rho, tau=tau)
      if(! is.na(fcop$para[1])) points(fcop$para[1], fcop$para[2])
    }
  }
  options(ops)
}
}
\note{
A convex combination (\code{\link{convex2COP}}) of \eqn{\mathbf{\Pi}} and \eqn{\mathbf{M}}, which is a modification of the \enc{Fréchet}{Frechet} Family, is the \emph{Linear Spearman} copula:
\deqn{\mathbf{C}_\alpha(u,v) = (1-\alpha)\mathbf{\Pi}(u,v) + \alpha\mathbf{M}(u,v)\mbox{,}}
for \eqn{0 \le \alpha \le 1}, and the parameter is equal to \eqn{\rho_\mathbf{C}}. When the convex combination is used for construction, the complement of the parameter is equal to \eqn{\rho_\mathbf{C}} (\emph{e.g.} \eqn{1-\alpha = \rho_\mathbf{C}}; \code{\link{rhoCOP}}), which can be validated by
\preformatted{
  rhoCOP(cop=convex2COP, para=list(alpha=1-0.48, cop1=P, cop2=M)) # 0.4799948
}
}
\references{
Durante, F., 2007, Families of copulas, Appendix C, \emph{in} Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{M}}, \code{\link{P}}, \code{\link{W}}
}
\examples{
ppara <- c(0.25, 0.50)
fcop <- FRECHETcop(para=ppara, par2rhotau=TRUE)
RHO <- fcop$rho; TAU <- fcop$tau

\dontrun{
level.curvesCOP(cop=FRECHETcop, para=ppara) # Durante (2007, Fig. C.27(b))
mtext("Frechet Family copula")
 UV <- simCOP(n=50, cop=FRECHETcop, para=ppara, ploton=FALSE, points=FALSE)
tau <- cor(UV$U, UV$V, method="kendall" ) # sample Kendall Tau
rho <- cor(UV$U, UV$V, method="spearman") # sample Spearman Rho
spara <- FRECHETcop(rho=rho, tau=tau) # a fitted Frechet Family copula
spara <- spara$para
if(is.na(spara[1])) { # now a fittable combination is not guaranteed
   warning("sample rho and tau do not provide valid parameters, ",
           "try another simulation")
} else { # now if fit, draw some red-colored level curves for comparison
   level.curvesCOP(cop=FRECHETcop, para=spara, ploton=FALSE, col=2)
} #}
}
\keyword{Frechet Family copula}
\keyword{copula (formulas)}
\keyword{copula}
\concept{Linear Spearman copula}
\concept{Linear Spearman}

