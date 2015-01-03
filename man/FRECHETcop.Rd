\encoding{utf8}
\name{FRECHETcop}
\alias{FRECHETcop}
\title{ The \enc{Fréchet}{Frechet} Family Copula }
\description{
The \emph{\enc{Fréchet}{Frechet} Family copula} (Durante, 2007, pp. 256--259) is
\deqn{\mathbf{C}_{\alpha, \beta}(u,v) = \mathbf{FF}(u,v) = \alpha\mathbf{M}(u,v) + (1-\alpha-\beta)\mathbf{\Pi}(u,v)+\beta\mathbf{W}(u,v)\mbox{,}}
where \eqn{\alpha, \beta \ge 0} and \eqn{\alpha + \beta \le 1}. The \enc{Fréchet}{Frechet} Family copulas are \emph{convex combinations} of the fundamental copulas \eqn{\mathbf{W}} (\emph{\enc{Fréchet}{Frechet}-Hoeffding lower bound copula}), \eqn{\mathbf{\Pi}} (independence), and \eqn{\mathbf{M}} (\emph{\enc{Fréchet}{Frechet}-Hoeffding upper bound copula}). The copula is \emph{comprehensive} because both \eqn{\mathbf{W}} and \eqn{\mathbf{M}} can be obtained.   The parameters are readily estimated using \emph{Spearman's Rho} (\eqn{\rho}) and \emph{Kendall's Tau} (\eqn{\tau}) by
\deqn{\tau = \frac{(\alpha - \beta)(\alpha + \beta + 2)}{3}\mbox{\ and\ } \rho = \alpha - \beta\mbox{.}}
The copula virtually always has a \emph{singular component} unless \eqn{\alpha, \beta = 0}. The \enc{Fréchet}{Frechet} Family copula has respective \emph{lower-} and \emph{upper-tail dependency parameters} of \eqn{\lambda_L = \alpha} and \eqn{\lambda_U = \alpha}. Lastly, Durante (2007) reports that the \enc{Fréchet}{Frechet} Family copula can approximate any bivariate copula in a \dQuote{unique way} and the error bound can be estimated.
}
\usage{
FRECHETcop(u,v, para=NULL, rho=NULL, tau=NULL, par2rhotau=FALSE, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (two element) of parameters \eqn{\alpha} and \eqn{\beta};}
  \item{rho}{Spearman's \eqn{\rho} from which to estimate the parameters;}
  \item{tau}{Kendall's \eqn{\tau} from which to estimate the parameters;}
  \item{par2rhotau}{A logical that if \code{TRUE} will return an \R \code{list} of the \eqn{\rho} and \eqn{\tau} for the parameters; and }
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned using the \eqn{\alpha} and \eqn{\beta} as set by argument \code{para}. If \code{para=NULL} and \code{rho} and \code{tau} are set and compatible with the copula, then \eqn{\{\rho, \tau\} \rightarrow \{\alpha, \beta\}} and an \R \code{list} is returned.
}
\note{
The function will check the consistency of the parameters whether given by argument or computed from \eqn{\rho} and \eqn{\tau}. Seemingly every combination of \eqn{\rho} and \eqn{\tau} can produce a valid copula. The term \dQuote{Family} is used with this particular copula in \pkg{copBasic} documentation so as to draw distinction to the \enc{Fréchet}{Frechet} lower and upper bounds as the two limiting copulas are sometimes known as.
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
tau <- cor(UV$U, UV$V, method="kendall" ) # sample Kendall's Tau
rho <- cor(UV$U, UV$V, method="spearman") # sample Spearman's Rho
spara <- FRECHETcop(rho=rho, tau=tau) # a fitted Frechet Family copula
spara <- spara$para
if(is.na(spara[1])) { # now a fittable combination is not guaranteed
   warning("sample rho and tau do not provide valid parameters, ",
           "try another simulation")
} else { # now if fit, draw some red-colored level curves for comparison
   level.curvesCOP(cop=FRECHETcop, para=spara, ploton=FALSE, col=2)
}


}
}
\keyword{Frechet Family copula}
\keyword{copula (formulas)}
\keyword{copula}

