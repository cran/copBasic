\name{hoefCOP}
\alias{hoefCOP}
\alias{LpCOP}
\alias{LpCOPradsym}
\alias{LpCOPpermsym}
\title{ The Hoeffding's Phi of a Copula or Lp Distances (Independence, Radial Asymmetry, or Reflection Symmetry Forms) }
\description{
Compute the measure of association known as \emph{Hoeffding's Phi} \eqn{\sigma_\mathbf{C}} of a copula from independence (\eqn{uv}, \code{\link{P}}) according to Cherunbini \emph{et al.} (2004, p. 164) by
\deqn{\phi_\mathbf{C} = 3 \sqrt{10\int\!\!\int_{\mathcal{I}^2} (\mathbf{C}(u,v) - uv)^2\,\mathrm{d}u\mathrm{d}v}\mbox{,}}
and Nelsen (2006, p. 210) shows this as
\deqn{\Phi_\mathbf{C} = \biggl(90\int\!\!\int_{\mathcal{I}^2} |\mathbf{C}(u,v) - uv|^2\,\mathrm{d}u\mathrm{d}v\biggr)^{1/2}\mbox{,}}
for which \eqn{\Phi^2_\mathbf{C}} (the square of the quantity) is known as the \dQuote{dependence index.}  A generalization (Nelsen, 2006) to \eqn{L_p} distances from independence through the \code{LpCOP} function is
\deqn{L_p \equiv \Phi(p)_\mathbf{C} = \biggl(k(p)\int\!\!\int_{\mathcal{I}^2} |\mathbf{C}(u,v) - uv|^p\,\mathrm{d}u\mathrm{d}v\biggr)^{1/p}\mbox{,}}
for a \eqn{p: 1 \le p \le \infty} and where \eqn{k(p)} is a normalization constant such that \eqn{\Phi(p)_\mathbf{C} = 1} when the copula \eqn{\mathbf{C}} is \eqn{\mathbf{M}} (see \code{\link{M}}) or \eqn{\mathbf{W}} (see \code{\link{W}}). The \eqn{k(p)} is given (Nelson, 2006, Exercise 5.44, p. 213) in terms of the complete gamma function \eqn{\Gamma(t)} by
\deqn{k(p) = \frac{\Gamma(2p+3)}{2[\Gamma(p + 1)]^2}\mbox{,}}
which is implemented by the \code{hoefCOP} function. It is important to realize that the \eqn{L_p} distances are symmetric nonparametric measures of dependence (Nelsen, 2006, p. 210). These are symmetric because distance from independence is used as evident by \eqn{uv} in the above definitions.

Asymmetric forms similar to the above distances exist. Joe (2015, p. 65) however shows two measures of bivariate \emph{reflection asymmetry} or \emph{radial asymmetry} (term favored in \pkg{copBasic}) as the distance between the copula and the survival copula \code{\link{surCOP}}
\deqn{L_\infty^{(\mathrm{radsym})} = \mathrm{sup}_{0\le u,v\le1}|\mathbf{C}(u,v) - \hat\mathbf{C}(u,v)|\mbox{,}}
or its \eqn{L_p^{(\mathrm{radsym})}} counterpart
\deqn{L_p^{(\mathrm{radsym})} = \biggl[\int\!\!\int_{\mathcal{I}^2} |\mathbf{C}(u,v) - \hat\mathbf{C}(u,v)|^p\,\mathrm{d}u\mathrm{d}v\biggr]^{1/p}\mbox{with\ } p \ge 1\mbox{.}}
Joe (2015) does not seem to discuss and normalization constants for these alternative distances. Joe (2015, p. 66) offers analogous measures of bivariate \emph{permutation asymmetry} (\eqn{\mathbf{C}(u,v) = \mathbf{C}(v,u)}) defined as
\deqn{L_\infty^{(\mathrm{permsym})} = \mathrm{sup}_{0\le u,v\le1}|\mathbf{C}(u,v) - \hat\mathbf{C}(u,v)|\mbox{,}}
or its \eqn{L_p^{(\mathrm{permsym})}} counterpart
\deqn{L_p^{(\mathrm{permsym})} = \biggl[\int\!\!\int_{\mathcal{I}^2} |\mathbf{C}(u,v) - \hat\mathbf{C}(u,v)|^p\,\mathrm{d}u\mathrm{d}v\biggr]^{1/p}\mbox{with\ } p \ge 1\mbox{.}}
Again, Joe (2015) does not seem to discuss and normalization constants for these alternative distances. The two asymmetrical measures of Joe are nonnegative numbers and evidently unbounded whose potential upper bounds depends on \eqn{p}. Yet in a previous paragraph likely concerning \eqn{L_\infty^{(\mathrm{radsym})}}, Joe indicates and upper bounds of 1/3.  The numerical integrations for \eqn{L_p^{(\mathrm{radsym})}} and \eqn{L_p^{(\mathrm{permsym})}} can readily return zeros and often inspection of the formula for the copula itself would be sufficient to judge whether symmetry exists and hence the distances are uniquely zero.
}
\usage{
hoefCOP(cop=NULL, para=NULL, p=2, brute=FALSE, delta=0.002, ...)

LpCOP(cop=NULL, para=NULL, p=2, brute=FALSE, delta=0.002, ...)

LpCOPradsym(cop=NULL, para=NULL, p=2, brute=FALSE, delta=0.002, ...)

LpCOPpermsym(cop=NULL, para=NULL, p=2, brute=FALSE, delta=0.002, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{p}{The value for \eqn{p} as described above with a default to 2 to match the discussion of Nelsen (2006) and \dQuote{Hoeffding's Phi} of Cherubini \emph{et al.} (2004);}
  \item{brute}{Should brute force be used instead of two nested \code{integrate()} functions in \R to perform the double integration;}
  \item{delta}{The \eqn{\mathrm{d}u} and \eqn{\mathrm{d}v} for the brute force (\code{brute=TRUE}) integration; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for \eqn{\Phi(p)_\mathbf{C}} is returned.
}
\note{
Concerning the distance from independence, when \eqn{p = 1}, then Spearman's Rho (\code{\link{rhoCOP}}) of a copula is computed where is it seen in that documentation that the \eqn{k(p = 1) = 12}. The respective values of \eqn{k(p)} for select integers \eqn{p} are
\deqn{p \mapsto [1, 2, 3, 4, 5] \equiv k(p) \mapsto \{12, 90, 560, 3150, 16600\}\mbox{,}}
and these values are hardwired into \code{hoefCOP} and \code{LpCOP}. The integers for \eqn{k_p} ensures that the equality in the second line of the examples is \code{TRUE}.  But the \eqn{p} can be a noninteger. Nelsen (2006, p. 211) reports that when \eqn{p = \infty} that \eqn{L_\infty} is
\deqn{L_\infty \equiv \Phi(\infty)_\mathbf{C} = \Lambda_\mathbf{C} = 4\;\mathrm{sup}_{u,v \in \mathcal{I}}|\mathbf{C}(u,v) - uv|\mbox{.}}
}
\references{
Cherubini, U., Luciano, E., and Vecchiato, W., 2004, Copula methods in finance: Hoboken, NJ, Wiley, 293 p.

Joe, H., 2015, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{rhoCOP}}, \code{\link{wolfCOP}}
}
\examples{
hoefCOP(cop=PSP) # 0.4547656
rhoCOP(cop=PSP) == hoefCOP(cop=PSP, p=1) # TRUE
LpCOP(cop=PLACKETTcop, para=1.6, p=2.6)  # Fractional p
\dontrun{
set.seed(938) # Phi(1.6; Plackett) = 0.1184489; L_1 = 0.1168737
UV <- simCOP(cop=PLACKETTcop, para=1.6, n=2000, ploton=FALSE, points=FALSE)
hoefCOP(cop=PLACKETTcop, para=1.6, p=200)  # Large p near internal limits
L_1 <- 4*max(abs(PLACKETTcop(UV$U, UV$V, para=1.6) - UV$U*UV$V)) # p is infty
# and finite n and arguably a sample-like statistic here, now on intuition try
# a more sample-like means
U <- runif(10000); V <- runif(10000)
L_2 <- 4*max(abs(EMPIRcop(U, V, para=UV) - U*V)) # 0.1410254 (not close enough)}
\dontrun{
para <- list(alpha=0.15, beta=0.90, kappa=0.06, gamma=0.96,
             cop1=GHcop, cop2=PLACKETTcop, para1=5.5, para2=0.07)
LpCOPradsym( cop=composite2COP, para=para)
LpCOPpermsym(cop=composite2COP, para=para)}
\dontrun{
"MOcop.formula" <- function(u,v, para=para, ...) {
   alpha <- para[1]; beta <- para[2]; return(min(v*u^(1-alpha), u*v^(1-beta)))
}
"MOcop" <- function(u,v, ...) { asCOP(u,v, f=MOcop.formula, ...) }
 LpCOPradsym(cop=MOcop, para=c(.8,.5))
 LpCOPpermsym(cop=MOcop, para=c(.8,.5))}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{dependence index}
\keyword{Lp distance}
\keyword{Nelsen (2006) Examples and Exercises}

