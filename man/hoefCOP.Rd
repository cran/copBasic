\encoding{utf8}
\name{hoefCOP}
\alias{hoefCOP}
\alias{LpCOP}
\alias{LpCOPradsym}
\alias{LpCOPpermsym}
\title{ The Hoeffding Phi of a Copula or Lp Distances (Independence, Radial Asymmetry, or Reflection Symmetry Forms) }
\description{
Compute the measure of association known as the \emph{Hoeffding Phi} \eqn{\Phi_\mathbf{C}} of a copula from \emph{independence} (\eqn{uv = \mathbf{\Pi}}; \code{\link{P}}) according to Cherunbini \emph{et al.} (2004, p. 164) by
\deqn{\Phi_\mathbf{C} = 3 \sqrt{10\int\!\!\int_{\mathcal{I}^2} \bigl(\mathbf{C}(u,v) - uv\bigr)^2\,\mathrm{d}u\mathrm{d}v}\mbox{,}}
and Nelsen (2006, p. 210) shows this as
\deqn{\Phi_\mathbf{C} = \biggl(90\int\!\!\int_{\mathcal{I}^2} |\mathbf{C}(u,v) - uv|^2\,\mathrm{d}u\mathrm{d}v\biggr)^{1/2}\mbox{,}}
for which \eqn{\Phi^2_\mathbf{C}} (the square of the quantity) is known as the \emph{dependence index}. \enc{Gaißer}{Gaisser} \emph{et al.} (2010, eq. 1) have \eqn{\Phi^2_\mathbf{C}} as the \emph{Hoeffding Phi-Square}, and their definition, when square-rooted, matches Nelsen's listing.

A generalization (Nelsen, 2006) to \eqn{L_p} distances from independence (\eqn{uv = \mathbf{\Pi}}; \code{\link{P}}) through the \code{LpCOP} function is
\deqn{L_p \equiv \Phi_\mathbf{C}(p) = \biggl(k(p)\int\!\!\int_{\mathcal{I}^2} |\mathbf{C}(u,v) - uv|^p\,\mathrm{d}u\mathrm{d}v\biggr)^{1/p}\mbox{,}}
for a \eqn{p: 1 \le p \le \infty} and where \eqn{k(p)} is a normalization constant such that \eqn{\Phi_\mathbf{C}(p) = 1} when the copula \eqn{\mathbf{C}} is \eqn{\mathbf{M}} (see \code{\link{M}}) or \eqn{\mathbf{W}} (see \code{\link{W}}). The \eqn{k(p)} (bivariate definition only) for other powers is given (Nelsen, 2006, exer. 5.44, p. 213) in terms of the \emph{complete gamma function} \eqn{\Gamma(t)} by
\deqn{k(p) = \frac{\Gamma(2p+3)}{2[\Gamma(p + 1)]^2}\mbox{,}}
which is implemented by the \code{hoefCOP} function. It is important to realize that the \eqn{L_p} distances are all \emph{symmetric nonparametric measures of dependence} (Nelsen, 2006, p. 210). These are symmetric because distance from independence is used as evident by \dQuote{\eqn{uv}} in the above definitions.

\emph{Reflection/Radial and Permutation Asymmetry}---Asymmetric forms similar to the above distances exist. Joe (2014, p. 65) shows two measures of bivariate \emph{reflection asymmetry} or \emph{radial asymmetry} (term favored in \pkg{copBasic}) as the distance between \eqn{\mathbf{C}(u,v)} and the survival copula \eqn{\hat{\mathbf{C}}(u,v)} (\code{\link{surCOP}}) measured by
\deqn{L_\infty^{(\mathrm{radsym})} = \mathrm{sup}_{0\le u,v\le1}|\mathbf{C}(u,v) - \hat{\mathbf{C}}(u,v)|\mbox{,}}
or its \eqn{L_p^{(\mathrm{radsym})}} counterpart
\deqn{L_p^{(\mathrm{radsym})} = \biggl[\int\!\!\int_{\mathcal{I}^2} |\mathbf{C}(u,v) - \hat{\mathbf{C}}(u,v)|^p\,\mathrm{d}u\mathrm{d}v\biggr]^{1/p}\mbox{with\ } p \ge 1\mbox{,}}
where \eqn{\hat{\mathbf{C}}(u,v) = u + v - 1 + \mathbf{C}(1-u, 1-v)} and again \eqn{p: 1 \le p \le \infty}. Joe (2014) does not seem to discuss and normalization constants for these two radial asymmetry distances.

Joe (2014, p. 66) offers analogous measures of bivariate \emph{permutation asymmetry} (\eqn{\mathbf{C}(u,v) \not= \mathbf{C}(v,u)}) defined as
\deqn{L_\infty^{(\mathrm{permsym})} = \mathrm{sup}_{0\le u,v\le1}|\mathbf{C}(u,v) - \hat{\mathbf{C}}(v,u)|\mbox{,}}
or its \eqn{L_p^{(\mathrm{permsym})}} counterpart
\deqn{L_p^{(\mathrm{permsym})} = \biggl[\int\!\!\int_{\mathcal{I}^2} |\mathbf{C}(u,v) - \hat{\mathbf{C}}(v,u)|^p\,\mathrm{d}u\mathrm{d}v\biggr]^{1/p}\mbox{with\ } p \ge 1\mbox{,}}
where \eqn{p: 1 \le p \le \infty}. Again, Joe (2014) does not seem to discuss and normalization constants for these two permutation symmetry distances. Joe (2014, p. 65) states that the \dQuote{simplest one-parameter bivariate copula families [and] most of the commonly used two-parameter bivariate copula families are permutation symmetric.}

The asymmetrical \eqn{L_\infty} and \eqn{L_p} measures identified by Joe (2014, p. 66) are nonnegative with an upper bounds that depends on \eqn{p}. The bound dependence on \eqn{p} is caused by the lack of normalization constant \eqn{k(p)}. In an earlier paragraph, Joe (2014) indicates an upper bounds of 1/3 for both (likely?) concerning \eqn{L_\infty^{(\mathrm{radsym})}} and \eqn{L_\infty^{(\mathrm{permsym})}}. The numerical integrations for \eqn{L_p^{(\mathrm{radsym})}} and \eqn{L_p^{(\mathrm{permsym})}} can readily return zeros and often inspection of the formula for the \eqn{\mathbf{C}(u,v)} itself would be sufficient to judge whether symmetry exists and hence the distances are uniquely zero.

Joe (2014, p. 66) completes the asymmetry discussion with three definitions of skewness of combinations of random variables \eqn{U} and \eqn{V}: Two definitions are in \code{\link{uvlmoms}} (for \eqn{U + V - 1} and \eqn{U - V}) and two are for \eqn{V-U} (\code{\link{nuskewCOP}}) and \eqn{U+V-1} (\code{\link{nustarCOP}}).
}
\usage{
hoefCOP(     cop=NULL, para=NULL, p=2, as.sample=FALSE,
                                       sample.as.prob=TRUE,
                                       brute=FALSE, delta=0.002, ...)

LpCOP(       cop=NULL, para=NULL, p=2, brute=FALSE, delta=0.002, ...)
LpCOPradsym( cop=NULL, para=NULL, p=2, brute=FALSE, delta=0.002, ...)
LpCOPpermsym(cop=NULL, para=NULL, p=2, brute=FALSE, delta=0.002, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{p}{The value for \eqn{p} as described above with a default to 2 to match the discussion of Nelsen (2006) and the \emph{Hoeffding Phi} of Cherubini \emph{et al.} (2004). Do not confuse \eqn{p} with \eqn{d} described in \bold{Note};}
  \item{as.sample}{A logical controlling whether an optional \R \code{data.frame} in \code{para} is used to compute the \eqn{\hat{\Phi}_\mathbf{C}} (see \bold{Note}). If set to \code{-1}, then the message concerning CPU effort will be surpressed;}
  \item{sample.as.prob}{When \code{as.sample} triggered, what are the units incoming in \code{para}? If they are probabilities, the default is applicable. If they are not, then the columns are re-ranked and divided simply by \eqn{1/n}---more sophisticated \emph{empirical copula} probabilities are not used (\code{\link{EMPIRcop}});}
  \item{brute}{Should brute force be used instead of two nested \code{integrate()} functions in \R to perform the double integration;}
  \item{delta}{The \eqn{\mathrm{d}u} and \eqn{\mathrm{d}v} for the brute force (\code{brute=TRUE}) integration; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for \eqn{\Phi_\mathbf{C}(p)} is returned.
}
\note{
Concerning the distance from independence, when \eqn{p = 1}, then the \emph{Spearman Rho} (\code{\link{rhoCOP}}) of a copula is computed where is it seen in that documentation that the \eqn{k_p(1) = 12}. The respective values of \eqn{k(p)} for select integers \eqn{p} are
\deqn{p \mapsto [1, 2, 3, 4, 5] \equiv k(p) \mapsto \{12, 90, 560, 3150, 16600\}\mbox{,}}
and these values are hardwired into \code{hoefCOP} and \code{LpCOP}. The integers for \eqn{k_p} ensures that the equality in the second line of the examples is \code{TRUE}, but the \eqn{p} can be a noninteger as well. Nelsen (2006, p. 211) reports that when \eqn{p = \infty} that \eqn{L_\infty} is
\deqn{L_\infty \equiv \Phi_\mathbf{C}(\infty) = \Lambda_\mathbf{C} = 4\;\mathrm{sup}_{u,v \in \mathcal{I}}|\mathbf{C}(u,v) - uv|\mbox{.}}

A sample \eqn{\hat{\Phi}_\mathbf{C}} (square root of the \emph{Hoeffding Phi-Square}) based on nonparametric estimation generalized for \eqn{d} dimensions (\eqn{d = 2} for bivariate) is presented by \enc{Gaißer}{Gaisser} \emph{et al.} (2010, eq. 10) for estimated probabilities \eqn{\hat{U}_{ij}} for the \eqn{i}th dimension and \eqn{j}th row (observation) for sample of size \eqn{n}. Those authors suggest that the \eqn{\hat{U}_{ij}} be estimated from the empirical copula:
\deqn{\hat\Phi_\mathbf{C} = \sqrt{h(d)[A + B]}\mbox{,}}
where
\deqn{A = \biggl(\frac{1}{n}\biggr)^2\sum_{j=1}^n\sum_{k=1}^n\prod_{i=1}^d
                    \bigl[1 - \mathrm{max}\bigl(\hat{U}_{ij}, \hat{U}_{ik}\bigr)\bigr]\mbox{,}}
\deqn{B = \biggl(\frac{1}{3}\biggr)^d - \biggl(\frac{2}{n}\biggr)\biggl(\frac{1}{2}\biggr)^d
               \sum_{j=1}^n\prod_{i=1}^d  [1 - \hat{U}_{ij}^2]\mbox{, and}}
the normalization constant is a function of dimension and is
\deqn{h(d)^{-1} = \frac{2}{(d+1)(d+2)} - \biggl(\frac{1}{2}\biggr)^d\frac{d!}{\prod_{i=0}^d\bigl(i+(1/2)\bigr)}+\biggl(\frac{1}{3}\biggr)^d\mbox{.}}
\preformatted{
  set.seed(1); UV <- simCOP(n=1000, cop=PSP)
  hoefCOP(cop=PSP)                                       # 0.4547656 (theo.)
  hoefCOP(para=UV, as.sample=TRUE)                       # 0.4892757
  set.seed(1); UV <- simCOP(n=1000, cop=PSP, snv=TRUE) # std normal variates
  hoefCOP(para=UV, as.sample=TRUE, sample.as.prob=FALSE) # 0.4270324
}
}
\references{
Cherubini, U., Luciano, E., and Vecchiato, W., 2004, Copula methods in finance: Hoboken, NJ, Wiley, 293 p.

\enc{Gaißer}{Gaisser}, S., Ruppert, M., and Schmid, F., 2010, A multivariate version of Hoeffding's Phi-Square: Journal of Multivariate Analysis, v. 101, no. 10, pp. 2571--2586.

Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{blomCOP}}, \code{\link{footCOP}}, \code{\link{giniCOP}},
         \code{\link{rhoCOP}}, \code{\link{tauCOP}}, \code{\link{wolfCOP}},
         \code{\link{joeskewCOP}}, \code{\link{uvlmoms}}
}
\examples{
\dontrun{
# Example (ii) Gaisser et al. (2010, p. 2574)
Theta <- 0.66 # Phi^2 = Theta^2 ---> Phi == Theta as shown
hoefCOP(cop=convex2COP, para=c(alpha=Theta, cop1=M, cop2=P)) # 0.6599886

rhoCOP(cop=PSP) == hoefCOP(cop=PSP, p=1) # TRUE
LpCOP(cop=PLACKETTcop, para=1.6, p=2.6)  # 0.1445137 (Fractional p)}

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
LpCOPradsym( cop=composite2COP, para=para) # 0.02071164
LpCOPpermsym(cop=composite2COP, para=para) # 0.01540297}

\dontrun{
"MOcop.formula" <- function(u,v, para=para, ...) {
   alpha <- para[1]; beta <- para[2]; return(min(v*u^(1-alpha), u*v^(1-beta)))
}
"MOcop" <- function(u,v, ...) { asCOP(u,v, f=MOcop.formula, ...) }
   LpCOPradsym( cop=MOcop, para=c(.8,.5)) # 0.0261843
   LpCOPpermsym(cop=MOcop, para=c(.8,.5)) # 0.0243912 }
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{dependence index}
\keyword{Lp distance}
\keyword{Nelsen (2006) Examples and Exercises}
\concept{Hoeffding Phi}
\concept{Hoeffding Phi-Square}
