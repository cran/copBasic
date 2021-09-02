\encoding{utf8}
\name{tEVcop}
\alias{tEVcop}
\title{ The t-EV (Extreme Value) Copula}
\description{
The \emph{t-EV copula} (Joe, 2014, p. 189) is a limiting form of the \emph{t-copula} (multivariate t-distribution):
\deqn{
\mathbf{C}_{\rho,\nu}(u,v) = \mathbf{tEV}(u,v; \rho, \nu) =
\mathrm{exp}\bigl(-(x+y)B(x/(x+y); \rho, \nu)\bigr)\mbox{,}
}
where \eqn{x = -\log(u)}, \eqn{y = -\log(v)}, and letting \eqn{\eta = \sqrt{(\nu+1)/(1-\rho^2)}} define
\deqn{
B(w; \rho, \nu) = wT_{\nu+1}\bigl(\eta[(w/[1-w])^{1/\nu}-\rho]\bigr) + (1-w)T_{\nu+1}\bigl(\eta[([1-w]/w)^{1/\nu}-\rho]\bigr)\mbox{,}
}
where \eqn{T_{\nu+1}} is the cumulative distribution function of the \emph{univariate t-distribution} with \eqn{\nu-1} degrees of freedom. As \eqn{\nu \rightarrow \infty}, the copula weakly converges to the \emph{\enc{Hüsler}{Husler}--Reiss copula} (\code{\link{HRcop}}) because the t-distribution converges to the normal (see \bold{Examples} for a study of this copula).

The \eqn{\mathbf{tEV}(u,v; \rho, \nu)} copula is a two-parameter option when working with extreme-value copula. There is a caveat though. Demarta and McNeil (2004) conclude that \dQuote{the parameter of the Gumbel [\code{\link{GHcop}}] or Galambos [\code{\link{GLcop}}] A-functions [the Pickend dependence function and B-function by association] can always be chosen so that the curve is extremely close to that of the t-EV A-function for any values of \eqn{\nu} and \eqn{\rho}. The implication is that in all situations where the t-EV copula might be deemed an appropriate model we can work instead with the simpler Gumbel or Galambos copulas.}
}
\usage{
tEVcop(u, v, para=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (two element) of parameters in \eqn{\rho} and \eqn{\nu} order; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned.
}
\note{
Note that Joe (2014) shows \eqn{x = \log(u)} (note absence of the minus sign). This is not correct.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Demarta, S., and McNeil, A.J., 2004, The t copula and related copulas: International Statistical Review, v. 33, no. 1, pp. 111--129, \doi{10.1111/j.1751-5823.2005.tb00254.x}
}
\author{ W.H. Asquith}
\seealso{ \code{\link{GHcop}}, \code{\link{GLcop}}, \code{\link{HRcop}}
}
\examples{
\dontrun{
tau <- 1/3 # Example from copula::evCopula.Rd
tev.cop <- copula::tevCopula(copula::iTau(copula::tevCopula(), tau))
copula::pCopula(c(0.1,.5), copula=tev.cop)         # 0.07811367
tEVcop(0.1, 0.5, para=slot(tev.cop, "parameters")) # 0.07811367}

\dontrun{
nsim <- 2000; pargh <- c(5, 0.5, 0.5)
UV <- simCOP(nsim, cop=GHcop, para=pargh)
U <- lmomco::pp(UV[,1], sort=FALSE)
V <- lmomco::pp(UV[,2], sort=FALSE)
RT <- mleCOP(u=U, v=V, cop=tEVcop, init.para=c(0.5,log(4)),
             parafn=function(k) return(c(k[1], exp(k[2]))))
partev <- RT$para

FT <- simCOP(nsim, cop=tEVcop, para=RT$para)

tauCOP(cop=GHcop,  para=pargh )
tauCOP(cop=tEVcop, para=partev)

tauCOP(cop=GHcop,  para=pargh ) # [1] 0.3003678
tauCOP(cop=tEVcop, para=partev) # [1] 0.3178904

densityCOPplot(cop=GHcop,  para=pargh)
densityCOPplot(cop=tEVcop, para=partev,
               ploton=FALSE, contour.col=2) #}

\dontrun{
# A demonstration Joe (2014, p. 190) for which tEvcop() has
# upper tail dependence parameter as
para <- c(0.8, 10)
lamU <- 2*pt(-sqrt((para[2]+1)*(1-para[1])/(1+para[1])), para[2]+1)
"tEVcop.copula" <- function(u,v, para=NULL, ...) {
      if(length(u)==1) u<-rep(u,length(v)); if(length(v)==1) v<-rep(v,length(u))
      return(copula::pCopula(matrix(c(u,v), ncol=2),
                   tevCopula(param=para[1], df=para[2])))
}
lamU.copBasic <- taildepCOP(cop=tEVcop,        para)$lambdaU
lamU.copula   <- taildepCOP(cop=tEVcop.copula, para)$lambdaU
print(c(lamU, lamU.copBasic, lamU.copula))
#[1] 0.2925185 0.2925200 0.2925200 # So we see that they all match. }

\dontrun{
# Convergence of tEVcop to HRcop as nu goes to infinity.
nu <- 10^(seq(-4,2,by=.1)) # nu right end point rho dependent
rho <- 0.7 # otherwise, expect to see 'zeros' errors on the plot()
# Compute Blomqvist Beta (fast computation is reason for choice)
btEV <- sapply(nu, function(n) blomCOP(tEVcop, para=c(rho, n)))
limit.thetas <- sqrt(2/(nu*(1-rho))) # for nu --> infinity HRcop
thetas <- sapply(btEV, function(b) {
     uniroot(function(l, blom=NA) { blom - blomCOP(HRcop, para=l) },
     interval=c(0,10), blom=b)$root })
plot(limit.thetas, thetas, log="xy", type="b",
     xlab="Theta of HRcop via limit nu --> infinity",
     ylab="Theta from Blomqvist Beta equivalent HRcop to tEVcop")
abline(0,1)
mtext(paste0("Note 'weak' convergence to lower left, and \n",
             "convergence increasing with rho"))
# Another reference of note
# https://mediatum.ub.tum.de/doc/1145695/1145695.pdf (p.39) #}
}
\keyword{t-EV copula}
\keyword{copula (formulas)}
\keyword{copula}
\keyword{package copula (comparison to)}
\concept{t-EV copula}
\keyword{copula (extreme value)}
\keyword{extreme value copula}
\keyword{Joe (2014) Examples and Exercises}
