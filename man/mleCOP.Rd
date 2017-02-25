\name{mleCOP}
\alias{mleCOP}
\title{ Maximum Pseudo-Log-Likelihood Estimation for Copula Parameter Estimation }
\description{
Perform maximum pseudo-log-likelihood estimation (pMLE) for copula parameters by maximizing the function:
\deqn{\mathcal{L}(\Theta_p) = \sum_{i=1}^n \log\bigl[ c(F_x(x_i), F_y(y_i); \Theta_p)\bigr]\mbox{,}}
where \eqn{\mathcal{L}(\Theta_p)} is the log-likelihood for parameter vector \eqn{\Theta_p} of dimension \eqn{p}, and \eqn{c(u,v; \Theta_p)} is the bivariate copula density. The \eqn{u} and \eqn{v} are estimated by the respective empirical cumulative distribution functions \eqn{u = F_x(\cdots)} and \eqn{v = F_y(\cdots)} for each of the joint realizations of a sample of size \eqn{n}. The \eqn{c(u,v)} is numerically estimated by the copula using the \code{\link{densityCOP}} function.
}
\usage{
mleCOP(u, v=NULL, cop=NULL, parafn=function(k) return(k),
          interval=NULL, init.para=NULL, verbose=FALSE, control=list(),
          the.zero=.Machine$double.eps^0.25, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction and if \code{NULL} then \code{u} is treated as a two column \R \code{data.frame};}
  \item{cop}{A copula function;}
  \item{parafn}{A function responsible for generating the parameters. This is often just a simple return of a parameter vector as \pkg{copBasic} uses this style of parameterization, but this function can take over parameter remapping to handle boundary conditions to benefit the search or provide an interface into other copula packages in \R (see \bold{Examples});}
  \item{interval}{The search interval for root finding if the parameter dimension of the copula is \eqn{p = 1}. The interval is not used for \eqn{p \ge 2};}
  \item{init.para}{The initial guesses for the parameters for the \eqn{p}-dimensional optimization for \eqn{p \ge 2}. These are not used if the parameter dimension of the copula is \eqn{p = 1};}
  \item{verbose}{A logical to trigger an minimal reporting scheme within the objective
function. This is independent from the \code{control$trace} of function \code{optim()};}
  \item{control}{This argument is the argument of the same name for \code{optim()};}
  \item{the.zero}{The value for \dQuote{the zero} of the copula density function. This argument is the argument of the same name for \code{\link{densityCOP}}. The default here is intended to suggest that a tiny nonzero value for density will trap the numerical zero densities; and}
  \item{...}{Additional arguments to pass, see source code for the internally used functions that can pick these additional arguments up.}
}
\value{
  The value(s) for the estimated parameters are returned within an \R \code{list} where the elements listed below are populated unique to this package. The other elements of the returned list are generated from either the \code{optimise()} or \code{optim()} functions of \R.
  \item{para}{The parameter(s) in a canonical element after the one-dimensional root finding (\eqn{p = 1}) or multi-dimensional optimization (\eqn{p \ge 2}) solutions are passed through \code{parafn};}
  \item{packagetext}{A helpful message unique to the \pkg{copBasic} package;}
  \item{loglik}{The maximum of the log-likelihood matching the name for the same quantity by the function \code{fitCopula} in package \pkg{copula} though a separate implementation is used in \pkg{copBasic};}
  \item{AIC}{Akaike information criterion (AIC): \eqn{AIC = 2p - 2\mathcal{L}(\Theta_p)}; and}
  \item{BIC}{Bayesian information criterion (BIC): \eqn{BIC = p\log(n) - 2\mathcal{L}(\Theta_p)}.}
}
\note{
This section provides for a more thorough assessment of pseudo-log-likelihood parameter estimation (pMLE) than shown in the \bold{Examples}.

\emph{INTERFACE TO THE \pkg{COPULA} PACKAGE}---A not uncommon question to the author is how can \pkg{copBasic} support copulas from other packages?  A \pkg{copBasic} pMLE implementation to the \emph{Gaussian copula} from the \pkg{copula} package is thus useful.

Two interface functions are required for the pMLE situation. First, interface the \pkg{copula} package in a generic form for the \pkg{copBasic} package:
\preformatted{
  "cB2copula" <-  # pCoupla() from package copula is analogous to COP()
  function(u,v, para=NULL, ...) {
    if(length(u) == 1) u <- rep(u, length(v)) # see asCOP() for reasoning of
    if(length(v) == 1) v <- rep(v, length(u)) # this "vectorization" hack
    return(copula::pCopula(matrix(c(u,v), ncol=2), para))
  }
}
where the \code{para} argument above must be built by the features of the \pkg{copula} package. The following function then provides for parameter setup specific to the \emph{Gaussian copula} having parameter \eqn{\rho}:
\preformatted{
  copula2cBpara <- function(rho) return(copula::normalCopula(rho, dim = 2))
}
Now, let us perform a parameter estimate for a sample of size \eqn{n=900}:
\preformatted{
  set.seed(162); UV <- simCOP(n=900, cop=cB2copula, para=copula2cBpara(0.45))
  mleCOP(UV, cop=cB2copula, parafn=copula2cBpara, interval=c(-1,1))$para
  #   rho.1  =  0.4248822 
}
The search interval for the \emph{Gaussian copula} is \eqn{\rho \in [-1, 1]}, and the final result is \eqn{\rho = 0.4458822}.

\emph{MULTI-DIMENSIONAL pMLE}---Consider a 2-parameter \emph{Gumbel-Hougaard copula} (\eqn{\mathbf{GH}(\Theta_1, \Theta_2)}) but now use the \code{parafn} argument to provide boundary condition assistance through function \code{GH2pfunc} to the \code{optim()} function that performs the maximization. 
\preformatted{
  set.seed(162); UV <- simCOP(n=890, cop=GHcop, para=c(2.4, .06))
  GH2pfunc <- function(p) { return(c(exp(p[1])+1, exp(p[2]))) }
  ML <- mleCOP(UV$U, UV$V, cop=GHcop, init.para=c(1,1), parafn=GH2pfunc)
  print(ML$para) # [1] 2.2755018 0.1194788
}
and the result is \eqn{\Theta_{1,2} = (2.2755018, 0.1194788)}. Next, consider a 3-parameter \eqn{\mathbf{GH}(\Theta, \pi_1, \pi_2)} copula and again use the \code{parafn} argument through function \code{GH3pfunc}  but notice that the 2nd and 3rd parameters are now mapped into \eqn{0 \le \pi_1, \pi_2 \le 1} domain using the \code{pnorm()} function.
\preformatted{
  set.seed(162); UV <- simCOP(n=500, cop=GHcop, para=c(5.5, .6, .9))
  GH3pfunc <- function(p) { return(c(exp(p[1])+1, pnorm(p[2]), pnorm(p[3]))) }
  ML <- mleCOP(UV$U, UV$V, cop=GHcop, init.para=c(1, .5, .5), parafn=GH3pfunc)
  print(ML$para) # [1] 5.3742229 0.6141652 0.9382638
}
and the result is \eqn{\Theta = 5.3742229} and \eqn{\pi_{1,2} = (0.6141652, 0.9382638)}.

\emph{ANOTHER MULTI-DIMENSIONAL pMLE}---Finally, an experiment can be played to fit a 3-parameter \eqn{\mathbf{GH}(\Theta, \pi_1, \pi_2)} to a simulation from a 2-parameter \eqn{\mathbf{GH}(\Theta_1, \Theta_2)}, where the seed is just arbitrary and \emph{Vuong's Procedure} (\code{\link{vuongCOP}}) is used to compare fits and make inference. The parameter functions \code{GH2pfunc} and \code{GH3pfunc} are as before.
\preformatted{
  set.seed(10); UV <- simCOP(n=500, cop=GHcop, para=c(1.7, 1.86))
  GH2pfunc <- function(p) { return(c(exp(p[1])+1,   exp(p[2])              )) }
  GH3pfunc <- function(p) { return(c(exp(p[1])+1, pnorm(p[2]), pnorm(p[3]) )) }
  para1 <- mleCOP(UV, cop=GHcop, init.para=c(1,1),     parafn=GH2pfunc)$para
  para2 <- mleCOP(UV, cop=GHcop, init.para=c(1,.5,.5), parafn=GH3pfunc)$para
  vuongCOP(UV, cop1=GHcop, para1=para1, cop2=GHcop, para2=para2)$message
  #[1] "Copula 1 has better fit than Copula 2 at 100x(1-alpha) level"
}
The results show the 2-p \eqn{\mathbf{GH}} is a better fit to the simulated data than the 3-p \eqn{\mathbf{GH}}, which seems a bit self evident?  Plot some same seeded simulations just to see.
\preformatted{
  set.seed(67) # First the estimated parameters but with the correct model.
  UV <- simCOP(n=200, GHcop, para=para1, snv=TRUE, pch=16, col=2)
  set.seed(67) # Second, the estimated incorrect model.
  UV <- simCOP(n=200, GHcop, para=para2, snv=TRUE, ploton=FALSE)
}
Yes, differences in form are manifest in the produced graphic. Now, let us try another set of parameters and again an arbitrary seed.
\preformatted{
  set.seed(10); UV <- simCOP(n=500, cop=GHcop, para=c(1.91, .16))
  para1 <- mleCOP(UV, cop=GHcop, init.para=c(1,1),     parafn=GH2pfunc)$para
  para2 <- mleCOP(UV, cop=GHcop, init.para=c(1,.5,.5), parafn=GH3pfunc)$para
  vuongCOP(UV, cop1=GHcop, para1=para1, cop2=GHcop, para2=para2)$message
  #[1] "Copulas 1 and 2 are not significantly different at 100(1-alpha)"
}
The results show equivalence, let us now check a graphic.
\preformatted{
  set.seed(67); z <- simCOP(n=200, GHcop, para=para1, snv=TRUE, pch=16, col=2)
  set.seed(67); z <- simCOP(n=200, GHcop, para=para2, snv=TRUE, ploton=FALSE)
}
The differences are small but the differences might be inflating into the lower left corner. What sample size could conceivably begin to distinguish between the copula?
\preformatted{
  kullCOP(cop1=GHcop, cop2=GHcop, para1=para1, para2=para2) # 625 on this run

  nsim <- 20; set.seed(67)
  Results <- sapply(1:nsim, function(i) {
    UV <- simCOP(n=625, cop=GHcop, para=c(1.91, .16), graphics=FALSE)
    p1 <- mleCOP(UV, cop=GHcop, init.para=c(1,1),     parafn=GH2pfunc)$para
    p2 <- mleCOP(UV, cop=GHcop, init.para=c(1,.5,.5), parafn=GH3pfunc)$para
    vuongCOP(UV, cop1=GHcop, para1=p1, cop2=GHcop, para2=p2)$result })
  sum(Results)
}
The summation yields 6 of 20 for which copula 1 has the better fit, but with \eqn{n=1{,}000} instead of \eqn{n=625}, the sum of the \code{Results} is 13 of 20 (so better than half the time). This seems to be in conflict with what the \eqn{n_{fg}} sample size from \code{\link{kullCOP}} should be telling. The author thinks it should be 18 to 19 of 20 (95th percentile) based on what the \code{\link{kullCOP}} is reported to do (NEED TO LOOK INTO THIS).
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{densityCOP}}}
\examples{
\dontrun{
set.seed(162); UV <- simCOP(n=188, cop=PLACKETTcop, para=5.6)
ML <- mleCOP(UV$U, UV$V, cop=PLACKETTcop, interval=c(0.1,40)) # 5.225459 estimated

Thetas <- 10^(seq(log10(0.001),log10(100), by=.005))
MLs <- sapply(Thetas, function(k)
             densityCOP(UV$U, UV$V, cop=PLACKETTcop, para=k, sumlogs=TRUE))
plot(Thetas, MLs, log="x", type="l", # draw the pMLE solution process
     xlab="Plackett Theta", ylab="sum of log densities")
lines(rep(ML$para, 2), c(ML$objective, par()$usr[3]), col=2) # }

# See extended code listings and discussion in the Note section
}
\keyword{copula (fitting)}
\keyword{maximum likelihood}
\concept{Vuong procedure}
\concept{Akaike}
\concept{AIC}
\concept{BIC}
\concept{Vuong}
\concept{Kullback-Leibler}
\keyword{API to the copula package}
