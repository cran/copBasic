\encoding{utf8}
\name{lcomCOP}
\alias{lcomCOP}
\title{L-comoments and Bivariate L-moments of a Copula}
\description{
Compute the \emph{L-comoments} (Serfling and Xiao, 2007; Asquith, 2011) through the \emph{bivariate L-moments (ratios)} (\eqn{\delta^{[\ldots]}_{k;\mathbf{C}}}) of a copula \eqn{\mathbf{C}(u,v; \Theta)}  The L-comoments include \emph{L-correlation} (\emph{Spearman Rho}), \emph{L-coskew}, and \emph{L-cokurtosis}.  As described by Brahimi \emph{et al.} (2015), the first four bivariate L-moments \eqn{\delta^{[12]}_k} for random variable \eqn{X^{(1)}} or \eqn{U} with respect to (\emph{wrt}) random variable \eqn{X^{(2)}} or \eqn{V} are defined as
\deqn{\delta^{[12]}_{1;\mathbf{C}} = 2\int\!\!\int_{\mathcal{I}^2}
\mathbf{C}(u,v)\,\mathrm{d}u\mathrm{d}v - \frac{1}{2}\mbox{,}}
\deqn{\delta^{[12]}_{2;\mathbf{C}} = \int\!\!\int_{\mathcal{I}^2}
(12v - 6)
\mathbf{C}(u,v)\,\mathrm{d}u\mathrm{d}v - \frac{1}{2}\mbox{,}}
\deqn{\delta^{[12]}_{3;\mathbf{C}} = \int\!\!\int_{\mathcal{I}^2}
(60v^2 - 60v + 12)
\mathbf{C}(u,v)\,\mathrm{d}u\mathrm{d}v - \frac{1}{2}\mbox{, and}}
\deqn{\delta^{[12]}_{4;\mathbf{C}} = \int\!\!\int_{\mathcal{I}^2}
(280v^3 - 420v^2 + 180v - 20)
\mathbf{C}(u,v)\,\mathrm{d}u\mathrm{d}v - \frac{1}{2}\mbox{,}}
where the bivariate L-moments are related to the L-comoment ratios by
\deqn{6\delta^{[12]}_k = \tau^{[12]}_{k+1}\mbox{\quad and \quad}6\delta^{[21]}_k = \tau^{[21]}_{k+1}\mbox{,}}
where in otherwords, \dQuote{the third bivariate L-moment \eqn{\delta^{[12]}_3} is one sixth the L-cokurtosis \eqn{\tau^{[12]}_4}.} The first four bivariate L-moments yield the first five L-comoments. The terms and nomenclature are not easy and also the English grammar adjective \dQuote{ratios} is not always consistent in the literature. The \eqn{\delta^{[\ldots]}_{k;\mathbf{C}}} are \bold{ratios}. The sample L-comoments are supported by the \pkg{lmomco} package, and in particular for the bivariate case, they are supported by the \code{lcomoms2()} function of that package.

Similarly, the \eqn{\delta^{[21]}_k} are computed by switching \eqn{u \rightarrow v} in the polynomials within the above integrals multiplied to the copula in the system of equations with \eqn{u}. In general, \eqn{\delta^{[12]}_k \not= \delta^{[21]}_k} for \eqn{k > 1} unless in the case of \emph{permutation symmetric} (\code{\link{isCOP.permsym}}) copulas. By theory, \eqn{\delta^{[12]}_1 = \delta^{[21]}_1 = \rho_\mathbf{C}/6} where \eqn{\rho_\mathbf{C}} is the \emph{Spearman Rho} \code{\link{rhoCOP}}.

The integral for \eqn{\delta^{[12]}_{4;\mathbf{C}}} does not appear in Brahimi \emph{et al.} (2015) but this and the other forms are verified in the \bold{Examples} and discussion in \bold{Note}. The four \eqn{k \in (1,2,3,4)} for \eqn{U} \emph{wrt} \eqn{V} and \eqn{V} \emph{wrt} \eqn{U} comprise a full spectrum of system of seven (not eight) equations. One equation is lost because \eqn{\delta^{[12]}_1 = \delta^{[21]}_1}.

Chine and Benatia (2017) describe \emph{trimmed L-comoments} as the multivariate extensions of the univariate \emph{trimmed L-moments} (Elamir and Seheult, 2003) that are implemented in \pkg{lmomco}. These are not yet implemented in \pkg{copBasic}.
}
\usage{
lcomCOP(cop=NULL, para=NULL, as.bilmoms=FALSE, orders=2:5, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{as.bilmoms}{A logical to trigger return of the \eqn{\delta_k} and the return vectors will be named differently;}
  \item{orders}{The orders of the L-comoments to return, which is internally adjusted if the argument \code{as.bilmoms} is set. There is no first order L-comoment and the first index on returned values is set to \code{NA} to remain index consistent with the \pkg{lmomco} package. An order greater than 5 is not supported; and}
  \item{...}{Additional arguments to pass to the \code{\link{densityCOP}} function.}
}
\value{
  An \R \code{list} of the L-comoments or bivariate L-moments is returned depending on \code{as.bilmoms} setting.
  \item{bilmomUV}{The bivariate L-moments \eqn{\delta^{[12]}_k} of \eqn{U} with respect to \eqn{V} for \eqn{k \in [1,2,3,4]} if \code{orders} is \code{2:5} and there is no \code{NA} index as for the L-comoments;}
  \item{bilmomVU}{The bivariate L-moments \eqn{\delta^{[21]}_k} of \eqn{V} with respect to \eqn{U} for \eqn{k \in [1,2,3,4]} if \code{orders} is \code{2:5} and there is no \code{NA} index as for the L-comoments;}
  \item{lcomUV}{The L-comoments \eqn{\tau^{[12]}_k} of \eqn{V} with respect to \eqn{U} for \eqn{k \in [2,3,4,5]} if \code{orders} is \code{2:5} and index 1 is \code{NA}; and}
  \item{lcomVU}{The L-comoments \eqn{\tau^{[21]}_k} of \eqn{V} with respect to \eqn{U} for \eqn{k \in [2,3,4,5]} if \code{orders} is \code{2:5} and index 1 is \code{NA}.}
}
\note{
The documention here is highly parallel to \code{\link{bilmoms}} for which that function was developed some years before \code{lmomCOP} was developed in January 2019. Also, \code{\link{bilmoms}} is based on gridded or Monte Carlo integration, and \code{\link{bilmoms}} is to be considered \bold{deprecated}. However, it is deliberate that related background and various algorithm testing are still documented in \code{\link{bilmoms}}.
}
\references{
Asquith, W.H., 2011, Distributional analysis with L-moment statistics using the R environment for statistical computing: Createspace Independent Publishing Platform, ISBN 978--146350841--8.

Brahimi, B., Chebana, F., and Necir, A., 2015, Copula representation of bivariate L-moments---A new estimation method for multiparameter two-dimensional copula models: Statistics, v. 49, no. 3, pp. 497--521.

Chine, Amel, and Benatia, Fatah, 2017, Bivariate copulas parameters estimation using the trimmed L-moments methods: Afrika Statistika, v. 12, no. 1, pp. 1185--1197.

Elamir, E.A.H, and Seheult, A.H., 2003, Trimmed L-moments: Computational Statistics and Data Analysis, v. 43, p. 299--314.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Serfling, R., and Xiao, P., 2007, A contribution to multivariate L-moments---L-comoment matrices: Journal of Multivariate Analysis, v. 98, pp. 1765--1781.
}
\author{W.H. Asquith}
\seealso{\code{\link{bilmoms}}, \code{\link{lcomCOPpv}}, \code{\link{uvlmoms}}
}
\examples{
\dontrun{
para <- list(alpha=0.5, beta=0.93, para1=4.5, cop1=GLcop, cop2=PSP)
copBasic:::lcomCOP(cop=composite2COP, para=para)$lcomUV[3]
# Lcomom:T3[12]=  +0.156
copBasic:::lcomCOP(cop=composite2COP, para=para)$lcomVU[3]
# Lcomom:T3[21]=  -0.0668
bilmoms(cop=composite2COP, n=10000, para=para, sobol=TRUE)$bilcomoms$T3
# Tau3[12]=+0.1566, Tau3[21]=-0.0655
# The numerical default Monte Carlo integration of bilmoms()
# matches the numerical integration of lcomCOP albeit with a
# substantially slower and less elegant means in bilmoms().}

\dontrun{
# The following Spearman Rho and L-coskew values are predicted for a monitoring
# location of the relation between peak streamflow (V) and time into the
# water year (U) where U and V are "U-statistics."
site_srho  <-  0.15536430
site_T3_12 <-  0.03866056
site_T3_21 <- -0.03090144

# Create an objective function for 3D optimization with some explicit intention that
# transforms are used to keep the parameters in acceptable parameter space for the
# alpha [0,1] and beta [0,1] and theta for the Plackett copula and the composite1COP
# used to add two more parameters to the Plackett.
ofunc <- function(par, srho=NA, T3_12=NA, T3_21=NA) {
  alpha <- pnorm(par[1]) # takes -Inf to +Inf ---> 0 to 1 # compositing domain
  beta  <- pnorm(par[2]) # takes -Inf to +Inf ---> 0 to 1 # compositing domain
  theta <-   exp(par[3]) # takes -Inf to +Inf ---> 0 to +Inf # Plackett domain
  lmr <- lcomCOP(cop=composite1COP,
                 para=list(alpha=alpha, beta=beta, para1=theta, cop1=PLcop))
  return((lmr$lcomUV[2] - srho )^2 + # look carefully, the 2, 3, 3 index
         (lmr$lcomUV[3] - T3_12)^2 + # use on the lmr list are correct, so do not
         (lmr$lcomVU[3] - T3_21)^2)  # expect to see 1, 2, 3 or 2, 3, 4.
}
# initial parameter guess ('middle' [0.5] compositing and independence [1]) and
# showing the transformations involved.
para_init <- c(qnorm(0.5), qnorm(0.5), log(1))
rt <- optim(par=para_init, ofunc,
            srho=site_srho, T3_12=site_T3_12, T3_21=site_T3_21)
lcom_para <- list(alpha=pnorm(rt$par[1]), beta=pnorm(rt$par[2]),
                  para1=exp(rt$par[3]), cop1=PLcop)
sUV <- simCOP(10000, cop=composite1COP, para=lcom_para, col=grey(0, 0.2), pch=16)
# Now as an exercise, consider increasing site_srho or negating it. Consider
# switching the signs on the L-coskews or increasing their magnitudes and study
# the resulting simulation to develop a personal feeling for L-coskew meaning. #}
}
\keyword{bivariate asymmetry (measures)}
\keyword{bivariate skewness}
\keyword{bivariate L-moments}
\concept{bivariate L-correlation}
\concept{bivariate L-skew}
\concept{bivariate L-kurtosis}
\concept{copula L-comoments}
\concept{copula L-correlation}
\concept{copula L-coskew}
\concept{copula L-cokurtosis}
\keyword{L-comoments}
\concept{Lcomoments}
\keyword{Spearman Rho}
