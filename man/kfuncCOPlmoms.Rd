\encoding{utf8}
\name{kfuncCOPlmoms}
\alias{kfuncCOPlmoms}
\alias{kfuncCOPlmom}
\title{ The L-moments of the Kendall Function of a Copula }
\description{
Compute the L-moments of the \emph{Kendall Function} (\eqn{F_K(z; \mathbf{C})}) of a copula \eqn{\mathbf{C}(u,v)} where the \eqn{z} is the joint probability of the \eqn{\mathbf{C}(u,v)}. The Kendall Function is the cumulative distribution function (CDF) of the joint probability \eqn{Z} of the coupla. The expected value of the \eqn{z(F_K)} (mean, first L-moment \eqn{\lambda_1}), because \eqn{Z} has nonzero probability for \eqn{0 \le Z \le \infty}, is

\deqn{\mathrm{E}[Z] = \lambda_1 = \int_0^\infty [1 - F_K(t)]\,\mathrm{d}t = \int_0^1 [1 - F_K(t)] \,\mathrm{d}t\mbox{,}}

where for circumstances here \eqn{0 \le Z \le 1}. The \eqn{\infty} is mentioned only because expectations of such CDFs are usually shown using \eqn{(0,\infty)} limits, whereas integration of quantile functions (CDF inverses) use limits \eqn{(0,1)}. Because the support of \eqn{Z} is \eqn{(0,1)}, like the probability \eqn{F_K}, showing just it (\eqn{\infty}) as the upper limit could be confusing---statements such as ``probabilities of probabilities'' are rhetorically complex so pursuit of word precision is made herein.

An expression for \eqn{\lambda_r} for \eqn{r \ge 2} in terms of the \eqn{F_K(z)} is
\deqn{
\lambda_r = \frac{1}{r}\sum_{j=0}^{r-2} (-1)^j {r-2 \choose j}{r \choose j+1} \int_{0}^{1} \! [F_K(t)]^{r-j-1}\times [1 - F_K(t)]^{j+1}\, \mathrm{d}t\mbox{,}
}
where because of these circumstances the limits of integration are \eqn{(0,1)} and not \eqn{(-\infty, \infty)} as in the usual definition of L-moments in terms of a distribution's CDF.

The mean, L-scale, coefficient of L-variation (\eqn{\tau_2}, LCV, L-scale/mean), L-skew (\eqn{\tau_3}, TAU3), L-kurtosis (\eqn{\tau_4}, TAU4), and \eqn{\tau_5} (TAU5) are computed. In usual nomenclature, the L-moments are
\eqn{ \lambda_1 = \mbox{mean,}}
\eqn{ \lambda_2 = \mbox{L-scale,}}
\eqn{ \lambda_3 = \mbox{third L-moment,}}
\eqn{ \lambda_4 = \mbox{fourth L-moment, and}}
\eqn{ \lambda_5 = \mbox{fifth L-moment,}}
whereas the L-moment ratios are
\eqn{ \tau_2 = \lambda_2/\lambda_1 = \mbox{coefficient of L-variation, }}
\eqn{ \tau_3 = \lambda_3/\lambda_2 = \mbox{L-skew, }}
\eqn{ \tau_4 = \lambda_4/\lambda_2 = \mbox{L-kurtosis, and}}
\eqn{ \tau_5 = \lambda_5/\lambda_2 = \mbox{not named.}}
It is common amongst practitioners to lump the L-moment ratios into the general term \dQuote{L-moments} and remain inclusive of the L-moment ratios. For example, L-skew then is referred to as the 3rd L-moment when it technically is the 3rd L-moment ratio.  There is no first L-moment ratio (meaningless) so results from this function will canoncially show a \code{NA} in that slot. The coefficient of L-variation is \eqn{\tau_2} (subscript 2) and not \emph{Kendall's Tau} (\eqn{\tau}). Sample L-moments are readily computed by several packages in \R (\emph{e.g.} \pkg{lmomco}, \pkg{lmom}, \pkg{Lmoments}, \pkg{POT}).
}
\usage{
kfuncCOPlmom(r, cop=NULL, para=NULL, ...)

kfuncCOPlmoms(cop=NULL, para=NULL, nmom=5, begin.mom=1, ...)
}
\arguments{
  \item{r}{The \eqn{r}th order of a single L-moment to compute;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{nmom}{The number of L-moments to compute;}
  \item{begin.mom}{The \eqn{r}th order to begin the sequence \code{lambegr:nmom} for L-moment computation. The rarely used argument is means to bypass the computation of the mean if the user has an alternative method for the mean or other central tendency characterization in which case \code{begin.mom = 2}; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  An \R \code{list} is returned by \code{kfuncCOPlmoms} and only the scalar value of \eqn{\lambda_r} by \code{kfuncCOPlmom}.

  \item{lambdas}{Vector of the L-moments. First element is \eqn{\lambda_1}, second element is \eqn{\lambda_2}, and so on;}
  \item{ratios}{Vector of the L-moment ratios. Second element is \eqn{\tau}, third element is \eqn{\tau_3} and so on; and}
  \item{source}{An attribute identifying the computational source of the L-moments: \dQuote{kfuncCOPlmoms}.}
}
\note{
The L-moments of Kendall Functions appear to be not yet fully researched. An interesting research direction would be the trajectories of the L-moments or \emph{L-moment ratio diagrams} for the Kendall Function and the degree to which distinction between copulas becomes evident---such diagrams are in wide-spread use for distinquishing between univariate distributions. It is noted, however, that \emph{Kendall Function L-moment ratio diagrams} might be of less utility that in the univariate world because different copulas can have the same \eqn{F_K(z)}, such as all bivariate extreme value copulas.
\preformatted{
  Rhos <- seq(0,0.9,by=0.05)
  L1 <- T2 <- T3 <- T4 <- Thetas <- vector(mode="numeric", length(Rhos))
  for(i in 1:length(Thetas)) {
     Thetas[i] <- uniroot(function(p)
                 Rhos[i] - rhoCOP(cop=PARETOcop, para=p), c(0,100))$root
     message("Rho = ", Rhos[i], " and Pareto theta = ",
                                             round(Thetas[i], digits=4))
     lmr <- kfuncCOPlmoms(cop=PARETOcop, para=Thetas[i], nmom=4)
     L1[i] <- lmr$lambdas[1]; T2[i] <- lmr$ratios[2]
     T3[i] <- lmr$ratios[3];  T4[i] <- lmr$ratios[4]
  }
  LMR <- data.frame(Rho=Rhos, Theta=Thetas, L1=L1, T2=T2, T3=T3, T4=T4)
  plot(LMR$Rho, LMR$T2, ylim=c(-0.04, 0.5), xlim=c(0,1),
       xlab="Spearman's Rho or coefficient of L-variation",
       ylab="L-moment ratio", type="l", col=1) # black
  lines(LMR$Rho, LMR$T3, col=2)        # red
  lines(LMR$Rho, LMR$T4, col=3)        # green
  lines(LMR$T2,  LMR$T3, lty=2, col=4) # dashed blue
  lines(LMR$T2,  LMR$T4, lty=2, col=5) # dashed cyan
  lines(LMR$T3,  LMR$T4, lty=2, col=6) # dashed purple
}
}
\references{
Asquith, W.H., 2011, Distributional analysis with L-moment statistics using the R environment for statistical computing: Createspace Independent Publishing Platform, ISBN 978--146350841--8.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{kfuncCOP}} }
\examples{
\dontrun{
kfuncCOPlmom(1, cop=P) # 0.5 * 0.5 = 0.25 is expected joint prob. of independence
#[1] 0.2499999  (in agreement with theory)

ls.str(kfuncCOPlmoms(cop=GHcop, para=4.21)) # Gumbel-Hougaard copula
# lambdas :  num [1:5] 0.440617 0.169085 0.011228 -0.000797 0.000249
# ratios  :  num [1:5]       NA 0.38375  0.0664   -0.00472  0.00147
# source  :  chr "kfuncCOPlmoms"  # e.g. L-skew = 0.0664}

\dontrun{
UV <- simCOP(200, cop=PLcop, para=1/pi, graphics=FALSE)
theta <- PLpar(UV[,1], UV[,2]); zs <- seq(0.01,0.99, by=.01) # for later

# Take the sample estimated parameter and convert to joint probabilities Z
# Convert the Z to the Kendall Function estimates again with the sample parameter
Z  <- PLcop(UV[,1], UV[,2], para=theta); KF <- kfuncCOP(Z, cop=PLcop, para=theta)

# Compute L-moments of the "Kendall function" and the sample versions
# Note again though that the L-moment are for the distribution of the Z!
KNFlmr <- kfuncCOPlmoms(cop=PLcop, para=theta); SAMlmr <- lmomco::lmoms(Z)
knftxt <- paste0("Kendall L-moments: ",
                 paste(round(KNFlmr$lambdas, digits=4), collapse=", "))
samtxt <- paste0("Sample L-moments: " ,
                 paste(round(SAMlmr$lambdas, digits=4), collapse=", "))

plot(Z, KF, xlim=c(0,1), ylim=c(0,1), lwd=0.8, col=1,
     xlab="COPULA(u,v) VALUE [JOINT PROBABILITY]",
     ylab="KENDALL FUNCTION, AS NONEXCEEDANCE PROBABILITY")
rug(Z,  side=1, col=2, lwd=0.2); rug(KF, side=2, col=2, lwd=0.2) # rug plots
lines(zs, kfuncCOP(zs, cop=PLcop, para=1/pi), col=3)
knf_meanZ <- KNFlmr$lambdas[1]; sam_meanZ <- SAMlmr$lambdas[1]
knf_mean  <- kfuncCOP(knf_meanZ, cop=PLcop, para=theta) # theo. Kendall function
sam_mean  <- kfuncCOP(sam_meanZ, cop=PLcop, para=theta) # sam. est. of Kendall func
points(knf_meanZ, knf_mean, pch=16, col=4, cex=4) # big blue dot
points(sam_meanZ, sam_mean, pch=16, col=5, cex=2) # smaller pale blue dot
lines(zs, zs-zs*log(zs), lty=2, lwd=0.8) # dash ref line for independence
text(0.2, 0.30, knftxt, pos=4, cex=0.8); text(0.2, 0.25, samtxt, pos=4, cex=0.8)
text(0.2, 0.18, paste0("Note the uniform distribution of the ",
                       "vertical axis rug."), cex=0.7, pos=4) #}
}
\keyword{copula (characteristics)}
\keyword{Kendall Function (L-moments)}
\concept{Kendall Function Lmoments}
\concept{Kendall Function L-moments}
\keyword{L-moments}
\concept{Lmoments}
\concept{Kendall Function L-moment ratio diagram}
\concept{Kendall Function L-moment diagram}
\concept{Kendall Function Lmoment ratio diagram}
\concept{Kendall Function Lmoment diagram}
