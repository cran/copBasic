\encoding{utf8}
\name{spectralmeas}
\alias{spectralmeas}
\title{Estimation of the Spectral Measure}
\description{
Kiriliouk \emph{et al.} (2016, pp. 360--364) describe estimation of the \emph{spectral measure} of bivariate data.  Standardize the bivariate data as \eqn{X^\star} and \eqn{Y^\star} as in \code{\link{psepolar}} and select a \dQuote{large} value for the \emph{pseudo-polar radius} \eqn{S_f} for nonexceedance probability \eqn{f}. Estimate the spectral measure \eqn{H(w)}, which is the limiting distribution of the \emph{pseudo-polar angle} component \eqn{W} given that the corresponding radial component \eqn{S} is large:
\deqn{\mathrm{Pr}[W \in \cdot | S > S_f] \rightarrow H(w) \mbox{\ as\ } S_f \rightarrow \infty\mbox{.}}
So \eqn{H(w)} is the cumulative distribution function of the spectral measure for angle \eqn{w \in (0,1)}. The \eqn{S_f} can be specified by a nonexceedance probability \eqn{F} for \eqn{S_f(F)}.

The estimation proceeds as follows:

\bold{Step 1:} Convert the bivariate data \eqn{(X_i, Y_i)} into \eqn{(\widehat{S}_i, \widehat{W}_i)} by \code{\link{psepolar}} and set the threshold \eqn{S_f} according to \dQuote{\eqn{n/k}} (this part involving \eqn{k} does not make sense in Kiriliouk \emph{et al.} (2016)) where for present implementation in \pkg{copBasic} the \eqn{S_f} given the \eqn{f} by the user is based on the empirical distribution of the \eqn{\widehat{S}_i}. The empirical distribution is estimated by the Bernstein empirical distribution function from the \pkg{lmomco} package.

\bold{Step 2:} Let \eqn{I_n} denote the set of indices that correspond to the observations when \eqn{\widehat{S}_i \ge S_f} and compute \eqn{N_n} as the cardinality of \eqn{N_n = |I_n|}, which simply means the length of the vector \eqn{I_n}.

\bold{Step 3:} Use the \emph{maximum Euclidean likelihood estimator}, which is the third of three methods mentioned by Kiriliouk \emph{et al.} (2016):
\deqn{\widehat{H}_3(w) = \sum_{i \in I_n} \hat{p}_{3,i} \times \mathbf{1}[\widehat{W}_i \le w ]\mbox{,}}
where \eqn{\mathbf{1}[\cdot]} is an \emph{indicator function} that is only triggered if \code{smooth=FALSE}, and following the notation of Kiriliouk \emph{et al.} (2016), the \dQuote{3} represents maximum Euclidean likelihood estimation. The \eqn{\hat{p}_{3,i}} are are the weights
\deqn{\hat{p}_{3,i} = \frac{1}{N_n}\bigl[ 1 - (\overline{W} - 1/2)S^{-2}_W(\widehat{W}_i - \overline{W}) \bigr]\mbox{,}}
where \eqn{\overline{W}} is the \emph{sample mean} and \eqn{S^2_W} is the \emph{sample variance} of \eqn{\widehat{W}_i}
\deqn{\overline{W} = \frac{1}{N_n} \sum_{i \in I_n} \widehat{W}_i\mbox{\quad and\quad } S^2_W = \frac{1}{N_n - 1} \sum_{i \in I_n} (\widehat{W}_i - \overline{W})^2\mbox{,}}
where Kiriliouk \emph{et al.} (2016, p. 363) do not show the \eqn{N_n - 1} in the denominator for the variance but \pkg{copBasic} uses it because those authors state that the \emph{sample variance} is used.

\bold{Step 4:} A smoothed version of \eqn{\hat{H}_3(w)} is optionally available by
\deqn{\tilde{H}_3(w) = \sum_{i \in I_n} \hat{p}_{3,i} \times \mathcal{B}(w; \widehat{W}_i\nu, (1-\widehat{W}_i)\nu)\mbox{,}}
where \eqn{\mathcal{B}(x; p, q)} is the cumulative distribution function of the Beta distribution for \eqn{p, q > 0} and where \eqn{\nu > 0} is a smoothing parameter that  can be optimized by cross-validation.

\bold{Step 5:} The spectral density lastly can be computed optionally as
\deqn{\tilde{h}_3(w) = \sum_{i \in I_n} \hat{p}_{3,i} \times \beta(w; \widehat{W}_i\nu, (1-\widehat{W}_i)\nu)}
where \eqn{\beta(x; p, q)} is the probability density function (pdf) of the Beta distribution. Readers are alerted to the absence of the \eqn{\mathbf{1}[\cdot]} indicator function in the definitions of \eqn{\tilde{H}_3(w)} and \eqn{\tilde{h}_3(w)}. This is correct and matches Kiriliouk \emph{et al.} (2016, eqs. 17.21 and 17.22) though this author was confused for a day or so by the indicator function in what is purported to be the core definition of \eqn{\hat{H}_l(w)} where \eqn{l = 3} in Kiriliouk \emph{et al.} (2016, eq. 17.21 and 17.17).
}
\usage{
spectralmeas(u, v=NULL, w=NULL, f=0.90, snv=FALSE,
                                smooth=FALSE, nu=100, pdf=FALSE, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction (actually the ranks are used so this can be a real-value argument as well);}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction  (actually the ranks are used so this can be a real-value argument as well) and if \code{NULL} then \code{u} is treated as a two column \R \code{data.frame};}
  \item{w}{A vector of polar angle values \eqn{W \in [0,1]} on which to compute the \eqn{H(w)};}
  \item{f}{The nonexceedance probability \eqn{F(S_f)} of the \emph{pseudo-polar radius} in \code{\link{psepolar}};}
  \item{snv}{Return the standard normal variate of the \eqn{H} by the well-known transform through the quantile function of the standard normal, \code{qnorm()};}
  \item{smooth}{A logical to return \eqn{\tilde{H}_3(w)} instead of \eqn{H_3(w)};}
  \item{nu}{The \eqn{\nu > 0} smoothing parameter;}
    \item{pdf}{A logical to return the smoothed probability density \eqn{\tilde{h}_3(w)}. If \code{pdf=TRUE}, then internally \code{smooth=TRUE} will be set; and}
  \item{...}{Additional arguments to pass to the \code{\link{psepolar}} function.}
}
\value{
  An \R \code{vector} of \eqn{H_3(w)}, \eqn{\tilde{H}_3(w)}, or \eqn{\tilde{h}_3(w)} is returned.
}
\note{
The purpose of this section is to describe an extremely CPU intensive study of goodness-of-fit between a \emph{Gumbel--Hougaard copula} (\code{\link{GHcop}}, \eqn{\mathbf{GH}(u,v; \Theta_1)}) parent and a fitted \emph{\enc{Hüsler}{Husler}--Reiss copula} (\code{\link{HRcop}}, \eqn{\mathbf{HR}(u,v; \Theta_2)}). Both of these copulas are extreme values and are somewhat similar to each other, so sample sizes necessary for detection of differences should be large. A two-sided Kolmogorov--Smirnov tests (KS test, \code{ks.test()}) will be used to measure significance in the differences between the estimated spectral measure distributions at \eqn{f=0.90} (the 90th percentile, \eqn{F(S_f)}) into the right tail.

The true copula will be the \eqn{\mathbf{GH}(\Theta_1)} having parameter \eqn{\Theta_1 = 3.3}. The number of simulations per sample size \code{n} \eqn{\in} \code{seq(50,1000, by=25)} is \code{nsim = 500}. For each sample size, a sample from the true parent is drawn, and a \eqn{\mathbf{HR}(\Theta_2)} fit by maximum likelihood (\code{\link{mleCOP}}). The two spectral measure distributions (\eqn{\widehat{H}_{\mathbf{GH}}(w)}, \code{Htru} and \eqn{\widehat{H}_{\mathbf{HR}}(w)}, \code{Hfit}) are estimated for a uniform variate of the angle \code{W} having length equal to the applicable sample size. The Kolmogorov--Smirnov test is made between \code{Htru} and \code{Hfit}, and number of p-values less than the \eqn{\beta = 0.05} (Type II error because alternative hypothesis is rigged as true) and simulation count are returned and written in file \code{Results.txt}. The sample sizes initially are small and traps of \code{NaN} (abandonment of a simulation run) are made. These traps are needed when the empirical distribution of \code{Htru} or \code{Hfit} degenerates.
\preformatted{
  Results <- NULL
  true.par <- 3.3; true.cop <- GHcop; fit.cop <- HRcop; search <- c(0,100)
  nsim <- 20000; first_time <- TRUE; f <- 0.90; beta <- 0.05
  ns <- c(seq(100,1000, by=50), 1250, 1500, 1750, 2000)
  for(n in ns) {
    W <- sort(runif(n)); PV <- vector(mode="numeric")
    for(i in 1:(nsim/(n/2))) {
      UV      <- simCOP(n=n, cop=true.cop, para=true.par, graphics=FALSE)
      fit.par <- mleCOP(UV,  cop= fit.cop, interval=search)$para
      UVfit   <- simCOP(n=n, cop= fit.cop, para=fit.par,  graphics=FALSE)
      Htru    <- spectralmeas(UV,    w=W, bound.type="Carv", f=f)
      Hfit    <- spectralmeas(UVfit, w=W, bound.type="Carv", f=f)
      if(length(Htru[! is.nan(Htru)]) != length(Hfit[! is.nan(Hfit)]) |
         length(Htru[! is.nan(Htru)]) == 0 |
         length(Hfit[! is.nan(Hfit)]) == 0) {
         PV[i] <- NA; next
      }   # suppressWarnings() to silence ties warnings from ks.test()
      KS <- suppressWarnings(stats::ks.test(Htru, Hfit)$p.value)
      #plot(FF, H, type="l"); lines(FF, Hfit, col=2); mtext(KS)
      message("-",i, appendLF=FALSE)
      PV[i] <- KS
    }
    message(":",n)
    zz <- data.frame(SampleSize=n, NumPVle0p05=sum(PV[! is.na(PV)] <= beta),
                     SimulationCount=length(PV[! is.na(PV)]))
    if(first_time) {
       Results <- zz; first_time <- FALSE; next
    }
    Results <- rbind(Results, zz)
  }
}

The \code{Results} show a general increase in the counts of p-value \eqn{\le} 0.05 as sample size increases. There is variation of course and increasing \code{nsim} would smooth that considerably. The results show for \eqn{n \approx 1{,}000} that the detection of statistically significant differences for extremal \eqn{F(S_f) = 0.90} dependency between the \eqn{\mathbf{GH}(\Theta_1{=}3.3)} and \eqn{\mathbf{HR}(\Theta_2)} are detected at the error rate implied by the specified \eqn{\beta = 0.05}.

This range in sample size can be compared to the Kullback--Leibler sample size (\eqn{n_{fg}}):
\preformatted{
  UV      <- simCOP(n=10000, cop=true.cop, para=true.par, graphics=FALSE)
  fit.par <- mleCOP(UV,      cop= fit.cop, interval=search)$para
  kullCOP(cop1=true.cop, para1=true.par, cop2=fit.cop, para2=fit.par)
  # The Kullback-Leibler (integer) sample size for detection of differences at
  # alpha=0.05 are n_fg = (742, 809, 815, 826, 915, 973, 1203) for seven runs
  # Do more to see variation.
}
where the Kullback--Leilber approach is to measure density departures across the whole \eqn{\mathcal{I}^2} domain as opposed to extremal dependency in the right tail as the spectral measure.

Different runs of the above code will result in different \eqn{n_{fg}} in part because of simulation differences internal to \code{\link{kullCOP}} but also because the \eqn{\Theta_2} has its own slight variation in its fit to the large sample simulation (\eqn{n=10{,}000}) of the parent. However, it seems that \eqn{n_{fg} \approx 900} will be on the order of the \eqn{n} for which the KS test on the spectral measure determines statistical significance with similar error rate.

Now if the aforementioned simulation run is repeated for \eqn{F(S_f) = 0.95} or \code{f=0.95}, the \eqn{n_{fg}} obviously remains unchanged at about \eqn{900} but the \eqn{n} for which the error rate is about \eqn{\beta = 0.05} is \eqn{n \approx 600}. This sample size is clearly smaller than before and smaller than \eqn{n_{fg}}, therefore, the analysis of the empirical spectral measure deeper into the tail \eqn{F(S_f) = 0.95} requires a smaller sample size to distinguish between the two copula. Though the analysis does not address the question as to whether one or both copula are adequate for the problem at hand. For a final comparison, if the aforementioned simulation run is repeated for \eqn{F(S_f) = 0.80} or \code{f=0.80}, then the \eqn{n} for which the error rate is about \eqn{\beta = 0.05} is \eqn{n \approx 1{,}700}. Thus as analysis is made further away from the tail into the center of the distribution, the sample size to distinguish between these two similar copula increases substantially.
}
\references{
Kiriliouk, Anna, Segers, Johan, \enc{Warchoł}{Warchol}, \enc{Michał}{Michal}, 2016, Nonparameteric estimation of extremal dependence: \emph{in} Extreme Value Modeling and Risk Analysis, D.K. Dey and Jun Yan \emph{eds.}, Boca Raton, FL, CRC Press, ISBN 978--1--4987--0129--7.
}
\author{William Asquith \email{william.asquith@ttu.edu} }
\seealso{ \code{\link{psepolar}}, \code{\link{stabtaildepf}}
}
\examples{
\dontrun{
UV <- simCOP(n=500, cop=HRcop, para=1.3, graphics=FALSE)
W <- seq(0,1,by=0.005)
Hu <- spectralmeas(UV, w=W)
Hs <- spectralmeas(UV, w=W, smooth=TRUE, nu=100)
plot(W,Hu, type="l", ylab="Spectral Measure H", xlab="Angle")
lines(W, Hs, col=2) # }

\dontrun{
"GAUScop" <- function(u,v, para=NULL, ...) {
  if(length(u)==1) u<-rep(u,length(v)); if(length(v)==1) v<-rep(v,length(u))
  return(copula::pCopula(matrix(c(u,v), ncol=2), para))
}
GAUSparfn <- function(rho) return(copula::normalCopula(rho, dim = 2))
n <- 2000 # The PSP parent has no upper tail dependency
uv    <- simCOP(n=n, cop=PSP,      para=NULL, graphics=FALSE)
PLpar <- mleCOP(uv,  cop=PLcop,    interval=c(0,100))$para
PLuv  <- simCOP(n=n, cop=PLcop,    para=PLpar, graphics=FALSE)
GApar <- mleCOP(uv,  cop=GAUScop,  parafn=GAUSparfn, interval=c(-1,1))$para
GAuv  <- simCOP(n=n, cop=GAUScop,  para=GApar, graphics=FALSE)
GLpar <- mleCOP(uv,  cop=GLcop,    interval=c(0,100))$para
GLuv  <- simCOP(n=n, cop=GLcop,    para=GLpar, graphics=FALSE)
FF <- c(0.001,seq(0.005,0.995, by=0.005),0.999); qFF <- qnorm(FF)
f <- 0.90 # Seeking beyond the 90th percentile pseudo-polar radius
PSPh <- spectralmeas(  uv, w=FF, f=f, smooth=TRUE, snv=TRUE)
PLh  <- spectralmeas(PLuv, w=FF, f=f, smooth=TRUE, snv=TRUE)
GAh  <- spectralmeas(GAuv, w=FF, f=f, smooth=TRUE, snv=TRUE)
GLh  <- spectralmeas(GLuv, w=FF, f=f, smooth=TRUE, snv=TRUE)
plot(qFF, PSPh, type="l", lwd=2, xlim=c(-3,3), ylim=c(-2,2),
     xlab="STANDARD NORMAL VARIATE OF PSEUDO-POLAR ANGLE",
     ylab="STANDARD NORMAL VARIATE OF SPECTRAL MEASURE PROBABILITY")
lines(qFF, PLh, col=2) #  red  line is the Plackett copula
lines(qFF, GAh, col=3) # green line is the Gaussian copula
lines(qFF, GLh, col=4) #  blue line is the Galambos copula
# Notice the flat spot and less steep nature of the PSP (black line), which is
# indicative of no to even spreading tail dependency. The Plackett and Gaussian
# copulas show no specific steepening near the middle, which remains indicative
# of no tail dependency with the Plackett being less steep because it has a more
# dispersed copula density at the right tail is approached than the Gaussian.
# The Galambos copula has upper tail dependency, which is seen by
# the mass concentration and steepening of the curve on the plot.}
}
\keyword{copula (goodness-of-fit)}
\keyword{copula (inference)}
\keyword{inference}
\keyword{spectral measure}
\keyword{inference}
\keyword{goodness-of-fit}
\concept{polar}
\keyword{API to the copula package}
\keyword{package copula (comparison to)}
