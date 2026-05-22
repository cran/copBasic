\encoding{utf8}
\name{wolfCOPsamc}
\alias{wolfCOPsamc}
\title{Monte Carlo Integration of Schweizer--Wolff Sigma}
\description{
Perform \emph{Monte Carlo integration} for the measure of dependence known as \emph{Schweizer--Wolff Sigma} \eqn{\sigma_\mathbf{C} \in (0,1)} of an \emph{empirical copula} (\eqn{\mathbf{C}_n(u,v)}). This function can be useful as the \code{as.sample} computation provided by \code{\link{wolfCOP}} scales very poorly as sample size increases. Please refer to \code{\link{wolfCOP}} for more details, including theory, about \eqn{\sigma_\mathbf{C}}. Sample estimation can be made by

\deqn{\hat\sigma_\mathbf{C} = 12\int\!\!\int_{\mathcal{I}^2} \bigl|\mathbf{C}_n(u,v) - uv\bigr|\,\mathrm{d}u\mathrm{d}v\mbox{,}}

for which the integrals in the above formula are replaced with Monte Carlo integration provided by this function. Allowance is made internally to swap out \eqn{\mathbf{C}_n(u,v)} for a copula family \eqn{\mathbf{C}(u,v)}, which means this function can be used for integration for families that might have numerical integration convergence problems in \code{\link{wolfCOP}}.
}
\usage{
wolfCOPsamc(u=NULL, v=NULL, cop=EMPIRcop, para=NULL, para_has_paras=FALSE,
            tol=0.005, minit=10, maxit=100, subdivisions=200, large_n=5E3,
            includelmoms=FALSE, trace=FALSE, verbose=FALSE, ...)
}
\arguments{
  \item{u}{Optional nonexceedance probability \eqn{u} in the \eqn{X} direction. This argument can also by a two-column data frame from which the \code{para} argument to \code{\link{EMPIRcop}} when \code{cop=EMPIRcop}) is internally constructed;}
  \item{v}{Optional nonexceedance probability \eqn{v} in the \eqn{Y} direction from which the \code{para} argument to \code{\link{EMPIRcop}} when \code{cop=EMPIRcop} is internally constructed;}
  \item{cop}{The empirical copula, \code{\link{EMPIRcop}}, or a copula family;}
  \item{para}{Optional parameter content for the \code{para} argument to the specified copula in \code{cop}. For an empirical copula application (default), the \code{para} must be a two-column data frame, but if \code{cop} is set to a copula family, then this function can be used as an alternative \eqn{\sigma_\mathbf{C}} computation using Monte Carlo integration in lieu of numerical integration exclusive to \code{\link{wolfCOP}} (refer to \bold{Examples} how to use the function in this way);}
  \item{para_has_paras}{A logical to inform processing whether the \code{para} argument contains parameters for a copula family in lieu of \code{para} containing the sample for the empirical copula (\code{cop=EMPIRcop}). Internally, \code{para_has_paras} is used to disable random subsetting by setting the sample size variable to \code{large_n}. Technically, it is possible internally to detection whether \code{para_has_paras} needed to be true (even if false) as the number of random variates for the \code{halton()} function of the \pkg{randtoolbox} package is computed, and the code will silently proceed as if \code{para_has_paras=TRUE};}
  \item{tol}{A tolerance in absolute percent change in the global mean as the iterations progress. This tolerance should be good enough for many practical large sample applications, but can be reduced considerable if the function is used as the backdoor to integration of a given copula family instead of a sample estimator;}
  \item{minit}{Minimum number of iterations regardless of \code{tol} setting;}
  \item{maxit}{Maximum number of iterations;}
  \item{subdivisions}{Number of subdivisions and somewhat an analog to the term used in numerical integration. This subdivisions will be the size of the Monte Carlo sample for each iteration;}
  \item{large_n}{For an empirical copula and as the sample size gets large, not only does the \eqn{\sigma_\mathbf{C}} computation slow down because a double summation is involved, but the counting that empirical copula must also perform gets large, therefore, the \code{para} that will be ultimately passed to the empirical copula is randomly subsetted by \code{large_n}. This is done in order to speed up iteration times at expense of increasing time that \code{tol} presumably is reached;}
  \item{includelmoms}{A logical to include upto the first five linear moments for \eqn{\sigma_\mathbf{C}} and logit-transformed \eqn{\mathrm{log}[\sigma_\mathbf{C}/(1-\sigma_\mathbf{C})]} in the returned list.}
  \item{trace}{A logical, if true, that will produce a scatter plot of the sample (or the current iteration subset [refer to \code{large_n}]) along with the random sample points used for the current Monte Carlo iteration;}
  \item{verbose}{A logical providing percent change results by iteration to the console; and}
  \item{...}{Additional arguments to pass to the \code{halton()} function of the \pkg{randtoolbox} and the copula specified by \code{cop} (\code{\link{EMPIRcop}}) or a copula family.}
}
\value{
  A list of the Monte Carlo integration results is returned containing
\item{wolves}{A vector of the \eqn{\sigma_\mathbf{C}} by iteration;}
\item{estimates}{The minimum, mean (the formal estimate \eqn{\hat\sigma_\mathbf{C}}), maximum along with optional variance (computed from second L-moment), L-scale (second L-moment), L-skew, and L-kurtosis of the \code{wolves};}
\item{estimates}{Using logit-transformation of the \code{wolves}, the minimum, mean, maximum along with optional variance (computed from second L-moment), L-scale (second L-moment), L-skew, and L-kurtosis of the \code{wolves};}
\item{its}{Number of iterations before termination by \code{maxits} or upon reaching \code{tol};}
\item{lastpctchg}{The percent change on the last iteration.}
}
\note{
Beyond about 1,000 sample sizes \code{wolfCOP(as.sample=TRUE)} scales in time poorly, so we see in the following recipe the giant increase in speed at a small cost in precision on the Schweizer--Wolff Sigma estimate.
\preformatted{
  UV       <- simCOP(3000, cop=CLcop, para=3, graphics=FALSE, seed=473)
  theowolf <- wolfCOP(     cop=CLcop, para=3) # 0.7864391
  system.time( sfwolf <- wolfCOP(    para=UV, as.sample=TRUE,
                                          nlarge=1, usefastgrid=TRUE ) )
  system.time( smwolf <- wolfCOP(    para=UV, as.sample=TRUE,
                                          nlarge=1, usefastgrid=FALSE) )
  system.time( mcwolf <- wolfCOPsamc(para=UV,   usetime=TRUE) )
  #   user  system elapsed   WOLF(sample fast) = 0.8002883
  #  0.600   0.017   0.616
  #   user  system elapsed   WOLF(sample slow) = 0.8002883
  # 188.072  25.594 213.632
  #   user  system elapsed   WOLF(monte carlo) = 0.799808
  #  12.729   1.449  14.191
}


If one wanted to make some comparisons of computed results between \code{\link{wolfCOP}} and \code{wolfCOPsamc}, the following recipe, with a suitably large sample size, that we see a general spread around the equal value line. The use of Monte Carlo integration approach is not advised when computational speed of \code{\link{wolfCOP}} is sufficient.  This \code{wolfCOPsamc} function is potentially of less usefulness than suggested because of the \code{\link{EMPIRgrid_fast}} within \code{\link{wolfCOP}} for rapid estimation by a gridded empirical copula.
\preformatted{
  nsam <- 1000; nsim <- 20; wolf <- mcwolf <- rep(NA, length=nsim)
  for(i in seq_len(nsim)) {
    uv <- as.data.frame(matrix(runif(nsam*2), ncol=2))
      wolf[i] <- wolfCOP(    para=uv, as.sample=TRUE)
    mcwolf[i] <- wolfCOPsamc(para=uv)$estimates[2]
    print(c(i, wolf[i], mcwolf[i]))
  }
  df <- data.frame(sigma=wolf, sigma_mc=mcwolf)
  df <- df[complete.cases(df),]
  xylim <- range( c(range(df$sigma, na.rm=TRUE), range(df$sigma_mc)) )
  plot(df$sigma, df$sigma_mc, xlim=xylim, ylim=xylim,
       xlab="Schweizer-Wolff sigma by whole-sample counting",
       ylab="Schweizer-Wolff sigma by Monte Carlo integration")
  lines(par()$usr[1:2], par()$usr[3:4]) # equal value line }
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Schweizer, B., and Wolff, E.F., 1981, On nonparametric measures of dependence for random variables: The Annuals of Statistics, v. 9, no. 4, pp. 879--855, \url{https://www.jstor.org/stable/2240856}.
}
\author{W.H. Asquith}
\seealso{\code{\link{wolfCOP}}, \code{\link{EMPIRcop}}
}
\examples{
n <- 40; para=c(1.3, 2.4)              # Small sample, for speed.
UV <- simCOP(40, para=para, cop=GHcop, seed=1) # Simulate a sample
wolfCOP(    para=para, cop=GHcop)      # Theo. value + next 2 lines
wolfCOPsamc(para=para, cop=GHcop)$estimate[2] # Same as next line
wolfCOPsamc(para=para, cop=GHcop, para_has_paras=TRUE)$estimate[2]
wolfCOP(    para=UV, as.sample=TRUE, usefastgrid=TRUE, nlarge=1) # Spring 2026
wolfCOP(    para=UV, as.sample=TRUE, usefastgrid=FALSE) # original pkg implementation
wolfCOPsamc(para=UV)$estimate[2]       # Monte Carlo estimate

\dontrun{
  # Test the trace and verbose arguments
  para <- list(cop=c(P, M), part=c(0,0.5,1))
  UV   <- simCOP(1000, cop=ORDSUWcop, para=para)
  SW   <- wolfCOPsamc(para=UV, tol=0.0005, trace=TRUE, verbose=TRUE) #}

\dontrun{
  # Test the various interfaces for a sample computation along with
  # setting the usetime to the random number subsystem.
  UV <- simCOP(1000, cop=CLcop, para=3, graphics=FALSE)
  mc1 <- wolfCOPsamc(para=UV, usetime=TRUE)$estimates[2]
  mc2 <- wolfCOPsamc(u=UV,    usetime=TRUE)$estimates[2]
  mc3 <- wolfCOPsamc(u=UV[,1], v=UV[,2], usetime=TRUE)$estimates[2] #}

\dontrun{
  # Let us check on the veracity of wolfCOPsamc() by a cluster-based computation
  # of realization of mean values of Lmoments of logit-transformed Sigmas for a
  # given sample size against the projected Lmoments (Lskew and Lkurtosis) in
  # regressions implemented within wolfCOPtest(). We will need the ellipse and
  # parallel packages already installed to the user's machine for this test.
  nsam <- 40 # Sample size (not related to simulation settings in the example.)
  lmomco::plotlmrdia(lmomco::lmrdia(), empty=TRUE, # Lmoment ratio diagram
            xlim=c(0.1, 0.35), ylim=c(0.1, 0.20), las=1, xaxs="i", yaxs="i")
  lmomco::plotlmrdia(lmomco::lmrdia(usrtrim=TRUE), add=TRUE, expand.names=TRUE,
             autolegend=TRUE, xleg="topleft", ncol=1, nolimits=TRUE,
             nogov=TRUE, nogpa=TRUE, noexp=TRUE,  nonor=TRUE, nogum =TRUE,
             noray=TRUE, nouni=TRUE, noaep4=TRUE, noglo=TRUE, nopdq3=TRUE)
  xy <- NULL # coordinates of the Lskew (T3) and Lkurtosis(T4) of logit-Sigma
  for(n in c(3:99, 10^seq(2, 6, by=0.1))) { # a vector of sample sizes
    wt <- wolfCOPtest(0, n)$lmoms_logit_sigma # Lmoments of logit-Sigma
    xy <- rbind(xy, data.frame(tau3=wt[3], tau4=wt[4])) # isolate the T3 and T4
  }
  lines( xy[,1], xy[,2], col="wheat3", lwd=4) # plot the estimation of T3 and T4
  wt <- wolfCOPtest(0, nsam)$lmoms_logit_sigma # isolate the prediction for the
  points(wt[3], wt[4], pch=16) # sample size that we are studying
  cols <- c("salmon4", "salmon1", "seagreen4", "seagreen1")
  # All the code below can be run with on setting of doMC false and then true.
  doMC <- FALSE # set to true for wolfCOPsamc() else wolfCOP(as.sample=TRUE)
  LMR  <- NULL; ncore <- 8; nloop <- 40; nsim <- 500 # last of initializations
  # The wolfCOPsamc() has long running time but by about 500-1000 samples it
  # likely will then begin outpacing the wolfCOP(). We have the nsam default as
  # small for this example and nsim enough for definitive stability in the results
  # and nloop large enough for an authoritative ellipse to be drawn.
  if(nloop < 4) stop("Need at least four for Lmoment computation later")
  for(j in seq_len(nloop)) {
    message(j, "-", appendLF=FALSE)
    if(length(grep("[02468]0$", as.character(j))) > 0) message("", appendLF=TRUE)
    cl <- parallel::makeCluster(getOption("cl.cores", ncore)) # make CPU cluster
    parallel::clusterExport( cl, "nsam") # make "nsam" visible in cluster FUN
    if(doMC) { # set tolerance "up" just for some speed increase for the example
      wolves <- parallel::parSapply(cl=cl, seq_len(nsim), FUN=function(i) {
           wolfCOPsamc(para=as.data.frame(matrix(runif(nsam*2),
                           ncol=2)), tol=0.01, usetime=TRUE)$estimates[2] })
    } else {
      wolves <- parallel::parSapply(cl=cl, seq_len(nsim), FUN=function(i) {
           wolfCOP(    para=as.data.frame(matrix(runif(nsam*2),
                           ncol=2)), as.sample=TRUE)                      })
    }
    parallel::stopCluster(cl) # stop the cluster as it will be restarted as needed
    wolves <- log( wolves / (1 - wolves) ) # make logit-Sigmas
    lmr    <- Lmoments::Lmoments(wolves)   # faster than lmomco::lmoms()
    lmr[3] <- lmr[3] / lmr[2]; lmr[4] <- lmr[4] / lmr[2] # Lskew and Lkurtosis
    LMR <- rbind(LMR, data.frame(L1=lmr[1], L2=lmr[2], T3=lmr[3], T4=lmr[4]))
    if((par()$usr[1] <= lmr[3] & lmr[3] <= par()$usr[2]) &
       (par()$usr[3] <= lmr[4] & lmr[4] <= par()$usr[4])) {
      if(doMC) {
        points(lmr[3], lmr[4], col=cols[1], bg=cols[2], pch=24, lwd=0.9, cex=0.8)
      } else {
        points(lmr[3], lmr[4], col=cols[3], bg=cols[4], pch=25, lwd=0.9, cex=0.8)
      }
    }
  }
  # Lskew and Lkurtosis are approximately multivariate normally distributed.
  Trho <- stats::cor( LMR$T3,  LMR$T4 ) # Pearson correlation coefficient
  Tscl <- c(Lmoments::Lmoments(LMR$T3)[2] * sqrt(pi), # standard deviations in U
            Lmoments::Lmoments(LMR$T4)[2] * sqrt(pi)) # standard deviations in V
  Tctr <- c(mean(LMR$T3), mean(LMR$T4))    # ellipse center
  Telp <- ellipse::ellipse(Trho, scale=Tscl, centre=Tctr, npoints=200, level=0.95)
  if(doMC) {
    lines(Telp, col=cols[1], lty=4, lwd=2) # ellipse itself
    points(mean(LMR$T3), mean(LMR$T4), pch=21, cex=2, col=cols[3], bg=cols[2])
  } else {
    lines(Telp, col=cols[3], lty=4, lwd=2) # ellipse itself
    points(mean(LMR$T3), mean(LMR$T4), pch=21, cex=2, col=cols[3], bg=cols[4])
  }
  points(wt[3], wt[4], pch=16) # sample size that we are studying # }
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{Monte Carlo integration}
\keyword{Schweizer and Wolff Sigma}
\keyword{Package randtoolbox}
\keyword{Package ellipse}
\keyword{Package parallel}
\keyword{Package Lmoments}
