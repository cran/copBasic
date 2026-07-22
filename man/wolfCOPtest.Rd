\encoding{utf8}
\name{wolfCOPtest}
\alias{wolfCOPtest}
\title{Empirically Created Schweizer and Wolff Sigma Test Against an Independence Copula}
\description{
Perform a \emph{Schweizer--Wolff Sigma} test for \eqn{\sigma_\mathbf{C}} stemming from \eqn{\mathrm{NULL} : \mathbf{C} = \mathbf{\Pi}} (\emph{Independence copula}, \code{\link{P}}), where  \eqn{\sigma_\mathbf{C}} is defined under \code{\link{wolfCOP}}. There are no other known hypothesis tests for \eqn{\sigma_\mathbf{C}} in the literature.

This \emph{Schweizer--Wolff Sigma hypothesis test} has been empirically tuned from small to large samples by extensive simulation, and, as implemented, the test is closely tied to the version of sample computation by \code{\link{wolfCOP}} using a particular form of the empirical copula (refer to that function for details). The \code{wolfCOPtest} function internally loads the \code{\link{wolfCOPtest_data_smlsam}} tables, and most users generally will not need to access that dataset themselves unless perhaps for checking veracity of the simulations.
}
\usage{
wolfCOPtest(x, y, asuv=FALSE, aslist=TRUE, na.rm=TRUE, digits=6,
                  probs=c(0.90, 0.95, 0.98, 0.99, 0.995),
                  zmat=NULL, statf=mean, rndphi=20, usepade=FALSE,
            ties.method=c("average", "first", "last", "random", "max", "min"), ...)
}
\arguments{
  \item{x}{Nonexceedance probabilities \eqn{u} in the \eqn{X} direction if \code{asuv=TRUE}, else data in the \eqn{X} direction for which nonexceedance probabilities will be computed by plotting-position formula. If \code{x} is a single value, it is assumed to be a previously computed \eqn{\sigma_\mathbf{C}} and \code{y} will be considered as the sample size. Note, statistical estimation is based on samples of \eqn{n \ge 3}, if the sample size is less than this, then internally \eqn{n = 3} is set for the computations. The \code{x} can also be a two column data structure and the \code{y} will be taken from the second column;}
  \item{y}{Nonexceedance probabilities \eqn{v} in the \eqn{Y} direction if \code{asuv=TRUE}, else data in the \eqn{Y} direction for which nonexceedance probabilities will be computed by plotting-position formula (refer to \code{lmomco::pp()} function, but simply the\cr\code{rank(...)} function is used);}
  \item{asuv}{Logical identifying whether the \code{x} and \code{y} are already in nonexceedance probabilities or rather such need to be computed internally by plotting-position formula (refer to \code{lmomco::pp()} function, but simply the \code{rank(...)} function is used);}
  \item{aslist}{Logical to return a list of the test results rather than a vector;}
  \item{na.rm}{A logical to trigger a \code{complete.cases} operation on internal matrix of the data as probabilities;}
  \item{digits}{Digits for rounding before return;}
  \item{probs}{Quantiles by nonexceedance probability to extract from the fitted distribution---these are common upper-tail critical values. If \code{probs} is empty, then 0.95 nonexceedance probability is silently used;}
  \item{zmat}{An optional matrix of four columns and rows equally those of the data values with columns 1 and 2 containing, respectively, the lower and upper limits of the \eqn{u} and columns 3 and 4 containing, respectively, the lower and upper limits of \eqn{v}. This \code{zmat} feature provides for support if data censoring is involved and simulation between the limits made (refer to \bold{Examples}). Note, if \code{zmat} is given, then \code{ties.method} is set \code{"random"} and \code{asuv} is set false;}
  \item{statf}{Statistical function for reducing simulated \eqn{\sigma} values (mean or median suggested), but a user could \code{plot(density(}\code{result$rand_sigma))} and reach their own decisions on centrality measure;}
  \item{rndphi}{A multiplier on the number of ties or censored values to drive simulation of samples as part of populating the \code{rand_sigma} element, when applicable, on the function return;}
  \item{usepade}{A logical to switch, on estimation of the empirical L-moments of the logit transformed simulations, between (1) power-exponent optimized regression equations, or (2) optimized \emph{\enc{Padé}{Pade} Approximant} forms;}
  \item{ties.method}{Argument of the same name for \code{rank}, but if there are ties, \code{asuv} is set false, and \code{ties.method} is set to random, then simulation is made internally on the ranking and \code{statf} used to compute the \eqn{\sigma} estimate;}
  \item{...}{Additional arguments to pass to the \code{lmomco::pp()} function.}
}
\details{
The implementation here is based on \emph{Monte Carlo simulations} of \eqn{\mathbf{\Pi}} and computation of the L-moments of a \emph{logit transform} of the computed \eqn{\sigma_\mathbf{\Pi}} for which nonlinear regression of the L-moments to the sample size have been created. Development scripts for the simulations are located in the directory \code{inst/make_}\code{wolfCOPtest/} of \pkg{copBasic} sources.

The \emph{logit transformation} is defined as

\deqn{\mathrm{logit}\bigl(\sigma_\mathbf{C}\bigr) = \mathrm{log}\frac{\sigma_\mathbf{C}}{1-\sigma_\mathbf{C}} = \eta\mbox{,}
}
and its inverse is defined as
\deqn{
\sigma_\mathbf{C} = \mathrm{logit}^{(-1)}(\eta) = \frac{1}{1 + \mathrm{exp}(-\eta)}\mbox{.}
}

Sample-sample empirical distributions (\eqn{n \in 3, \cdots, 40}) will have p-values reported on output and missing for \eqn{n \ge 41} (refer to \code{\link{wolfCOPtest_data_smlsam}} for more details).

The predicted L-moments are used to drive from the \pkg{lmomco} package the Pearson type III (PE3) (\code{parpe3}, \code{cdfpe3}, \code{quape3}) of the \emph{logit transform} of \eqn{\sigma_\mathbf{\Pi}} to compute the p-value and supporting information.

Evaluation of an \emph{L-moment ratio diagram} (refer to \pkg{lmomco} functions \code{plotlmrdia()} and \code{lmrdia()}) shows a change from a generalized normal distribution (GNO) to PE3 at about \eqn{n = 40}, but inspection of upper-tail empirical distributions (and hence p-values in the practical range for \eqn{\mathrm{NULL}} rejection) suggest the PE3 tail is better choice for overall application. The \code{wolfCOPtest} function, therefore, exclusively uses the PE3.


To conclude, the figure below shows the \emph{L-moment ratio diagram} of the \emph{L-skew} and \emph{L-kurtosis} diagram for the logit transform of the \emph{L-moments} of the \eqn{\hat\sigma_\mathbf{C}} based on \code{\link{EMPIRgrid_fast}} in the aforementioned build directory of the \pkg{copBasic} package. The figure is based on nearly \eqn{900} million simulations summed across a representative range of sample sizes \eqn{n \in 8, \cdots 15001}, and provides support for the decision by the author to use the PE3 as the distribution model for the hypothesis test.

\if{html}{\figure{lmrdiaT34wolfP.jpg}{options: width=500}}
\if{latex}{\figure{lmrdiaT34wolfP.pdf}{options: width=5in, trim=0in 0in 0in 0in, clip}}

}
\value{
Extensive list or named vector output the test is returned. For the list, the \code{estimate} contains duplicate information as \code{statistic[1]}; this is so that there is an \code{estimate} entry to parallel the return of \code{cor.test(...)} to help users write nomenclature similar calls between the functions.
}
\author{W.H. Asquith}
\seealso{\code{\link{wolfCOP}}, \code{\link{wolfCOPsamc}}, \code{\link{wolfCOPtest_data_smlsam}}
}
\examples{
uv <- simCOP(n=62, cop=PSP, graphics=FALSE, seed=8)
# Approx p-value 0.001 PSP is not independence.
# There is no p.value(sample_le40) entry on the return because too large sample size.
wolfCOPtest(uv[,1], uv[,2], asuv=TRUE, aslist=FALSE, usepade=FALSE)
wolfCOPtest(uv[,1], uv[,2], asuv=TRUE, aslist=FALSE, usepade=TRUE )
# See how the p-values between the empirical approximations are about the same.

uv <- simCOP(n=62, cop=P,   graphics=FALSE, seed=8)
wolfCOPtest(uv[,1], uv[,2], asuv=TRUE, aslist=FALSE) # approx p-value 0.200***
# Note, the small sample p-values are NA for two previous examples because the sample
# size has become "large", but in the next example, we can inspect two p-values
# (1) by the Pearson type III distribution to the predicted L-moments for the sample
# size and (2) by the small-sample empirical tables in object wolfCOPtest_data_smlsam.
uv <- simCOP(n=13, cop=P,   graphics=FALSE, seed=81)
wolfCOPtest(uv[,1], uv[,2], asuv=TRUE, aslist=FALSE)
#   p.value(dist_pe3) p.value(sample_le40)  # four signif. figures kept here as
#          0.2550***            0.25144**   # simulations might tune up internals

\dontrun{
  # This example needs larger nsim but will show that the Sigma is performing close to
  # Spearman Rho, which helps establish the veracity of the algorithms. If the user were
  # to switch out the copula P and replace with CIRCcop (circular copula), then one will
  # see Sigma performing similarly but Rho failing because the CIRCcop is an example of
  # perfect dependence in a setting of independence. Use of much larger nsim for this
  # example will hone the apparent rejection to about 0.05 for P, but clearly we see
  # divergence in performance for CIRCcop.
  alpha <- 0.05; n <- 42; nsim <- 20000; cop <- P; p <- NULL
  set.seed(1)
  wolf   <- replicate(nsim, { wolfCOP(para=rCOP(n=n, cop, para=p),
                              as.sample=TRUE, nlarge=0, usefastgrid=TRUE) })
  wolfpv <- sapply(wolf, function(w) {
                   wolfCOPtest(w, n, asuv=TRUE, aslist=FALSE)[4]})
  message("  Schweizer-Wolff Sigma(n=", n, ") NULL=P rejection rate = ",
           sum(wolfpv < alpha) / nsim)
  set.seed(1)
  rhopv <- replicate(nsim, { uv <- rCOP(n=n, cop=cop, para=p);
                    cor.test(uv[,1], uv[,2], method="spearman")$p.value })
  message("           Spearman Rho(n=", n, ") NULL=P rejection rate = ",
           sum(rhopv  < alpha) / nsim)
  # For nsim <- 20000
  #       P : Schweizer-Wolff Sigma(n=42) NULL=P rejection rate = 0.04935
  #       P :          Spearman Rho(n=42) NULL=P rejection rate = 0.04805
  # CIRCcop : Schweizer-Wolff Sigma(n=42) NULL=P rejection rate = 0.28125
  # CIRCcop :          Spearman Rho(n=42) NULL=P rejection rate = 0.00100
  # Such example computations show that Rho does not "sense" the perfect dependence
  # of CIRCcop because its "correlation" is zero but has obvious dependence when
  # plotted. Therefore, Sigma has a much higher rejection rate that will go to unity
  # as the sample size increase.}

\dontrun{
  lmomco::plotlmrdia(lmomco::lmrdia(usrtrim=TRUE), nopoints=TRUE, xleg="topleft",
                    xlim=c(0.11,0.32), ylim=c(0.12,0.20), autolegend=TRUE,
                    expand.names=TRUE, autoaxes=TRUE, xaxs="i", yaxs="i", lwd.cex=1.3)
  xy <- st <- NULL
  ns <- sort(unique(c(10^seq(0,1,0.01), 10^seq(0, 4, by=0.02))))
  for(n in ns) {
    lmr <- wolfCOPtest(0, n, usepade=FALSE)$lmoms_logit_sigma; if(is.null(lmr)) next
    xy <- rbind(xy, data.frame(n=n, mu=lmr[1], lam2=lmr[2], tau3=lmr[3], tau4=lmr[4]))
  }
  xy <- xy[par()$usr[1] <= xy$tau3 & xy$tau3 <= par()$usr[2],]
  xy <- xy[par()$usr[3] <= xy$tau4 & xy$tau4 <= par()$usr[4],]
  lines(xy$tau3, xy$tau4, col="grey60", lwd=7)
  for(n in ns) {
    lmr <- wolfCOPtest(0, n, usepade=TRUE)$lmoms_logit_sigma; if(is.null(lmr)) next
    st <- rbind(st, data.frame(n=n, mu=lmr[1], lam2=lmr[2], tau3=lmr[3], tau4=lmr[4]))
  }
  st <- st[par()$usr[1] <= st$tau3 & st$tau3 <= par()$usr[2],]
  st <- st[par()$usr[3] <= st$tau4 & st$tau4 <= par()$usr[4],]
  lines(st$tau3, st$tau4, col="red1", lwd=2)
  txt <- c("Logit transform sampling Sigma distribution, wolfCOPtest(usepade=FALSE)",
           "Logit transform sampling Sigma distribution, wolfCOPtest(usepade=TRUE )")
  legend("bottomright", txt, lwd=c(7,2), col=c("grey50", "red1"), bty="n", cex=0.9) #}

\dontrun{
  # Here is a cross check in properties of the mathematics computing W.
  # The examples here also show the pass of 2-columns in x; so, a local copy
  # of a simulations is not needed before passing to wolfCOPtest().
  alpha <- 0.05; n <- 42; nsim <- 2000; cop <- P; p <- NULL
  set.seed(1)
  wolf1   <- replicate(nsim, { wolfCOP(para=rCOP(n=n, cop, para=p),
                              as.sample=TRUE, nlarge=0, usefastgrid=TRUE) })
  wolf1pv <- sapply(wolf1, function(w) { # here, sigma precomputed
                   wolfCOPtest(w, n, asuv=TRUE, aslist=FALSE)[4]})
  set.seed(1)
  wolf2pv <- replicate(nsim, { # here the "true" probabilities are known
                   wolfCOPtest(rCOP(n=n, cop, para=p), asuv=TRUE )$p.value[1]})
  set.seed(1)
  wolf3pv <- replicate(nsim, { # here the probabilities computed internally
                   wolfCOPtest(rCOP(n=n, cop, para=p), asuv=FALSE)$p.value[1]})
  message("  Schweizer-Wolff Sigma(n=", n, ") NULL: P rejection rate = ",
           sum(wolf1pv < alpha) / nsim)
  message("  Schweizer-Wolff Sigma(n=", n, ") NULL: P rejection rate = ",
           sum(wolf2pv < alpha) / nsim)
  message("  Schweizer-Wolff Sigma(n=", n, ") NULL: P rejection rate = ",
           sum(wolf3pv < alpha) / nsim) # }

\dontrun{
  # Here is a cross check of sorts between wolfCOPtest and the Independence test
  # available in the copula package. We use a Clayton copula with small bivariate
  # association and will see for this circumstance that wolfCOPtest out performs
  # copula::indepTest by just a small amount by rejecting the NULL more often.
  sample_size <- 100; ALPHA <- 0.05
  THECOP  <- CLcop; THEPARA <- 0.20; rhoCOP(cop=THECOP, para=0.20) # 0.1358466

  nsim <- 2000; digits <- as.integer( floor( log10(nsim) ) )
  J <- copula::indepTestSim(sample_size, 2, N=100000)

  W <- I <- NULL
  for(i in seq_len(nsim)) {
    if(is.null(W)) {
      uv <- copBasic::simCOP( sample_size, cop=THECOP, para=THEPARA )
      mtext("Copula simulation reference figure", line=0.5, font=2 )
    } else {
      uv <- copBasic::rCOP(   sample_size, cop=THECOP, para=THEPARA)
    }
    I <- c(I,   copula::indepTest(  uv,         J             )$pvalues[1])
    W <- c(W, copBasic::wolfCOPtest(uv[,1], uv[,2], asuV=TRUE )$p.value[1])
  }

  par(lend=2, las=1); hyphen <- "\u00ad"
  plot( sort( rank(I) ) / ( length(I) + 1 ), sort(I), type="l", xaxs="i", yaxs="i",
       xlab="Nonexceedance probability", lwd=3, col="grey50",
       ylab=paste0("P", hyphen, "value of respective test of independence"))
  lines(sort( rank(W) ) / ( length(W) + 1 ), sort(W), col="darkorange1")
  txt <- c(paste0("Independence test (copula::indepTest) has rejection rate = ",
                  round( sum(I < ALPHA) / nsim, digits=digits), ""),
           paste0("Schweizer-Wolff test (copBasic::wolfCOPtest) has rejection rate = ",
                  round( sum(W < ALPHA) / nsim, digits=digits), ""))
  legend("topleft", txt, bty="o", box.lty=0, bg=NA, inset=0.02, cex=0.9,
         lwd=c(3,1), col=c("grey50", "darkorange1"))
  mtext(paste0("Empirical distribution of p", hyphen, "values"), line=0.5, font=2) #}

\dontrun{
  # DATA CENSORING EXAMPLE ------------------------------------------------------------
  set.seed(10) # Example using the zmat feature for support of censoring by simulation.
  muslp <- 0.005; sdslp <- 0.019 # drift with exp used to keep parameter positive
  yy <- 1980:2021; n <- length(yy) # Simulated log-Pearson III annual peak streamflows
  qq <- 10^sapply(yy, function(y) { # put a small drift in the log10 mean and variation
              para <- lmomco::vec2par(c(3.5+(y-min(yy))*muslp,
                               exp(log(0.5)-(y-min(yy))*sdslp), 0.3), type="pe3")
                      lmomco::rlmomco(1, para) }) # flow
  zul <- zur <- rep(NA, n) # initialize the lower and upper limits for years
  zvl <- zvr <- rep(NA, n) # initialize the lower and upper limits for flows
  # NA years are not simulated and the data are used as is
  # Make some highly contrived censoring for the 3, 6, 9, 13, and 35+th observations.
  zul[ 3] <- yy[ 3] - 1.1; zur[ 3] <- yy[ 3] + 1.1
  zul[13] <- yy[13] - 1.5; zur[13] <- yy[13] + 2.4
  zvl[ 3] <- 10; zvr[3] <- qq[3]
  zvl[ 6] <- qq[6]; zvr[6] <- max(qq) + sqrt(.Machine$double.eps) # nudge above max
  zvl[ 9] <- 10^(log10(qq[9]) - 0.3); zvr[9] <- 10^(log10(qq[9]) + 0.2) # log-cycles
   qq[ 9] <- sqrt( zvl[9] * zvr[9] ) # geometric mean
  zvl[(n-6):n] <- 10; zvr[(n-6):n] <- qq[(n-6):n]

  xy <- cbind(yy, qq); zmat <- cbind(zul, zur, zvl, zvr)
  swolf <- wolfCOPtest(xy[,1], xy[,2], zmat=zmat, asuv=FALSE, statf=median, rndphi=500)
  ylim <- log10( range( qq ) ); ylim <- 10^c( floor( ylim[1] ), ceiling( ylim[2] ) )
  plot(  yy, qq, type="n", log="y", xlab="Water year", ylab="Streamflow", ylim=ylim)
  segments(x=zul, x1=zur, y=qq,  y1=qq,  col="darkgoldenrod3")
  segments(x=yy,  x1=yy,  y=zvl, y1=zvr, col="darkgoldenrod3")
  points(yy, qq,  bg="darkgoldenrod1",   col="darkgoldenrod4",  pch=21)
  naive <- wolfCOPtest(xy[,1], xy[,2], asuv=FALSE, ties.method="random")
  txt <- paste0("Sigma simulation (nsim=",
                prettyNum(length(swolf$rand_sigma), big.mark=","),   ") ",
                "p.value=",       round(swolf$p.value[1], digits=3), "; ",
                "Naive p.value=", round(naive$p.value[1], digits=3))
  mtext(txt, line=1) # Visually the point values tell us to expect independence to be
  # rejected, but when the data censoring is taken into account and simulation used,
  # we do not reject independence that is heavily influenced by the last seven data
  # values on the right being censored down through the bottom axis.
  swolf$rand_sigma <- sort(swolf$rand_sigma)
  p <- sapply(swolf$rand_sigma, function(k) { # distribution of p-values
                        n <- ifelse(swolf$sample_size <= 40, 2, 1)
                     wolfCOPtest(k, swolf$sample_size)$p.value[n] })
  swolf$rand_p.values <- p
  # plot(swolf$rand_sigma, swolf$rand_p.values, pch=16, col=grey(0, 0.05)) # }
}
\keyword{copula (hypothesis test)}
\keyword{Schweizer and Wolff Sigma}
\keyword{Schweizer and Wolff Sigma (hypothesis test)}
\keyword{generalized normal distribution}
\keyword{Pearson type III distribution}
\keyword{GNO (see generalized normal distribution)}
\keyword{PE3 (see Pearson type III distribution)}
\keyword{logit transform}
\concept{Hypothesis test for Schweizer and Wolff Sigma}
\keyword{censored data}
