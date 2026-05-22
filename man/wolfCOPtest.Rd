\encoding{utf8}
\name{wolfCOPtest}
\alias{wolfCOPtest}
\title{Empirically Created Schweizer and Wolff Sigma Test Against an Independence Copula}
\description{
Perform a \emph{Schweizer--Wolff Sigma} test for \eqn{\sigma_\mathbf{C}} stemming from \eqn{\mathrm{NULL} : \mathbf{C} = \mathbf{\Pi}} (\emph{Independence copula}, \code{\link{P}}), where  \eqn{\sigma_\mathbf{C}} is defined under \code{\link{wolfCOP}}. This \emph{Schweizer--Wolff Sigma hypothesis test} has been empirically tuned from small to large samples by extensive simulation; and, as implemented, the test is closely tied to the version of sample computation by \code{\link{wolfCOP}} using a particular form of the empirical copula (refer to that function for details). The \code{wolfCOPtest} function internally loads the \code{\link{wolfCOPtest_data_smlsam}} tables, and most users will not need to access the that dataset themselves unless for checking veracity of the simulations. There are no other known hypothesis tests for \eqn{\sigma_\mathbf{C}} in the literature.
}
\usage{
wolfCOPtest(x, y, asuv=FALSE, aslist=TRUE, na.rm=TRUE, digits=6,
                  probs=c(0.90, 0.95, 0.98, 0.99, 0.995), usepade=FALSE, ...)
}
\arguments{
  \item{x}{Nonexceedance probabilities \eqn{u} in the \eqn{X} direction if \code{asuv=TRUE}, else data in the \eqn{X} direction for which nonexceedance probabilities will be computed by plotting-position formula. If \code{x} is a single value, it is assumed to be a previously computed \eqn{\sigma_\mathbf{C}} and \code{y} will be considered as the sample size. Note, statistical estimation is based on samples of \eqn{n \ge 3}, if the sample size is less than this, then internally \eqn{n = 3} is set for the computations;}
  \item{y}{Nonexceedance probabilities \eqn{v} in the \eqn{Y} direction if \code{asuv=TRUE}, else data in the \eqn{Y} direction for which nonexceedance probabilities will be computed by plotting-position formula (\code{lmomco::pp()} function);}
  \item{asuv}{Logical identifying whether the \code{x} and \code{y} are already in nonexceedance probabilities or rather such need to be computed internally by plotting-position formula (\code{lmomco::pp()} function);}
  \item{aslist}{Logical to return a list of the test results rather than a vector;}
  \item{na.rm}{A logical to trigger a \code{complete.cases} operation on internal matrix of the data as probabilities;}
  \item{digits}{Digits for rounding before return;}
  \item{probs}{Quantiles by nonexceedance probability to extract from the fitted distribution---these are common upper-tail critical values. If \code{probs} is empty, then 0.95 nonexceedance probability is silently used;}
  \item{usepade}{A logical to switch, on estimation of the empirical L-moments of the logit transformed simulations, between (1) power-exponent optimized regression equations, or (2) optimized \emph{\enc{Padé}{Pade} Approximant} forms; and}
  \item{...}{Additional arguments to pass to the \code{lmomco::pp()} function.}
}
\details{
The algorithm is based on Monte Carlo simulations of \eqn{\mathbf{\Pi}} and computation of the L-moments of a \emph{logit transform} of the computed \eqn{\sigma_\mathbf{\Pi}} for which nonlinear regression of the L-moments to the sample size have been created. Development scripts for the simulations are located in the directory \code{inst/make_}\code{wolfCOPtest/} of \pkg{copBasic} sources.

The \emph{logit transformation} is defined as

\deqn{\mathrm{logit}\bigl(\sigma_\mathbf{C}\bigr) = \mathrm{log}\bigl(\sigma_\mathbf{C}/(1-\sigma_\mathbf{C})\bigr) = \eta\mbox{,}
}
and its inverse is defined as
\deqn{
\sigma_\mathbf{C} = \mathrm{logit}^{(-1)}(\eta) = 1/\bigl(1 + \mathrm{exp}(-\eta)\bigr)\mbox{.}
}

The predicted L-moments are used to drive from the \pkg{lmomco} package the Pearson type III (PE3) (\code{parpe3}, \code{cdfpe3}, \code{quape3}) of the \emph{logit transform} of \eqn{\sigma_\mathbf{\Pi}} to compute the p-value and supporting information. Evaluation of an \emph{L-moment ratio diagram} (refer to \pkg{lmomco} functions \code{plotlmrdia()} and \code{lmrdia()}) shows a heuristic change from GNO to PE3 at about \eqn{n = 40}, but inspection of upper-tail empirical distributions (and hence p-values in the practical range of use for \eqn{\mathrm{NULL}} rejection) suggest the PE3 tail is better. Therefore, the \code{wolfCOPtest} function exclusively uses the PE3.

Sample-sample empirical distributions (\eqn{n \in 3, \cdots, 40}) will have p-values reported on output and missing for \eqn{n \ge 41} (refer to \code{\link{wolfCOPtest_data_smlsam}} for more details).

To conclude, the figure below shows the \emph{L-moment ratio diagram} of the \emph{L-skew} and \emph{L-kurtosis} diagram for the logit transform of the \emph{L-moments} of the \eqn{\hat\sigma_\mathbf{C}} based on \code{\link{EMPIRgrid_fast}} in the aforementioned build directory of the \pkg{copBasic} package. The figure is based on nearly \eqn{900} million simulations summed across a range of representative sample sizes \eqn{n \in 8, \cdots 15001}, and provides support for the decision by the author to use the PE3 as the distribution model for the hypothesis test.

\if{html}{\figure{lmrdiaT34wolfP.jpg}{options: width=500}}
\if{latex}{\figure{lmrdiaT34wolfP.pdf}{options: width=5in, trim=0in 0in 0in 0in, clip}}

}
\value{
  Extensive list or named vector output the test is returned.
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
}
\keyword{copula (hypothesis test)}
\keyword{Schweizer and Wolff Sigma}
\keyword{Schweizer and Wolff Sigma (hypothesis test)}
\keyword{generalized normal distribution}
\keyword{Pearson type III distribution}
\keyword{GNO (see generalized normal distribution)}
\keyword{PE3 (see Pearson type III distribution)}
\keyword{logit transform}
