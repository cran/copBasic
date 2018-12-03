\encoding{utf8}
\name{vuongCOP}
\alias{vuongCOP}
\title{ Vuong's Procedure for Parametric Copula Comparison }
\description{
Perform \emph{Vuong's Procedure} following Joe (2014, pp. 257--258). Consider two copula densities \eqn{f_1 = c_1(u,v; \Theta_1)} and \eqn{f_2 = c_2(u,v; \Theta_2)} for two different bivariate copulas \eqn{\mathbf{C}_1(\Theta_1)} and \eqn{\mathbf{C}_2(\Theta_2)} having respective parameters \eqn{\Theta_1} and \eqn{\Theta_2} that provide the \dQuote{closest} \emph{Kullback--Leibler divergence} from the true copula density \eqn{g(u,v)}.

The difference of the Kullback--Leibler divergence (\code{\link{kullCOP}}) of the two copulas from the true copula density can be measured for a sample of size \eqn{n} and bivariate sample realizations \eqn{\{u_i, v_i\}} by
\deqn{\hat{D}_{12} = n^{-1}\sum_{i=1}^n D_i\mbox{,}}
where \eqn{\hat{D}_{12}} is referred to in the \pkg{copBasic} package as \dQuote{Vuong's \eqn{D}} and \eqn{D_i} is defined as
\deqn{D_i = \log\biggl[\frac{f_1(u_i, v_i; \Theta_2)}{f_2(u_i, v_i; \Theta_1)}\biggr]\mbox{.}}
The variance of \eqn{\hat{D}_{12}} can be estimated by
\deqn{\hat\sigma^2_{12} = (n-1)^{-1}\sum_{i=1}^n (D_i - \hat{D}_{12})^2\mbox{.}}
The sample estimate and variance are readily turned into the \eqn{100{{\times}}(1 - \alpha)} confidence interval by
\deqn{\hat{D}^{(\mathrm{lo})}_{12} < \hat{D}_{12} < \hat{D}^{(\mathrm{hi})}_{12}\mbox{,}}
where, using the quantile (inverse) function of the t-distribution \eqn{\sim} \eqn{\mathcal{T}^{(-1)}(F; \mathrm{df}{=}(n-2))} for nonexceedance probability \eqn{F} and \eqn{n-2} degrees of freedom for \eqn{n} being the sample size, the confidence interval is
\deqn{\hat{D}_{12}-\mathcal{T}^{(-1)}(1-\alpha/2){\times}\hat\sigma_{12}/\sqrt{n} < \hat{D}_{12} < \hat{D}_{12}+\mathcal{T}^{(-1)}(1-\alpha/2){\times}\hat\sigma_{12}/\sqrt{n}\mbox{.}}
Joe (2014, p. 258) reports other interval forms based (1) on the Akaike (AIC) correction and (2) on the Schwarz (BIC) correction, which are defined for AIC as
\deqn{\mathrm{AIC} = \hat{D}_{12} - (2n)^{-1}\log(n)\biggl[\mathrm{dim}(\Theta_2) - \mathrm{dim}(\Theta_1)\biggr]\pm \mathcal{T}^{(-1)}(1-\alpha/2){\times}\hat\sigma_{12}/\sqrt{n}\mbox{,}}
and for BIC as
\deqn{\mathrm{BIC} = \hat{D}_{12} - (2n)^{-1}\log(n)\biggl[\mathrm{dim}(\Theta_2) - \mathrm{dim}(\Theta_1)\biggr]\pm \mathcal{T}^{(-1)}(1-\alpha/2){\times}\hat\sigma_{12}/\sqrt{n}\mbox{.}}
The AIC and BIC corrections incorporate the number of parameters in the copula and for all else being equal the copula with the fewer parameters is preferable. If the two copulas being compared have equal number of parameters than the AIC and BIC equate to \eqn{\hat{D}_{12}} and the same confidence interval because the difference \eqn{[\mathrm{dim}(\Theta_2) - \mathrm{dim}(\Theta_1)]} is zero.

Joe (2014, p. 258) reports that these three intervals can be used for \emph{diagnostic inference} as follows. If an interval contains 0 (zero), then copulas \eqn{\mathbf{C}_1(\Theta_1)} and \eqn{\mathbf{C}_2(\Theta_2)} are not considered significantly different. If the interval does not contain 0, then copula \eqn{\mathbf{C}_1(\Theta_1)} or \eqn{\mathbf{C}_2(\Theta_2)} is the better fit depending on whether the interval is completely below 0 (thus \eqn{\mathbf{C}_1(\Theta_1)} better fit) or above 0 (thus \eqn{\mathbf{C}_2(\Theta_2)} better fit), respectively. Joe (2014) goes on the emphasize that \dQuote{the procedure compares different [copulas] and assesses whether they provide similar fits to the data. [The procedure] does not assess whether [either copula] is a good enough fit.}
}
\usage{
vuongCOP(u, v=NULL, cop1=NULL, cop2=NULL, para1=NULL, para2=NULL,
                    alpha=0.05, method=c("D12", "AIC", "BIC"),
                    the.zero=.Machine$double.eps^0.25, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction and if \code{NULL} then \code{u} is treated as a two column \R \code{data.frame};}
  \item{cop1}{A copula function corresponding to copula \eqn{f_1} in Vuong's Procedure;}
  \item{para1}{Vector of parameters or other data structure, if needed, to pass to the copula \eqn{f_1};}
  \item{cop2}{A copula function corresponding to copula \eqn{f_2} in Vuong's Procedure;}
  \item{para2}{Vector of parameters or other data structure, if needed, to pass to the copula \eqn{f_2};}
  \item{alpha}{The \eqn{\alpha} in Vuong's Procedure, which results in the \eqn{100{\times}(1 - \alpha)} confidence interval (two sided);}
  \item{method}{The interval to evaluate as to position of the respective statistic form the comparison of the two copulas;}
  \item{the.zero}{The value for \dQuote{the zero} of the copula density function. This argument is the argument of the same name for \code{\link{densityCOP}}. The default here is intended to suggest that a tiny nonzero value for density will trap the numerical zero densities; and}
  \item{...}{Additional arguments to pass to the \code{\link{densityCOP}} function.}
}
\value{
  An \R \code{list} is returned having the following components:
  \item{title}{A descriptive title of the procedure;}
  \item{method}{A textual description of the \code{method} setting;}
  \item{result.text}{A textual description of the result of Vuong's Procedure;}
  \item{result}{A value 1 if \eqn{\mathbf{C}_1(\Theta_1)} is better fit, 2 if copula \eqn{\mathbf{C}_2(\Theta_2)} is better fit, and \code{0} if neither is better (\eqn{\hat{D}_{12} = 0}), and \code{NA} including the likely(?) erroneous situation of \eqn{\mathbf{C}_1(\Theta_1) \equiv \mathbf{C}_2(\Theta_2)};}
  \item{p.value}{The two-sided p-values of the Vuong's Procedure inclusive of \eqn{\mathrm{AIC}} and \eqn{\mathrm{BIC}};}
  \item{D12}{A named vector of the lower and upper bounds of Vuong's \eqn{D} at the respective confidence interval percentage along with \eqn{\hat{D}_{12}} and \eqn{\sigma^2_{12}};}
  \item{AIC}{A named vector of the lower and upper bounds of Vuong's \eqn{\mathrm{AIC}} at the respective confidence interval percentage;}
  \item{BIC}{A named vector of the lower and upper bounds of Vuong's \eqn{\mathrm{BIC}} at the respective confidence interval percentage; and}
  \item{parameters}{A named vector of the alpha, sample size, value for the t-distribution quantile \code{qt(1-alpha/2, df=n)}, and \eqn{\hat\sigma_{12}}.}
}
\note{
The \code{vuongCOP} function along with \code{\link{kullCOP}} and features of function \code{\link{densityCOPplot}} represent collective milestones towards \emph{copula inference} and diagnostics post fitting of copulas to the usual measures of association such as \emph{Kendall's Tau} (\eqn{\tau_K}) and \emph{Spearman's Rho} (\eqn{\rho_S}) and their copula counterparts \eqn{\tau_\mathbf{C}} (\code{\link{tauCOP}}) and \eqn{\rho_\mathbf{C}} (\code{\link{rhoCOP}}).

For an example application, imagine a problem of say low springflow risk at \dQuote{nearby springs} that jointly should converge in the lower tail because drought usually has a strong regional impact. First, it is necessary to form a reflection of the \emph{Gumbel--Hougaard copula} (\eqn{\mathbf{GH}(u,v; \Theta_{\mathbf{GH}})}; \code{\link{GHcop}}) but parameter estimation using \eqn{\tau_\mathbf{C}} is the same because sample \eqn{\hat\tau_K} is invariant to reflection.
\preformatted{
  "rGHcop" <- function(u,v, ...) { u + v - 1 + GHcop(1-u, 1-v, ...) }
  set.seed(385) # setting so reported quantities here are reproducible
}
The prior code also sets a seed on the pseudo-random number generator so that reported values here are reproducible. The reflected \eqn{\mathbf{GH}(u,v; \Theta_{\mathbf{GH}})} is denoted \eqn{\mathbf{rGH}(u,v; \Theta_{\mathbf{rGH}})}.

Second, the \eqn{\mathbf{PSP}(u,v)} copula (\code{\link{PSP}}) is chosen as the parent distribution, and this copula has no parameter. The \eqn{\mathbf{PSP}} has lower-tail dependency, which will be important as discussion unfolds. The following two lines of code establish a sample size to be drawn from the \eqn{\mathbf{PSP}} and then simulates a sample from that copula. The color grey is used for the simulated values on the figure produced by \code{\link{simCOP}}, which forms a background example of the joint structure of the \eqn{\mathbf{PSP}} copula.
\preformatted{
  n <- 390
  UV <- simCOP(cop=PSP, n=n, col=8, pch=16) # simulate and form the base image
}
By inspection of the so-produced graphic, it is obvious that there is contraction in the lower-left corner of the plot, which is a geometric representation of tail dependency. The lower-tail dependency thus phenomenalogically says that there is joint interconnect during low springflow conditions---both springs are likely to be at low flow simultaneously. The variable \code{UV} contains the bivariate data as uniform variables (nonexceedance probabilities \eqn{u} and \eqn{v}).

The \emph{Plackett copula} (\eqn{\mathbf{PL}(u,v; \Theta_{\mathbf{PL}})}; \code{\link{PLACKETTcop}}) and the \eqn{\mathbf{rGH}(u,v; \Theta_{\mathbf{rGH}})} copula are chosen as candidate models of the \dQuote{unknown} parent. Both \eqn{\mathbf{PL}} and \eqn{\mathbf{rGH}} copulas use different \dQuote{measures of association} for their parameter estimation. So next, sample estimates of the copula parameters using \emph{Schweizer and Wolff's Sigma} \eqn{\hat\sigma_\mathbf{C}}. The sample value computations and parameter estimates also are set as shown in the following code:
\preformatted{
  Wolf   <- wolfCOP(para=UV, as.sample=TRUE) # 0.496943
  paraPL <- uniroot(function(p)
                Wolf - wolfCOP(cop=PLACKETTcop, para=p), c(1,30))$root
  paraGH <- uniroot(function(p)
                Wolf - wolfCOP(cop=rGHcop,      para=p), c(1,30))$root
}

\emph{STEP 1---Compute Kullback--Leibler sample size:} The Kullback--Leibler divergences (\eqn{\mathrm{KL}(f|g)} and \eqn{\mathrm{KL}(g|f)}) are computed (\code{\link{kullCOP}}) for the evaluation of the sample size as appropriate for distinguishing between the two candidate copulas 95 percent of the time. The Kullback--Leibler sample size (\eqn{n_{f\!g}}) also is computed as the following code illustrates and provides additional commentary.
\preformatted{
  KL <- replicate(20, kullCOP(cop1=PLACKETTcop, para1=paraPL,
            cop2=rGHcop, para2=paraGH, n=1E5)$KL.sample.size) # CPU intensive
  print(round(mean(KL))) #         n_{fg} = 221   sample size
  print(     range(KL))  # 204 <-- n_{fg} --> 252 sample size range
}

Depending on the sample \eqn{\hat\sigma_\mathbf{C}} coming from the simulation of the parent \eqn{\mathbf{PSP}} copula, the call to \code{\link{kullCOP}} will likely report different \eqn{n_{f\!g}} values because \eqn{n_{f\!g}(\mathbf{C}_1(\Theta_1), \mathbf{C}_1(\Theta_1)}. These sample sizes have a range for 20 replications of about \eqn{n_{f\!g}=204{-}252}. The result here is \eqn{n_{f\!g}=221} and thus \bold{the sample size \eqn{n=390} should be more than large enough to generally distinguish between the \eqn{\mathbf{PL}} and \eqn{\mathbf{rGH}} copulas at the respective sample measure of association.}

\emph{STEP 2---Perform Vuong's Procedure:} The Vuong's Procedure can now be completed. Now watch the copula and parameter order in the next code for mistakes, the author has purposefully switched order here to draw attention to the need to make sure argument \code{cop1} has the correct parameter(s) for copula 1 (the \eqn{\mathbf{PL}}). The two calls to \code{\link{simCOP}} are made to graphically superimpose these simulations on top of the parent \eqn{\mathbf{PSP}}.
\preformatted{
  VD <-vuongCOP(UV, cop2=rGHcop, para2=paraGH, cop1=PLACKETTcop, para1=paraPL)
  print(VD) # "Copula 2 better" or rGHcop (Gumbel-Hougaard is better)
  set.seed(385) # seems harmless enough to reuse the seed to emphasize "fit"
  TMP <-simCOP(cop=PLACKETTcop,para=paraPL,n=n,plot=FALSE,col=2,pch=16,cex=0.5)
  set.seed(385) # seems harmless enough to reuse the seed to emphasize "fit"
  TMP <-simCOP(cop=rGHcop,     para=paraGH,n=n,plot=FALSE,col=3,pch=16,cex=0.5)
  rm(TMP) # just cleaning up the workspace.
}

Further discussion of Vuong's Procedure is informative. Simply speaking, the result is that \bold{the \eqn{\mathbf{rGH}} (copula 2) has better fit than \eqn{\mathbf{PL}} (copula 1).} The 95-percent confident limits from Vuong's Procedure having \eqn{\hat{D}_{12} = 0.049} with p-value \eqn{0.0012}, \eqn{\hat\sigma_{12} = 0.297}, and \eqn{n=390} are \eqn{0.0194 < \hat{D}_{12} < 0.0786}. This interval does not contain zero and is greater than zero and therefore a conclusion may be drawn that copula 2 has the better fit.

\emph{STEP 3---Comparison of lower-tail dependency parameters:} What does the tail dependency do for inference? This can be checked by computing the lower-tail dependency parameters (\eqn{\lambda^L_\mathbf{C}}; \code{\link{taildepCOP}}) in the code that follows for each of the three copulas \eqn{+} the empirical copula but note that true sample estimators do not quite exist. Numeric focus need only be on the lower tail, but the four graphics are informative.
\preformatted{
  taildepCOP(cop=PSP,                      plot=TRUE)$lambdaL # = 1/2
  taildepCOP(cop=PLACKETTcop, para=paraPL, plot=TRUE)$lambdaL # = ZERO
  taildepCOP(cop=rGHcop,      para=paraGH, plot=TRUE)$lambdaL # = 0.429
  taildepCOP(cop=EMPIRcop,    para=UV,     plot=TRUE)$lambdaL # = 0.328
}

The important aspect of the graphics by \code{\link{taildepCOP}} is that the \eqn{\mathbf{rGH}} has lower-tail dependency and the \eqn{\mathbf{PL}} does not, so based on inspection \eqn{\mathbf{rGH}} is superior given that we known \eqn{\mathbf{PSP}} was the true parent.  The empirical estimate of the \eqn{\hat\lambda^L_\mathbf{C} = 0.328} through the \code{\link{EMPIRcop}} copula indicates that its lower-tail dependency is closer to that of the \eqn{\mathbf{rGH}} relative to \eqn{\mathbf{PL}} and thus \bold{quantitatively by lower-tail dependency the \eqn{\mathbf{rGH}} has a superior fit.}

So the \eqn{\mathbf{rGH}} has a tail dependency more similar to the true model compared to the \eqn{\mathbf{PL}}. Hence for this example, the \eqn{\mathbf{rGH}} is clearly a superior fitting model in terms of \emph{Vuong's Procedure} (fit alone) and the \eqn{\lambda^L_\mathbf{C}} then is used as a follow up to shown that the \eqn{\mathbf{rGH}} might be \dQuote{good enough} an approximation to the \eqn{\mathbf{PSP}}. The efficacy of reflecting the \eqn{\mathbf{GH}} copula into a \dQuote{new} form as \eqn{\mathbf{rGH}} is demonstrated. Users are strongly encouraged to review the so-produced graphic from the \code{\link{simCOP}} call several listings back for \eqn{n=390}, and lastly, this example is one for which absence of the argument \code{snv} (standard normal variate [scores]) by \code{\link{simCOP}} makes the tail dependency issue for the sample size more prominent in the graphic.

\emph{STEP 4---Qualitatively compare using copula density plots:} Graphical depiction of copula density contours by the \code{\link{densityCOPplot}} function supports the conclusion that the \eqn{\mathbf{rGH}} is the superior model relative to the \eqn{\mathbf{PL}}. The so-produced graphic obviously shows that \bold{the \eqn{\mathbf{rGH}} strongly mimics the shape of the parent \eqn{\mathbf{PSP}}.}
\preformatted{
  densityCOPplot(cop=PSP, contour.col=8) # grey is the parent bivariate density
  densityCOPplot(cop=PLACKETTcop,para=paraPL,contour.col=3,ploton=FALSE)# green
  densityCOPplot(cop=rGHcop,     para=paraGH,contour.col=2,ploton=FALSE)# red
}

\emph{STEP 5---Compute L-comoments of the data via simulation and estimate the sampling distributions:} An open research problem is the what if any role that \emph{L-comoments} might play in either copula estimation or inference. (There being virtually no literature on the topic?) Because a measure of association was used for parameter estimation, the L-correlation is uniformative, but a comparison is conceptually useful. The \eqn{\hat\sigma_\mathbf{C} = 0.4969} and \emph{Spearman's Rho} of the data \eqn{\hat\rho_S = } and the L-correlations \eqn{\tau^{[12]}_{2} \approx \tau^{[21]}_{2} \approx 0.497} are all similar.

Inference using L-coskew and L-cokurtosis seems possible. The following code listing is CPU intensive. First, the L-correlation, L-coskew, and L-cokurtosis values are computed from the simulated sample by the \code{lcomoms2()} function of the \pkg{lmomco} package. Second and third, the respective sampling distributions of these L-comoments (\code{\link{lcomCOPpv}}) for the two copulas are estimated.
\preformatted{
  UVlmr <- lmomco::lcomoms2(UV,nmom=4) # The sample L-comoments
  # This execution will result in nonrejection of rGH copula.
  GHlmr <- lcomCOPpv(n, UVlmr, cop=rGHcop,      para=paraGH) # NONREJECTION
  # LcomType      n     Mean  Lscale    Lskew   Lkurt sample.est p.value signif
  #    Tau3[12] 390 -0.06952 0.01819  0.04505 0.12024   -0.11188 0.08795      .
  #    Tau3[21] 390 -0.06739 0.02084  0.04104 0.12917   -0.10673 0.14162      -
  # Tau3[12:21] 390 -0.06845 0.01713  0.04930 0.11696   -0.10931 0.08161      .
  #    Tau4[12] 390  0.04970 0.01682 -0.01635 0.10150    0.04183 0.38996      -
  #    Tau4[21] 390  0.05129 0.01606 -0.06833 0.13798    0.07804 0.17470      -
  # Tau4[12:21] 390  0.05049 0.01329 -0.02045 0.12001    0.05994 0.35069      -

  # This execution will result in rejection of Plackett copula.
  PLlmr <- lcomCOPpv(n, UVlmr, cop=PLACKETTcop, para=paraPL) # REJECT PLACKETT
  #  LcomType     n     Mean  Lscale    Lskew   Lkurt sample.est p.value signif
  #    Tau3[12] 390 -0.00267 0.02133  0.01556 0.09581   -0.11188 0.00129     **
  #    Tau3[21] 390 -0.00112 0.02022 -0.00663 0.13338   -0.10673 0.00189     **
  # Tau3[12:21] 390 -0.00189 0.01757  0.00906 0.10226   -0.10931 0.00019    ***
  #    Tau4[12] 390  0.00153 0.01652 -0.03320 0.12468    0.04183 0.07924      .
  #    Tau4[21] 390  0.00361 0.01851 -0.01869 0.12052    0.07804 0.00929     **
  # Tau4[12:21] 390  0.00257 0.01362 -0.01194 0.10796    0.05994 0.00744     **
}
Because each copula was fit to a measure of association, the p-values for the L-correlations are all nonsignificant (noninformative because of how the copulas were fit), and therefore p-values quite near to the 50th percentile should be produced. So here, the L-correlation is noninformative on fit even though it might have some role because it is asymmetrical unlike that statistics \eqn{\tau_K} and \eqn{\rho_S}. The results in variable \code{GHlmr} show no statistically significant entries (all p-values \eqn{{>}0.05 = (\alpha=0.1)/2)}) for L-coskew and L-cokurtosis---\bold{the \eqn{\mathbf{rGH}} copula is not rejected.} The results in \code{PLlmr} show many p-values \eqn{{<}0.05 = (\alpha=0.1)/2} for both L-coskew and L-cokurtosis---\bold{the \eqn{\mathbf{PL}} copula is rejected}. The experimental L-comoment inference shown is consistent with results with Vuong's Procedure.

Vuong's Procedure, however, does not address adequacy of fit---it just evaluates which copula fits better. The inspection of the lower tail dependency results previously shown (\eqn{\lambda^L_\mathbf{PSP} = 1/2 \approx \lambda^U_\mathbf{rGH}} = 0.429) along with the L-coskew and L-cokurtosis of the sample being well within the sample distribution suggests that the \eqn{\mathbf{rGH}} is a adequate mimic of the \eqn{\mathbf{PSP}} copula.

Some open research questions concern the numerical performance of the L-comoments as simulation sample size becomes large and whether or not the L-comoments should be computed on the probabilities \eqn{\{u, v\}}. Also should conversion to normal scores be made and if so, should adjustment by the \emph{Hazen plotting positions} (\eqn{u_i = (r_i - 0.5)/n} for rank \eqn{r_i}) be made that Joe (2014) repeatedly advocates when standard normal variates (scores) [\eqn{z_i = \Phi^{(-1)}(u_i)} for quantile function of standard normal distribution \eqn{\Phi(0,1)}]? Collectively, Nelsen (2006) and Salvadori \emph{et al.} (2007) are both silent on the matter of normal score conversion, and in conclusion Nelsen (2006), Salvadori \emph{et al.} (2007), and Joe (2014) also are all silent on L-comoment applications with copulas.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{densityCOP}}, \code{\link{kullCOP}}, \code{\link{simCOP}}, \code{\link{statTn}}, \code{\link{mleCOP}}}
\examples{
# See extended code listings and discussion in the Note section
# See Examples in mleCOP() (Last example therein might suggest a problem in the
# implied 95th percentile associated with n_fg above.
}
\keyword{inference}
\keyword{copula (goodness-of-fit)}
\keyword{copula (inference)}
\keyword{copula (density)}
\keyword{inference}
\keyword{goodness-of-fit}
\concept{Vuong procedure}
\concept{Akaike}
\concept{AIC}
\concept{BIC}
\concept{Vuong}
\concept{Kullback--Leibler}
\concept{Kullback-Leibler}
\concept{L-comoment copula inference}
\concept{Lcomoment copula inference}
