\encoding{utf8}
\name{vuongCOP}
\alias{vuongCOP}
\title{ Vuong's Procedure for Parametric Copula Comparison }
\description{
Perform the \emph{Vuong's Procedure} following Joe (2015, pp. 257--258). Consider two copula densities \eqn{f_1 = c_1(u,v; \Theta_1)} and \eqn{f_2 = c_2(u,v; \Theta_2)} for two different bivariate copulas \eqn{\mathbf{C}_1(\Theta_1)} and \eqn{\mathbf{C}_2(\Theta_2)} having respective parameters \eqn{\Theta} that provide the \dQuote{closest} \emph{Kullback-Leibler divergence} from the true copula density \eqn{g(u,v)}. The difference of the Kullback-Leibler  divergence (\code{\link{kullCOP}}) of the two copulas from the true copula density can be measured for a sample of size \eqn{n} and bivariate sample realizations \eqn{\{u_i, v_i\}} by
\deqn{\hat{D}_{12} = n^{-1}\sum_{i=1}^n D_i\mbox{,}}
where \eqn{\hat{D}_{12}} is referred to in the \pkg{copBasic} package as \dQuote{Vuong's \eqn{D}} and \eqn{D_i} is defined as
\deqn{D_i = \log\biggl[\frac{f_1(u_i, v_i; \Theta_2)}{f_2(u_i, v_i; \Theta_1)}\biggr]\mbox{.}}
The variance of \eqn{\hat{D}_{12}} can be estimated by
\deqn{\hat\sigma^2_{12} = (n-1)^{-1}\sum_{i=1}^n (D_i - \hat{D}_{12})^2\mbox{.}}
The sample estimate and variance are readily turned into the \eqn{100{{\times}}(1 - \alpha)} confidence interval by
\deqn{\hat{D}^{(\mathrm{lo})}_{12} < \hat{D}_{12} < \hat{D}^{(\mathrm{hi})}_{12}\mbox{,}}
where, using the quantile function of the Standard Normal distribution [\eqn{\Phi^{(-1)}(t)}] for nonexceedance probability \eqn{t}, the confidence interval is
\deqn{\hat{D}_{12}-\Phi^{(-1)}(1-\alpha){\times}\hat\sigma_{12}/\sqrt{n} < \hat{D}_{12} < \hat{D}_{12}+\Phi^{(-1)}(1-\alpha){\times}\hat\sigma_{12}/\sqrt{n}\mbox{.}}
Joe (2015, p. 258) reports other interval forms based (1) on the Akaike (AIC) correction and (2) on the Schwarz (BIC) correction, which are defined for AIC as
\deqn{\mathrm{AIC} = \hat{D}_{12} - (2n)^{-1}\log(n)\biggl[\mathrm{dim}(\Theta_2) - \mathrm{dim}(\Theta_1)\biggr]\pm\Phi^{(-1)}(1-\alpha){\times}\hat\sigma_{12}/\sqrt{n}\mbox{,}}
and for BIC as
\deqn{\mathrm{BIC} = \hat{D}_{12} - (2n)^{-1}\log(n)\biggl[\mathrm{dim}(\Theta_2) - \mathrm{dim}(\Theta_1)\biggr]\pm\Phi^{(-1)}(1-\alpha){\times}\hat\sigma_{12}/\sqrt{n}\mbox{.}}
The AIC and BIC corrections incorporate the number of parameters in the copula and for all else being equal the copula with the fewer parameters is preferable. If the two copulas being compared have equal number of parameters than the AIC and BIC equate to \eqn{\hat{D}_{12}} and the same confidence interval because the difference \eqn{[\mathrm{dim}(\Theta_2) - \mathrm{dim}(\Theta_1)]} is zero.

Joe (2015, p. 258) reports that these three intervals can be used for \emph{diagnostic inference} as follows. If an interval contains 0 (zero), then copulas \eqn{\mathbf{C}_1(\Theta_1)} and \eqn{\mathbf{C}_2(\Theta_2)} are not considered significantly different. If the interval does not contain 0, then copula \eqn{\mathbf{C}_1(\Theta_1)} or \eqn{\mathbf{C}_2(\Theta_2)} is the better fit depending on whether the interval is completely below 0 (thus \eqn{\mathbf{C}_1(\Theta_1)} better fit) or above 0 (thus \eqn{\mathbf{C}_2(\Theta_2)} better fit), respectively. Joe (2015) goes on the emphasize that \dQuote{the procedure compares different [copulas] and assesses whether they provide similar fits to the data. [The procedure] does not assess whether [either copula] is a good enough fit.}
}
\usage{
vuongCOP(u, v=NULL, cop1=NULL, cop2=NULL, para1=NULL, para2=NULL,
                    alpha=0.05, method=c("D12", "AIC", "BIC"), ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction and if \code{NULL} then \code{u} is treated as a two column \R \code{data.frame};}
  \item{cop1}{A copula function corresponding to copula \eqn{f_1} in Vuong's Procedure;}
  \item{para1}{Vector of parameters or other data structure, if needed, to pass to the copula \eqn{f_1};}
  \item{cop2}{A copula function corresponding to copula \eqn{f_2} in Vuong's Procedure;}
  \item{para2}{Vector of parameters or other data structure, if needed, to pass to the copula \eqn{f_2};}
  \item{alpha}{The \eqn{\alpha} in Vuong's Procedure, which results in the \eqn{100{\times}(1 - \alpha)} confidence interval (two-tail);}
  \item{method}{The interval to evaluate as to position of the respective statistic form the comparison of the two copulas;}
  \item{...}{Additional arguments to pass to the \code{\link{densityCOP}} function.}
}
\value{
  An \R \code{list} is returned having the following components:
  \item{title}{A descriptive title of the procedure;}
  \item{result}{A textual description of the result of Vuong's Procedure;}
  \item{D12}{An \R \code{data.frame} containing the lower and upper bounds of Vuong's \eqn{D} at the respective confidence interval percentage along with \eqn{\hat{D}_{12}} and \eqn{\sigma^2_{12}};}
  \item{AIC}{An \R \code{data.frame} containing the lower and upper bounds of Vuong's \eqn{\mathrm{AIC}} at the respective confidence interval percentage along with \eqn{\mathrm{AIC}}; and}
  \item{BIC}{An \R \code{data.frame} containing the lower and upper bounds of Vuong's \eqn{\mathrm{BIC}} at the respective confidence interval percentage along with \eqn{\mathrm{BIC}}.}
}
\note{
The \code{vuongCOP} function along with \code{\link{kullCOP}} and features of function \code{\link{densityCOPplot}} represent collective milestones towards \emph{copula inference} and diagnostics post fitting of copulas to the usual measures of association such as Kendall's Tau (\eqn{\tau_K}) and Spearman's Rho (\eqn{\rho_S}) and their copula counterparts (\eqn{\tau_\mathbf{C}} (\code{\link{tauCOP}}) and \eqn{\rho_\mathbf{C}} (\code{\link{rhoCOP}})).

For an example application, imagine a problem of say low springflow risk at \dQuote{nearby springs} that jointly should converge in the lower tail because drought usually has a strong regional impact. First, it is necessary to form a reflection of the Gumbel-Hougaard (\eqn{\mathbf{GH}(u,v; \Theta_{\mathbf{GH}})}) coupla (\code{\link{GHcop}}) but parameter estimation using \eqn{\tau_\mathbf{C}} is the same because sample \eqn{\hat\tau_K} is invariant to reflection.
\preformatted{
"rGHcop" <- function(u,v, ...) { u + v - 1 + GHcop(1-u, 1-v, ...) }
set.seed(385) # setting so reported quantities here are reproducible
}
The prior code also sets a seed on the pseudo-random number generator so that reported values here are reproducible. The reflected \eqn{\mathbf{GH}(u,v; \Theta_{\mathbf{GH}})} is denoted \eqn{\mathbf{rGH}(u,v; \Theta_{\mathbf{rGH}})}.

Second, the \eqn{\mathbf{PSP}(u,v)} copula (\code{\link{PSP}}) is chosen as the parent distribution, and this copula has no parameter. The \eqn{\mathbf{PSP}} has lower-tail dependency, which will be important as discussion unfolds. The following two lines of code establish a sample size to be drawn from the \eqn{\mathbf{PSP}} and then simulates from that copula. The color grey is used for the simulated values on the figure produced by \code{\link{simCOP}}, which forms a background example of the joint structure of the \eqn{\mathbf{PSP}} copula. By inspection of the so-produced graphic, it is obvious that there is contraction in the lower-left corner of the plot, which is a geometric representation of tail dependency. The lower-tail dependency thus phenomenalogically says that there is joint interconnect during low springflow conditions---both springs are likely to be at lowflow simultaneously. The variable \code{UV} contains the bivariate data as uniform variables (nonexceedance probabilities \eqn{u} and \eqn{v}).
\preformatted{
n <- 390
UV <- simCOP(cop=PSP, n=n, col=8, pch=16) # simulate and form the base image
}

The Plackett (\eqn{\mathbf{PL}(u,v; \Theta_{\mathbf{PL}})}) copula (\code{\link{PLACKETTcop}}) and the \eqn{\mathbf{rGH}(u,v; \Theta_{\mathbf{rGH}})} copula are chosen as candidate models of the \dQuote{unknown} parent. Both \eqn{\mathbf{PL}} and \eqn{\mathbf{rGH}} copulas use different \dQuote{measures of association} for their parameter estimation. So next, sample estimates of \eqn{\hat\tau_K} and \eqn{\hat\rho_S} are computed and the respective copula parameters,\eqn{\Theta_{\mathbf{PL}}}, \eqn{\Theta_{\mathbf{rGH}}}, respectively, estimated. And the sample value computations and parameter estimates set as shown in the following code:
\preformatted{
Rho <- cor(UV[,1], UV[,2], method="spearman") # Sample Spearman's Rho
Tau <- cor(UV[,1], UV[,2], method="kendall" ) # Sample Kendall's Tau
paraPL <- PLACKETTpar(rho=Rho)  # Rho = 0.4968900; paraPL = 5.054246
paraGH <- GHcop(tau=Tau)$para   # Tau = 0.3459363; paraGH = 1.528903
}

\emph{STEP 1---Compute Kullback-Leibler sample size:} The Kullback-Leibler divergences (\eqn{\mathrm{KL}(f|g)} and \eqn{\mathrm{KL}(g|f)}) are computed (\code{\link{kullCOP}}) for the evaluation of the sample size as appropriate for distinguishing between the two candidate copulas 95 percent of the time. The Kullback-Leibler sample size (\eqn{n_{fg}}) also is computed as the following code illustrates and provides additional commentary.
\preformatted{
# KL <- kullCOP(cop1=PLACKETTcop, para1=paraPL, cop2=rGHcop,      para2=paraGH)
# Errors wih a 'divergent integral' on the Plackett, let us adjust the deltas, the
# deluv, which is for densityCOP(), seems fix the numerics in this case.
KL <- kullCOP(cop1=PLACKETTcop, para1=paraPL,    # THIS CALL WORKS
              cop2=rGHcop,      para2=paraGH, deluv=0.0005) # CPU intensive
print(KL$KL.sample.size)  # n_{fg} = 212 sample size
}

Depending on the sample \eqn{\hat\tau_K} and \eqn{\hat\rho_S} coming from the simulation of the parent \eqn{\mathbf{PSP}} copula, the call to \code{\link{kullCOP}} will likely report different \eqn{n_{fg}} values because \eqn{n_{fg}(\mathbf{C}_1(\Theta_1), \mathbf{C}_1(\Theta_1)}. These sizes seem to generally be about \eqn{n_{fg}=200{-}260} and are dependent on the pseudo-random number seed. The result here is \eqn{n_{fg}=212} and thus the sample size \eqn{n=390} should be, by intuition, likely large enough to generally distinguish between the \eqn{\mathbf{PL}} and \eqn{\mathbf{rGH}} copulas at the respective \eqn{\hat\rho_S} and \eqn{\hat\tau_K} measures of association.

\emph{STEP 2---Perform Vuong's Procedure:} The Vuong's Procedure can now be made. Now watch the copula and parameter order in the next code for mistakes, the author has purposefully switched order here to draw attention to the need to make sure argument \code{cop1} has the correct parameter(s) for copula 1 (the \eqn{\mathbf{PL}}). The two calls to \code{\link{simCOP}} are made to graphically superimpose these simulations on top of the parent \eqn{\mathbf{PSP}}.
\preformatted{
VD <- vuongCOP(UV, cop2=GHcop, para2=paraGH, cop1=PLACKETTcop, para1=paraPL)
print(VD) # The results say "Copula 2 better" or GHcop (Gumbel-Hougaard is better)
set.seed(385) # seems harmless enough to reuse the seed to emphasize "fit"
TMP <- simCOP(cop=PLACKETTcop,para=paraPL, n=n, plot=FALSE,col=2,pch=16,cex=0.5)
set.seed(385) # seems harmless enough to reuse the seed to emphasize "fit"
TMP <- simCOP(cop=rGHcop,     para=paraGH, n=n, plot=FALSE,col=3,pch=16,cex=0.5)
rm(TMP) # just cleaning up the workspace.
}

Further discussion of Vuong's Procedure is informative. Simply speaking, the result is that coupla 2 (\eqn{\mathbf{rGH}}) has better fit than copula 1 (\eqn{\mathbf{PL}}). The 95-percent confident limits from Vuong's Procedure having \eqn{\hat{D}_{12} = 0.0311}, \eqn{\hat\sigma_{12} = 0.2439}, and \eqn{n=390} are \eqn{0.00687 < \hat{D}_{12} < 0.0553}. This interval does not contain zero and is greater than zero. But see how near the lower bounds of the Vuong's \eqn{\hat{D}_{12}} are close to zero, so the call is somewhat close. If the parameters \eqn{\Theta_{\mathbf{PL}}} and \eqn{\Theta_{\mathbf{rGH}}} remain unchanged, an \eqn{n > 390} would pull the lower bounds up and  \eqn{n < 390} would push the lower bounds lower. This seems consistent with the generalized expected range of \eqn{n_{fg}} as reported above.

\emph{STEP 3---Comparison of lower-tail dependency parameters:} What does the tail dependency do for inference? The important aspect of the three produced graphics by \code{\link{taildepCOP}} in the code that follows is that the \eqn{\mathbf{rGH}} has lower-tail dependency and the \eqn{\mathbf{PL}} does not, so based on inspection the reflection of \eqn{\mathbf{GH}} is superior.  Let us check this by comparing the tail dependency parameters (\eqn{\lambda^L_\mathbf{C}}) for each of the three copulas and note that effective sample estimators or tail dependency seem to remain elusive in the literature. Numeric focus is only on the lower tail but the three graphics are informative.
\preformatted{
taildepCOP(cop=PSP,                      plot=TRUE)$lambdaL # = 1/2
taildepCOP(cop=PLACKETTcop, para=paraPL, plot=TRUE)$lambdaL # = ZERO
taildepCOP(cop=rGHcop,      para=paraGH, plot=TRUE)$lambdaL # = 0.42641
}

So the \eqn{\mathbf{rGH}} has a tail dependency more similar to the true model compared to the \eqn{\mathbf{PL}}. Hence for this example, the \eqn{\mathbf{rGH}} is clearly a superior fitting model in terms of \emph{Vuong's Procedure} (fit alone) and the tail dependency then is used as a follow up to shown that the \eqn{\mathbf{rGH}} might be \dQuote{good enough} an approximation to the \eqn{\mathbf{PSP}}. The efficacy of manually reflecting the \eqn{\mathbf{GH}} copula into a \dQuote{new} form as \eqn{\mathbf{rGH}} is demonstrated. Users are strongly encouraged to review the so-produced graphic from the \code{\link{simCOP}} call several listings back for \eqn{n=390}, and lastly, this example is one for which absence of the argument \code{snv} (standard normal variate [scores]) by \code{\link{simCOP}} makes the tail dependency issue for the sample size more prominent in the graphic.

\emph{STEP 4---Perform further graphical evaluation using copula density plots:} Graphical depiction of copula density contours by the \code{\link{densityCOPplot}} function supports the conclusion that the \eqn{\mathbf{rGH}} is the superior model relative to the \eqn{\mathbf{PL}}. The so-produced graphic obviously shows that the \eqn{\mathbf{rGH}} strongly mimics the shape of the parent \eqn{\mathbf{PSP}}.
\preformatted{
densityCOPplot(cop=PSP, contour.col=8) # grey is the parent bivariate density
densityCOPplot(cop=PLACKETTcop,para=paraPL, contour.col=3,ploton=FALSE) # green
densityCOPplot(cop=rGHcop,     para=paraGH, contour.col=2,ploton=FALSE) # red
}

\emph{STEP 5---Compute L-comoments of the data and compare to stochastically estimate the sampling distributions:} An open research problem is the role that L-comoments might play in either copula estimation or inference. (There being virtually no literature on the topic.) Because the \eqn{\rho_S} and \eqn{\tau_K} are used for the parameter estimation for the two copulas in this example, here we can explore inference. The following code listing is CPU intensive. First, the L-correlation, L-coskew, and L-cokurtosis values are computed from the simulated sample by the \code{lcomoms2} function of the \pkg{lmomco} package. Second and third, the respective sampling distributions of these L-comoments (\code{\link{lcomCOPpv}}) for the two copulas are computed.
\preformatted{
UVlmr <- lmomco::lcomoms2(UV,nmom=4) # The sample L-comoments
set.seed(12) # just so similar numerics as shown will result
GHlmr <- lcomCOPpv(n, UVlmr, cop=rGHcop,      para=paraGH) # NONREJECTION
# LcomType      n      Mean  Lscale    Lskew   Lkurt sample.est p.value signif
#    Tau3[12] 390  -0.06946 0.01825  0.04427 0.12119   -0.11188 0.08852      .
#    Tau3[21] 390  -0.06727 0.02090  0.04106 0.12917   -0.10673 0.14157      -
# Tau3[12|21] 390  -0.06836 0.01716  0.04925 0.11780   -0.10931 0.08146      .
#    Tau4[12] 390   0.04959 0.01693 -0.01719 0.10020    0.04183 0.39168      -
#    Tau4[21] 390   0.05123 0.01611 -0.06888 0.13790    0.07804 0.17491      -
# Tau4[12|21] 390   0.05041 0.01335 -0.02057 0.12025    0.05994 0.35013      -

PLlmr <- lcomCOPpv(n, UVlmr, cop=PLACKETTcop, para=paraPL) # REJECT PLACKETT
#  LcomType     n      Mean  Lscale    Lskew   Lkurt sample.est p.value signif
#    Tau3[12] 390  -0.00267 0.02132  0.01568 0.09588   -0.11188 0.00128     **
#    Tau3[21] 390  -0.00112 0.02022 -0.00663 0.13338   -0.10673 0.00189     **
# Tau3[12|21] 390  -0.00189 0.01757  0.00916 0.10233   -0.10931 0.00019    ***
#    Tau4[12] 390   0.00153 0.01653 -0.03335 0.12462    0.04183 0.07925      .
#    Tau4[21] 390   0.00361 0.01851 -0.01869 0.12052    0.07804 0.00929     **
# Tau4[12|21] 390   0.00257 0.01362 -0.01202 0.10789    0.05994 0.00743     **
}
Because each copula was fit to a measure of association, the p-values for the L-correlations are all nonsignificant and should produce p-values quite near to the 50th percentile because of how the copulas were fit. So here, the L-correlation is noninformative on fit even though it might have some role because it is asymmetrical unlike that statistics \eqn{\tau_K} and \eqn{\rho_S}. The results in variable \code{GHlmr} show no significant p-values (all p-values \eqn{{>}0.05 = (\alpha=0.1)/2)}) for either L-coskew or L-cokurtosis. Thus, the \eqn{\mathbf{rGH}} copula can not be rejected. The results in \code{PLlmr} show many p-values \eqn{{<}0.05 = (\alpha=0.1)/2} for both L-coskew and L-cokurtosis; the \eqn{\mathbf{PL}} copula thus can be rejected. The experimental L-comoment analysis is consistent with results with Vuong's Procedure.

\emph{Vuong's Procedure} however does not address adequacy of fit---it just evaluates which coupla fits better. The inspection of the tail dependency results previously shown along with the L-coskew and L-cokurtosis of the sample being well within the sample distribution suggests that the \eqn{\mathbf{rGH}} is a adequate mimic of the \eqn{\mathbf{PSP}} copula. Some open research questions are the numerical performance of the L-comoments as simulation sample size becomes large and whether or not the L-comoments should be computed on the probabilities \eqn{\{u, v\}} or whether conversion to normal scores should be made and if so, should adjustment by the \emph{Hazen plotting positions} (\eqn{u_i = (r_i - 0.5)/n} for rank \eqn{r_i}) that Joe (2015) repeatedly advocates when standard normal variates (scores) [\eqn{z_i = \Phi^{(-1)}(u_i)}) for quantile function of Standard Normal distribution \eqn{\Phi(0,1)}] are used with copulas. Collectively, Nelsen (2006) and Salvadori \emph{et. al} (2007) are both silent on the matter of normal score conversion, and in conclusion Nelsen (2006), Salvadori \emph{et. al} (2007), and Joe (2015) also are all silent on L-comoment applications with copulas.
}
\references{
Joe, H., 2015, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{densityCOP}}, \code{\link{kullCOP}}, \code{\link{simCOP}}}
\examples{
# See extended code listings and discussion in the Note section
}
\keyword{copula (inference)}
\keyword{copula (density)}
\concept{Vuong procedure}
\concept{Akaike}
\concept{AIC}
\concept{BIC}
\concept{Vuong}
\concept{Kullback-Leibler}

