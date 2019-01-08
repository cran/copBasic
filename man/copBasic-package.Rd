\encoding{utf8}
\name{copBasic-package}
\docType{package}
\alias{copBasic-package}
\title{Basic Theoretical Copula, Empirical Copula, and Various Utility Functions}
\description{
The \pkg{copBasic} package is oriented around \emph{bivariate copula theory} and mathematical operations closely follow the standard texts by Nelsen (2006) and Joe (2014) as well as select other references. Another recommended text is Salvadori \emph{et al.} (2007) and is cited herein, but about half of that excellent book concerns univariate applications. The primal objective of \pkg{copBasic} is to provide a basic application programming interface (API) to numerous results shown by authoritative texts on copulas. It is hoped in part that the package will help new copula students in their self study, potential course work, and applied circumstances.

\bold{Notes on copulas that are supported.} The author has focused on pedagogical aspects of copulas, and this package is a \emph{diary} of sorts in self-guided learning. Originally, the author did not implement many copulas in the \pkg{copBasic} in order to deliberately avoid redundancy to that support such as it exists on the \R CRAN. Though as time has progressed, other copulas have been added occassionally based on needs of the user community, need to show some specific concept in the general theory, or test algorithms. For example, the Clayton copula (\code{\link{CLcop}}) is a late arriving addition to the \pkg{copBasic} package (\emph{c.}2017), which was added to assist a specific user.


\bold{Helpful Navigation of Copulas Implemented in the copBasic Package}
Some helpful pivot points into the copulas implemented are listed in the following table:
\tabular{lclr}{
\bold{Name} \tab \bold{Symbol} \tab \bold{Function} \tab \bold{Concept} \cr
Lower-bounds copula \tab \eqn{\mathbf{W}(u,v)} \tab \code{\link{W}} \tab copula\cr
Independence copula \tab \eqn{\mathbf{\Pi}(u,v)} \tab \code{\link{P}} \tab copula\cr
Upper-bounds copula \tab \eqn{\mathbf{M}(u,v)} \tab \code{\link{M}} \tab copula\cr
\enc{Fréchet}{Frechet} Family copula \tab \eqn{\mathbf{FF}(u,v)} \tab \code{\link{FRECHETcop}} \tab copula\cr
Ali--Mikhail--Haq copula \tab \eqn{\mathbf{AMH}(u,v)} \tab \code{\link{AMHcop}} \tab copula\cr
Clayton copula \tab \eqn{\mathbf{CL}(u,v)} \tab \code{\link{CLcop}} \tab copula\cr
Farlie--Gumbel--Morgenstern (generalized) copula \tab \eqn{\mathbf{FGM}(u,v)} \tab \code{\link{FGMcop}} \tab copula\cr
Galambos copula \tab \eqn{\mathbf{GL}(u,v)} \tab \code{\link{GLcop}} \tab copula\cr
Gumbel--Hougaard copula \tab \eqn{\mathbf{GH}(u,v)} \tab \code{\link{GHcop}} \tab copula\cr
\enc{Hüsler}{Husler}--Reiss copula \tab \eqn{\mathbf{HR}(u,v)} \tab \code{\link{HRcop}} \tab copula\cr
Joe B5 copula \tab \eqn{\mathbf{B5}(u,v)} \tab \code{\link{JOcopB5}} \tab copula\cr
Nelsen eq.4-2-12 copula \tab \eqn{\mathbf{N4212cop}(u,v)} \tab \code{\link{N4212cop}} \tab copula\cr
Pareto copula \tab \eqn{\mathbf{PA}(u,v)} \tab \code{\link{PLcop}} \tab copula\cr
Plackett copula \tab \eqn{\mathbf{PL}(u,v)} \tab \code{\link{PLcop}} \tab copula\cr
PSP copula \tab \eqn{\mathbf{PSP}(u,v)} \tab \code{\link{PSP}} \tab copula\cr
Raftery copula \tab \eqn{\mathbf{RF}(u,v)} \tab \code{\link{RFcop}} \tab copula\cr
g-EV copula (Gaussian extreme value) \tab \eqn{\mathbf{gEV}(u,v)} \tab \code{\link{gEVcop}} \tab copula\cr
t-EV copula (t-distribution extreme value) \tab \eqn{\mathbf{tEV}(u,v)} \tab \code{\link{tEVcop}} \tab copula\cr
}


The language and vocabulary of copulas is formidable. The author (Asquith) has often emphasized \dQuote{vocabulary} words in \emph{italics}, which is used extensively and usually near the opening of function-by-function documentation to identify vocabulary words, such as \emph{survival copula} (see \code{\link{surCOP}}). This syntax tries to mimic and accentuate the word usage in Nelsen (2006) and Joe (2014). The \pkg{copBasic} package was started well before Joe (2014) so more citations to Nelsen (2006) are made.

The italics then are used to draw connections between concepts. In conjunction with the summary of functions in \bold{copBasic-package}, the extensive cross referencing to functions and expansive keyword indexing should be beneficial. The author had no experience with copulas prior to a chance happening upon Nelsen (2006) in \emph{c.}2008. The \pkg{copBasic} package is a personal \emph{tour de force} in self-guided learning. Hopefully, this package and user's manual will be helpful to others.

A few comments on notation herein are needed. A bold math typeface is used to represent a copula such as \eqn{\mathbf{\Pi}} (see \code{\link{P}}) for the \emph{independence copula}. The syntax \eqn{\mathcal{R}\times\mathcal{R} \equiv \mathcal{R}^2} denotes the orthogonal domain of two real numbers, and \eqn{[0,1]\times [0,1]} \eqn{\equiv} \eqn{\mathcal{I}\times\mathcal{I} \equiv \mathcal{I}^2} denotes the orthogonal domain on the unit square of probabilities. Limits of integration \eqn{[0,1]} or \eqn{[0,1]^2} involving copulas are thus shown as \eqn{\mathcal{I}} and \eqn{\mathcal{I}^2}, respectively.

The random variables \eqn{X} and \eqn{Y} respectively denote the horizontal and vertical directions in \eqn{\mathcal{R}^2}. Their probabilistic counterparts are uniformly distributed random variables on \eqn{[0,1]}, are respectively denoted as \eqn{U} and \eqn{V}, and necessarily also are the respective directions in \eqn{\mathcal{I}^2} (\eqn{U} denotes the horizontal, \eqn{V} denotes the vertical). Often realizations of these random variables are respectively \eqn{x} and \eqn{y} for \eqn{X} and \eqn{Y} and \eqn{u} and \eqn{v} for \eqn{U} and \eqn{V}.

There is an obvious difference between nonexceedance probability \eqn{F} and its complement---exceedance probability \eqn{1-F}. Both \eqn{u} and \eqn{v} herein are in \dQuote{units} of nonexceedance probability. Arguments to many functions herein are \code{u} \eqn{= u} and \code{v} \eqn{= v} and are almost \emph{exclusively nonexceedance} but there are instances for which the probability arguments are \code{u} \eqn{= 1 - u = u'} and \code{v} \eqn{= 1 - v = v'}.\cr

\bold{Helpful Navigation of the copBasic Package}

Some helpful pivot points into the package are listed in the following table:
\tabular{lclr}{
\bold{Name} \tab \bold{Symbol} \tab \bold{Function} \tab \bold{Concept} \cr
Copula  \tab \eqn{\mathbf{C}(u,v)} \tab \code{\link{COP}} \tab copula theory\cr
Survival copula \tab \eqn{\hat\mathbf{C}(u',v')} \tab \code{\link{surCOP}} \tab copula theory\cr
Joint survival function \tab \eqn{\overline{\mathbf{C}}(u,v)} \tab \code{\link{surfuncCOP}} \tab copula theory\cr
Co-copula \tab \eqn{\mathbf{C}^\star(u',v')} \tab \code{\link{coCOP}} \tab copula theory\cr
Dual of a copula \tab \eqn{\tilde\mathbf{C}(u,v)} \tab \code{\link{duCOP}} \tab copula theory\cr
Primary copula diagonal  \tab \eqn{\delta(t)} \tab \code{\link{diagCOP}} \tab copula theory\cr
Secondary copula diagonal  \tab \eqn{\delta^\star(t)} \tab \code{\link{diagCOP}} \tab copula theory\cr
Inverse copula diagonal \tab \eqn{\delta^{(-1)}(f)} \tab \code{\link{diagCOPatf}} \tab copula theory\cr
Joint probability \tab \eqn{{-}{-}} \tab \code{\link{jointCOP}} \tab copula theory\cr
Bivariate L-moments \tab \eqn{\delta^{[\ldots]}_{k;\mathbf{C}}} \tab \code{\link{bilmoms}} \tab bivariate moments\cr
Bivariate L-comoments \tab \eqn{\tau^{[\ldots]}_{k;\mathbf{C}}} \tab \code{\link{bilmoms}} \tab bivariate moments\cr
Bivariate L-comoments \tab \eqn{\tau^{[\ldots]}_{k;\mathbf{C}}} \tab \code{\link{lcomCOP}} \tab bivariate moments\cr
Blomqvist Beta \tab \eqn{\beta_\mathbf{C}} \tab \code{\link{blomCOP}} \tab bivariate association\cr
Gini Gamma \tab \eqn{\gamma_\mathbf{C}} \tab \code{\link{giniCOP}} \tab bivariate association\cr
Hoeffding Phi \tab \eqn{\Phi_\mathbf{C}} \tab \code{\link{hoefCOP}} \tab bivariate association\cr
Nu-Skew \tab \eqn{\nu_\mathbf{C}} \tab \code{\link{nuskewCOP}} \tab bivariate moments\cr
Nu-Star (skew) \tab \eqn{\nu^\star_\mathbf{C}} \tab \code{\link{nustarCOP}} \tab bivariate moments\cr
Lp distance \tab  \eqn{\Phi_\mathbf{C} \rightarrow L_p} \tab \code{\link{LpCOP}} \tab bivariate association\cr
Kendall Tau \tab \eqn{\tau_\mathbf{C}} \tab \code{\link{tauCOP}} \tab bivariate association\cr
Kendall Measure \tab \eqn{K_\mathbf{C}(z)} \tab \code{\link{kmeasCOP}} \tab copula theory\cr
Kendall Function \tab \eqn{F_K(z)} \tab \code{\link{kfuncCOP}} \tab copula theory\cr
Inverse Kendall Function \tab \eqn{F_K^{(-1)}(z)} \tab \code{\link{kfuncCOPinv}} \tab copula theory\cr
An L-moment of \eqn{F_K(z)} \tab \eqn{\lambda_r(F_K)} \tab\code{\link{kfuncCOPlmom}} \tab L-moment theory\cr
L-moments of \eqn{F_K(z)} \tab \eqn{\lambda_r(F_K)} \tab \code{\link{kfuncCOPlmoms}} \tab L-moment theory\cr
Semi-correlations (negatives) \tab \eqn{\rho_N^{-}(a)} \tab \code{\link{semicorCOP}} \tab bivariate tail association\cr
Semi-correlations (positives) \tab \eqn{\rho_N^{+}(a)} \tab \code{\link{semicorCOP}} \tab bivariate tail association\cr
Spearman Footrule \tab \eqn{\psi_\mathbf{C}} \tab \code{\link{footCOP}} \tab bivariate association\cr
Spearman Rho \tab \eqn{\rho_\mathbf{C}} \tab \code{\link{rhoCOP}} \tab bivariate association\cr
Schweizer--Wolff Sigma \tab \eqn{\sigma_\mathbf{C}} \tab \code{\link{wolfCOP}} \tab bivariate association\cr
Density of a copula\tab \eqn{c(u,v)} \tab \code{\link{densityCOP}} \tab copula density\cr
Density visualization \tab \eqn{{-}{-}} \tab \code{\link{densityCOPplot}} \tab copula density\cr
Empirical copula \tab \eqn{\mathbf{C}_n(u,v)} \tab \code{\link{EMPIRcop}} \tab copula\cr
Empirical simulation \tab \eqn{{-}{-}} \tab \code{\link{EMPIRsim}} \tab copula simulation\cr
Empirical simulation \tab \eqn{{-}{-}} \tab \code{\link{EMPIRsimv}} \tab copula simulation\cr
Empirical copulatic surface \tab \eqn{{-}{-}} \tab \code{\link{EMPIRgrid}} \tab copulatic surface\cr
Parametric copulatic surface \tab \eqn{{-}{-}} \tab \code{\link{gridCOP}} \tab copulatic surface\cr
Parametric simulation \tab \eqn{{-}{-}} \tab \code{\link{simCOP}} or   \code{\link{rCOP}}\tab copula simulation\cr
Parametric simulation \tab \eqn{{-}{-}} \tab \code{\link{simCOPmicro}} \tab copula simulation\cr
Maximum likelihood \tab \eqn{\mathcal{L}(\Theta_d)} \tab \code{\link{mleCOP}} \tab copula fitting\cr
Akaike information criterion \tab \eqn{\mathrm{AIC}_\mathbf{C}} \tab \code{\link{aicCOP}} \tab goodness-of-fit\cr
Bayesian information criterion \tab \eqn{\mathrm{BIC}_\mathbf{C}} \tab \code{\link{bicCOP}} \tab goodness-of-fit\cr
Root mean square error \tab \eqn{\mathrm{RMSE}_\mathbf{C}} \tab \code{\link{rmseCOP}} \tab goodness-of-fit\cr
Another goodness-of-fit \tab \eqn{T_n} \tab \code{\link{statTn}} \tab goodness-of-fit\cr
}

Several of the functions listed above are measures of \dQuote{bivariate association.} Two of the measures (\emph{Kendall Tau}, \code{\link{tauCOP}}; \emph{Spearman Rho}, \code{\link{rhoCOP}}) are widely known. \R provides native support for their sample estimation of course, but each function can be used to call the \code{cor()} function in \R for parallelism to the other measures of this package. The other measures (\emph{Blomqvist Beta}, \emph{Gini Gamma}, \emph{Hoeffding Phi}, \emph{Schweizer--Wolff Sigma}, \emph{Spearman Footrule}) support sample estimation by specially formed calls to their respective functions: \code{\link{blomCOP}}, \code{\link{giniCOP}}, \code{\link{hoefCOP}}, \code{\link{wolfCOP}}, and \code{\link{footCOP}}. Gini Gamma (\code{\link{giniCOP}}) documentation (also \code{\link{joeskewCOP}}) shows extensive use of theoretical and sample compuations for these and other functions.

Concerning \emph{goodness-of-fit} and although not quite the same as copula properties (such as \dQuote{correlation}) per se as the coefficients aforementioned in the prior paragraph, three goodness-of-fit metrics of a copula compared to the empirical copula, which are all based on \emph{mean square error} (MSE), are \code{\link{aicCOP}}, \code{\link{bicCOP}}, and \code{\link{rmseCOP}}. This triad of functions is useful for making decisions on whether a copula is more favorable than another to a given dataset. However, because they are genetically related by using MSE and if these are used for copula fitting by minimization, the fits will be identical. A statement of \dQuote{not quite the same} is made because the previously described copula properties are generally defined as types of deviations from other copulas (such as \code{\link{P}}). Another goodness-of-fit statistic is \code{\link{statTn}}, which is based on magnitude summation of fitted copula difference from the empirical copula. These four (\code{\link{aicCOP}}, \code{\link{bicCOP}}, \code{\link{rmseCOP}}, and \code{\link{statTn}}) collectively are relative simple and readily understood measures. These bulk sample statistics are useful, but generally thought to not capture the nuances of tail behavior (\code{\link{semicorCOP}} and \code{\link{taildepCOP}} might be useful).

\emph{Bivariate skewness} measures are supported in the functions \code{\link{joeskewCOP}} (\code{\link{nuskewCOP}} and \code{\link{nustarCOP}}) and \code{\link{uvlmoms}} (\code{\link{uvskew}}). Extensive discussion and example computations of bivariate skewness are provided in the \code{\link{joeskewCOP}} documentation.  Lastly, so-called \emph{bivariate L-moments} and \emph{bivariate L-comoments} of a copula are directly computable in \code{\link{bilmoms}}, and that function is the theoretical counterpart to the \emph{sample L-comoments} long provided in the \pkg{lmomco} package.

Bivariate random simulation by several functions is identified in the previous table. The \pkg{copBasic} package explicitly uses only \emph{conditional simulation} also known as the \emph{conditional distribution method} for \emph{random variate} generation following Nelsen (2006, pp. 40--41) (see also \code{\link{simCOPmicro}}, \code{\link{simCOP}}). The \emph{numerical derivatives} (\code{\link{derCOP}} and \code{\link{derCOP2}}) and their \emph{inversions} (\code{\link{derCOPinv}} and \code{\link{derCOPinv2}}) represent the foundation of the conditional simulation. There are other methods in the literature and available in other \R packages, and a comparison of some methods is made in the \bold{Examples} section of the Gumbel--Hougaard copula (\code{\link{GHcop}}).

Several functions in \pkg{copBasic} make the distinction between \eqn{V} with respect to (\emph{wrt}) \eqn{U} and \eqn{U} \emph{wrt} \eqn{V}, and a guide for the nomenclature involving \emph{wrt} distinctions is listed in the following table:
\tabular{lclr}{
\bold{Name} \tab \bold{Symbol} \tab \bold{Function} \tab \bold{Concept} \cr
Copula inversion \tab \eqn{V} \emph{wrt} \eqn{U} \tab \code{\link{COPinv}} \tab copula operator\cr
Copula inversion \tab \eqn{U} \emph{wrt} \eqn{V} \tab \code{\link{COPinv2}} \tab copula operator\cr
Copula derivative \tab \eqn{\delta \mathbf{C}/\delta u} \tab \code{\link{derCOP}} \tab copula operator\cr
Copula derivative \tab \eqn{\delta \mathbf{C}/\delta v} \tab \code{\link{derCOP2}} \tab copula operator\cr
Copula derivative inversion \tab \eqn{V} \emph{wrt} \eqn{U} \tab \code{\link{derCOPinv}} \tab copula operator\cr
Copula derivative inversion \tab \eqn{U} \emph{wrt} \eqn{V} \tab \code{\link{derCOPinv2}} \tab copula operator\cr
Joint curves \tab \eqn{t \mapsto \mathbf{C}(u=U, v)} \tab \code{\link{joint.curvesCOP}} \tab copula theory\cr
Joint curves \tab \eqn{t \mapsto \mathbf{C}(u, v=V)} \tab \code{\link{joint.curvesCOP2}} \tab copula theory\cr
Level curves \tab \eqn{t \mapsto \mathbf{C}(u=U, v)} \tab \code{\link{level.curvesCOP}} \tab copula theory\cr
Level curves \tab \eqn{t \mapsto \mathbf{C}(u, v=V)} \tab \code{\link{level.curvesCOP2}} \tab copula theory\cr
Level set \tab \eqn{V} \emph{wrt} \eqn{U} \tab \code{\link{level.setCOP}} \tab copula theory\cr
Level set \tab \eqn{U} \emph{wrt} \eqn{V} \tab \code{\link{level.setCOP2}} \tab copula theory\cr
Median regression \tab \eqn{V} \emph{wrt} \eqn{U} \tab \code{\link{med.regressCOP}} \tab copula theory\cr
Median regression \tab \eqn{U} \emph{wrt} \eqn{V} \tab \code{\link{med.regressCOP2}} \tab copula theory\cr
Quantile regression \tab \eqn{V} \emph{wrt} \eqn{U} \tab \code{\link{qua.regressCOP}} \tab copula theory\cr
Quantile regression \tab \eqn{U} \emph{wrt} \eqn{V} \tab \code{\link{qua.regressCOP2}} \tab copula theory\cr
Copula section \tab \eqn{t \mapsto \mathbf{C}(t,a)} \tab \code{\link{sectionCOP}} \tab copula theory\cr
Copula section \tab \eqn{t \mapsto \mathbf{C}(a,t)} \tab \code{\link{sectionCOP}} \tab copula theory\cr
}

The previous two tables do not include all of the myriad of special functions to support similar operations on \emph{empirical copulas}. All empirical copula operators and utilites are prepended with \code{EMPIR} in the function name. An additional note concerning package nomenclature is that an appended \dQuote{\code{2}} to a function name indicates \eqn{U} \emph{wrt} \eqn{V} (\emph{e.g.} \code{\link{EMPIRgridderinv2}} for an inversion of the partial derivatives \eqn{\delta \mathbf{C}/\delta v} across the grid of the empirical copula).

Some additional functions to compute often salient features or characteristics of copulas or bivariate data, including functions for bivariate inference or goodness-of-fit, are listed in the following table:
\tabular{lclr}{
\bold{Name} \tab \bold{Symbol} \tab \bold{Function} \tab \bold{Concept} \cr
Left-tail decreasing \tab \eqn{V} \emph{wrt} \eqn{U}  \tab \code{\link{isCOP.LTD}} \tab bivariate association\cr
Left-tail decreasing \tab \eqn{U} \emph{wrt} \eqn{V}  \tab \code{\link{isCOP.LTD}} \tab bivariate association\cr
Right-tail increasing \tab \eqn{V} \emph{wrt} \eqn{U}  \tab \code{\link{isCOP.RTI}} \tab bivariate association\cr
Right-tail increasing \tab \eqn{U} \emph{wrt} \eqn{V}  \tab \code{\link{isCOP.RTI}} \tab bivariate association\cr
Pseudo-polar representation \tab \eqn{(\widehat{S},\widehat{W})} \tab \code{\link{psepolar}} \tab extremal dependency\cr
Tail concentration function \tab \eqn{q_\mathbf{C}(t)} \tab \code{\link{tailconCOP}} \tab bivariate tail association\cr
Tail (lower) dependency \tab \eqn{\lambda^L_\mathbf{C}} \tab \code{\link{taildepCOP}} \tab bivariate tail association\cr
Tail (upper) dependency \tab \eqn{\lambda^U_\mathbf{C}} \tab \code{\link{taildepCOP}} \tab bivariate tail association\cr
Tail (lower) order \tab \eqn{\kappa^L_\mathbf{C}} \tab \code{\link{tailordCOP}} \tab bivariate tail association\cr
Tail (upper) order \tab \eqn{\kappa^U_\mathbf{C}} \tab \code{\link{tailordCOP}} \tab bivariate tail association\cr
Neg'ly quadrant dependency \tab NQD \tab \code{\link{isCOP.PQD}} \tab bivariate association\cr
Pos'ly quadrant dependency \tab PQD \tab \code{\link{isCOP.PQD}} \tab bivariate association\cr
Permutation symmetry \tab \eqn{\mathrm{permsym}} \tab \code{\link{isCOP.permsym}} \tab copula symmetry\cr
Radial symmetry \tab \eqn{\mathrm{radsym}} \tab \code{\link{isCOP.radsym}} \tab copula symmetry\cr
Skewness (Joe, 2014) \tab \eqn{\eta(p; \psi)} \tab \code{\link{uvskew}} \tab bivariate skewness\cr
Kullback--Leibler divergence \tab \eqn{\mathrm{KL}(f|g)} \tab \code{\link{kullCOP}} \tab bivariate inference\cr
KL sample size \tab \eqn{n_{f\!g}} \tab \code{\link{kullCOP}} \tab bivariate inference\cr
Vuong's Procedure \tab \eqn{{-}{-}} \tab \code{\link{vuongCOP}} \tab bivariate inference\cr
Spectral measure \tab \eqn{H(w)} \tab \code{\link{spectralmeas}} \tab extremal dependency inference\cr
Stable tail dependence \tab \eqn{\widehat{l}(x,y)} \tab \code{\link{stabtaildepf}} \tab extremal dependency inference\cr
L-comoments (samp. distr.) \tab \eqn{{-}{-}} \tab \code{\link{lcomCOPpv}} \tab experimental bivariate inference\cr
}

The following \bold{Table of Probabilities} lists important relations between various joint probability concepts, the copula, nonexceedance probabilities \eqn{u} and \eqn{v}, and exceedance probabilities \eqn{u'} and \eqn{v'}. A compact summary of these probability relations has obvious usefulness. The notation \eqn{[\ldots, \ldots]} is to read as \eqn{[\ldots \mathrm{\ and\ } \ldots]}, and the \eqn{[\ldots | \ldots]} is to be read as \eqn{[\ldots \mathrm{\ given\ } \ldots]}.
\tabular{rcl}{
\bold{Probability} \tab \bold{and} \tab \bold{Symbol Convention}\cr
\eqn{\mathrm{Pr}[\,U \le u, V \le v\,]}     \tab \eqn{=} \tab \eqn{\mathbf{C}(u,v)}\cr
\eqn{\mathrm{Pr}[\,U > u, V > v\,]}         \tab \eqn{=} \tab \eqn{\hat\mathbf{C}(u',v')}\cr
\eqn{\mathrm{Pr}[\,U \le u, V > v\,]}       \tab \eqn{=} \tab \eqn{u - \mathbf{C}(u,v')}\cr
\eqn{\mathrm{Pr}[\,U > u, V \le v\,]}       \tab \eqn{=} \tab \eqn{v - \mathbf{C}(u',v)}\cr
\eqn{\mathrm{Pr}[\,U \le u \mid V \le v\,]} \tab \eqn{=} \tab \eqn{\mathbf{C}(u,v)/v}\cr
\eqn{\mathrm{Pr}[\,V \le v \mid U \le u\,]} \tab \eqn{=} \tab \eqn{\mathbf{C}(u,v)/u}\cr
\eqn{\mathrm{Pr}[\,U \le u \mid V > v\,]}   \tab \eqn{=} \tab \eqn{\bigl(u - \mathbf{C}(u,v)\bigr)/(1 - v)}\cr
\eqn{\mathrm{Pr}[\,V \le v \mid U > u\,]}   \tab \eqn{=} \tab \eqn{\bigl(v - \mathbf{C}(u,v)\bigr)/(1 - u)}\cr
\eqn{\mathrm{Pr}[\,U > u \mid V > v\,]}   \tab \eqn{=} \tab \eqn{\hat\mathbf{C}(u',v')/u' = \overline\mathbf{C}(u,v)/(1-u)}\cr
\eqn{\mathrm{Pr}[\,V > v \mid U > u\,]}   \tab \eqn{=} \tab \eqn{\hat\mathbf{C}(u',v')/v' = \overline\mathbf{C}(u,v)/(1-v)}\cr
\eqn{\mathrm{Pr}[\,V \le v \mid U = u\,]}   \tab \eqn{=} \tab \eqn{\delta \mathbf{C}(u,v)/\delta u}\cr
\eqn{\mathrm{Pr}[\,U \le u \mid V = v\,]}   \tab \eqn{=} \tab \eqn{\delta \mathbf{C}(u,v)/\delta v}\cr
\eqn{\mathrm{Pr}[\,U > u \mathrm{\ or\ } V > v\,]}     \tab \eqn{=} \tab \eqn{\mathbf{C}^\star(u',v') = 1 - \mathbf{C}(u',v')}\cr
\eqn{\mathrm{Pr}[\,U \le u \mathrm{\ or\ } V \le v\,]} \tab \eqn{=} \tab \eqn{\tilde\mathbf{C}(u,v) = u + v - \mathbf{C}(u,v)}\cr
}
The function \code{\link{jointCOP}} has considerable demonstration in its \bold{Note} section of the \bold{joint and} and \bold{joint or} relations shown through simulation and counting scenarios. Also there is a demonstration in the \bold{Note} section of function \code{\link{duCOP}} on application of the concepts of \bold{joint and} conditions, \bold{joint or} conditions, and importantly joint \bold{mutually exclusive or} conditions.

One, two, or more copulas can be \dQuote{composited,} \dQuote{combined,} or \dQuote{multiplied} in interesting ways to create highly unique bivariate relationsm and as a result, complex dependence structures can be formed. \pkg{copBasic} provides three main functions for copula composition: \code{\link{composite1COP}} composites a single copula with two compositing parameters, \code{\link{composite2COP}} composites two copulas with two compositing parameters, and \code{\link{composite3COP}} composites two copulas with four compositing parameters. Also two copulas can be combined through a weighted convex combination using \code{\link{convex2COP}} with a single weighting parameter, and even \eqn{N} number of copulas can be combined by weights using \code{\link{convexCOP}}. Finally, multiplication of two copulas to form a third is supported by \code{\link{prod2COP}}. All six functions for compositing, combining, or multipling copulas are compatible with joint probability simulation (\code{\link{simCOP}}), measures of association (\emph{e.g.} \eqn{\rho_\mathbf{C}}), and presumably all other copula operations using \pkg{copBasic} features.

\tabular{cclr}{
\bold{No. of copulas} \tab \bold{Combining Parameters} \tab \bold{Function} \tab \bold{Concept}\cr
1 \tab \eqn{\alpha, \beta} \tab \code{\link{composite1COP}} \tab copula combination\cr
2 \tab \eqn{\alpha, \beta} \tab \code{\link{composite2COP}} \tab copula combination\cr
2 \tab \eqn{\alpha, \beta, \kappa, \gamma} \tab \code{\link{composite3COP}} \tab copula combination\cr
2 \tab \eqn{\alpha} \tab \code{\link{convex2COP}} \tab copula combination\cr
\eqn{N} \tab \eqn{\omega_{i \in N}} \tab \code{\link{convexCOP}} \tab copula combination\cr
2 \tab \eqn{\bigl(\mathbf{C}_1 \ast \mathbf{C}_2 \bigr)} \tab \code{\link{prod2COP}} \tab copula multiplication\cr
}

\bold{Useful Copula Relations by Visualization}

There are a myriad of relations amongst variables computable through copulas, and these were listed in the \bold{Table of Probabilities} earlier in this \pkg{copBasic} documentation. There is a script located in the \code{inst/doc} directory of the \pkg{copBasic} sources titled \code{CopulaRelations_BaseFigure_inR.txt}. This script demonstrates, using the \code{\link{PSP}} copula, relations between the copula (\code{\link{COP}}), survival copula (\code{\link{surCOP}}), joint survival function of a copula (\code{\link{surfuncCOP}}), co-copula (\code{\link{coCOP}}), and dual of a copula function (\code{\link{duCOP}}). The script performs simulation and manual counts observations meeting various criteria in order to compute their \emph{empirical probabilities}. The script produces a \dQuote{base figure,} which after extending in vector editing software, is suitable for educational description, and is provided within these help pages.

\bold{A Review of \dQuote{Return Periods} using Copulas}

Risk analyses of natural hazards are commonly expressed as \emph{annual return periods} \eqn{T} in years, which are defined for a nonexceedance probability \eqn{q} as \eqn{T = 1/(1-q)}. In bivariate analysis, there immediately emerge two types of return periods representing \eqn{T_{q;\,\mathrm{coop}}} and \eqn{T_{q;\,\mathrm{dual}}} conditions between nonexceedances of the two hazard sources (random variables) \eqn{U} and \eqn{V}. It is usual in many applications for \eqn{T} to be expressed equivalently as a probability \eqn{q} in common for both variables.

Incidently, the \eqn{\mathrm{Pr}[\,U > u \mid V > v\,]} and \eqn{\mathrm{Pr}[\,V > v \mid U > u\,]} probabilities also are useful for \emph{conditional return period} computations following Salvadori \emph{et al.} (2007, p. 159--160) but are not further considered here. Also the \eqn{F_K(w)} (\emph{Kendall Function} or \emph{Kendall Measure} of a copula) is the core tool for \emph{secondary return period} computations (see \code{\link{kfuncCOP}}).


Let the copula \eqn{\mathbf{C}(u,v; \Theta)} for nonexceedances \eqn{u} and \eqn{v} be set for some copula family (formula) by a parameter vector \eqn{\Theta}. The copula family and parameters define the joint coupling (loosely meant the dependency/correlation) between hazards \eqn{U} and \eqn{V}. If \dQuote{failure} occurs if \bold{either} or \bold{both} hazards \eqn{U} and \eqn{V} are a probability \eqn{q} threshold (\eqn{u = v = 1 - 1/T = q}) for \eqn{T}-year return period, then the \bold{real return period} of failure is defined using either the copula (\eqn{\mathbf{C}(q,q; \Theta)} or the \emph{co-copula} (\eqn{\mathbf{C}^\star(q',q'; \Theta)}) for exceedance probability \eqn{q' = 1 - q} is

\deqn{T_{q;\,\mathrm{coop}} = \frac{1}{1 - \mathbf{C}(q, q; \Theta)} = \frac{1}{\mathbf{C}^\star(1-q, 1-q; \Theta)}\mbox{\ and}}
\deqn{T_{q;\,\mathrm{coop}} \equiv \frac{1}{\mathrm{cooperative\ risk}}\mbox{.}}

Or in words, the hazard sources \bold{collaborate} or \bold{cooperate} to cause failure. However, if failure only occurs if and only if \bold{both} hazards \eqn{U} and \eqn{V} occur simultaneously (that is the hazards must \dQuote{dually work together}), then the \bold{real return period} is defined using either the \emph{dual of a copula (function)} (\eqn{\tilde\mathbf{C}(q,q; \Theta)}), the \emph{joint survival function} (\eqn{\overline\mathbf{C}(q,q;\Theta)}), or \emph{survival copula} (\eqn{\hat\mathbf{C}(q',q'; \Theta)}) as

\deqn{T_{q;\,\mathrm{dual}} = \frac{1}{1 - \tilde\mathbf{C}(q,q; \Theta)} = \frac{1}{\overline\mathbf{C}(q,q;\Theta)} = \frac{1}{\hat\mathbf{C}(q',q';\Theta)} \mbox{\ and}}
\deqn{T_{q;\,\mathrm{dual}} \equiv \frac{1}{\mathrm{complement\ of\ dual\ protection}}\mbox{.}}


A numerical demonstration is informative. Salvadori \emph{et al.} (2007, p. 151) show for a \emph{Gumbel--Hougaard copula} (\code{\link{GHcop}}) having \eqn{\Theta =} 3.055 and \eqn{T =} 1,000 years (\eqn{q = 0.999}) that \eqn{T_{q;\,\mathrm{coop}} = 797.1} years and that \eqn{T_{q;\,\mathrm{dual}}} = 1,341.4 years, which means that average return periods between and \dQuote{failures} are
\deqn{T_{q;\,\mathrm{coop}} \le T \le T_{q;\,\mathrm{dual}}\mbox{\ and thus}}
\deqn{797.1 \le T \le 1314.4\mbox{\ years.}}
With the following code, these values are readily computed and verified using the \code{prob2T()} function from the \pkg{lmomco} package along with \pkg{copBasic} functions \code{\link{COP}} (generic functional interface to a copula) and \code{\link{duCOP}} (\emph{dual of a copula}):
\preformatted{
  q <- T2prob(1000)
  lmomco::prob2T(  COP(q,q, cop=GHcop, para=3.055)) #  797.110
  lmomco::prob2T(duCOP(q,q, cop=GHcop, para=3.055)) # 1341.438
}
An early source (in 2005) by some of those authors cited on p. 151 of Salvadori \emph{et al.} (2007; their citation \dQuote{[67]}) shows \eqn{T_{q;\,\mathrm{dual}} = 798} years---a rounding error seems to have been committed. Finally just for reference, a Gumbel--Hougaard copula having \eqn{\Theta = 3.055} corresponds to an analytical \emph{Kendall's Tau} (see \code{\link{GHcop}}) of \eqn{\tau \approx 0.673}, which can be verified through numerical integration available from \code{\link{tauCOP}} as:
\preformatted{
  tauCOP(cop=GHcop, para=3.055, brute=TRUE) # 0.6726542
}
Thus, a \dQuote{better understanding of the statistical characteristics of [multiple hazard sources] requires the study of their joint distribution} (Salvadori \emph{et al.}, 2007, p. 150).


\bold{Interaction of copBasic to Copulas in Other Packages}

As mentioned elsewhere, \pkg{copBasic} was not originally intended to be a port of the numerous bivariate copulas or over re-implementation other bivariate copulas available in \R. (Though as the package passes its 10th year in 2018, this has gradually changed.) It is useful to point out a demonstration showing an implemention of the \emph{Gaussian copula} from the \pkg{copula} package, which is shown in the \bold{Note} section of \code{\link{med.regressCOP}} in a circumstance of ordinary least squares linear regression  compared to \emph{median regression} of a copula as well as prediction limits of both regressions. Another demonstration in context of \emph{maximum pseudo-log-likelihood estimation} of copula parameters is seen in the \bold{Note} section \code{\link{mleCOP}}, and also see \dQuote{\bold{API to the copula package}} or \dQuote{\bold{package copula (comparison to)}} entries in the Index.

\if{html}{\figure{CopulaRelationsFigure4pkg.jpg}}
\if{latex}{\figure{CopulaRelationsFigure4pkg.pdf}{options: width=4in, trim=1in 2.25in 0.75in 1in, clip}}


}
\references{
Cherubini, U., Luciano, E., and Vecchiato, W., 2004, Copula methods in finance: Hoboken, NJ, Wiley, 293 p.

\enc{Hernández-Maldonado}{Hernandez-Maldonado}, V., \enc{Díaz-Viera}{Diaz-Viera}, M., and Erdely, A., 2012, A joint stochastic simulation method using the Bernstein copula as a flexible tool for modeling nonlinear dependence structures between petrophysical properties: Journal of Petroleum Science and Engineering, v. 90--91, pp. 112--123.

Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in nature---An approach using copulas: Dordrecht, Netherlands, Springer, Water Science and Technology Library 56, 292 p.
}
\author{William Asquith \email{william.asquith@ttu.edu} }
\examples{
\dontrun{
# Nelsen (2006, p. 75, exer. 3.15b) provides for a nice test of copBasic features.
"mcdurv" <- function(u,v, theta) {
   ifelse(u > theta & u < 1-theta & v > theta & v < 1 - theta,
             return(M(u,v) - theta), # Upper bounds copula with a shift
             return(W(u,v)))         # Lower bounds copula
}
"MCDURV" <- function(u,v, para=NULL) {
   if(is.null(para))         stop("need theta")
   if(para < 0 | para > 0.5) stop("theta ! in [0,1/2]")
   return(asCOP(u, v, f=mcdurv, para))
}
"afunc" <- function(t) { # a sample size = 1,000 hard wired
   return(cov(simCOP(n=1000, cop=MCDURV, para=t, ploton=FALSE, points=FALSE))[1,2])
}
set.seed(6234) # setup covariance based on parameter "t" and the "root" parameter
print(uniroot(afunc, c(0,0.5))) # "t" by simulation = 0.1023742
# Nelsen reports that if theta appox. 0.103 then covariance of U and V is zero.
# So one will have mutually completely dependent uncorrelated uniform variables!

# Let us check some familiar measures of association:
rhoCOP( cop=MCDURV, para=0.1023742) # Spearman Rho = 0.005854481 (near zero)
tauCOP( cop=MCDURV, para=0.1023742) # Kendall Tau  = 0.2648521
wolfCOP(cop=MCDURV, para=0.1023742) # S & W Sigma  = 0.4690174 (less familiar)
D <- simCOP(n=1000, cop=MCDURV, para=0.1023742) # Plot mimics Nelsen (2006, fig. 3.11)
# Lastly, open research problem. L-comoments (matrices) measure high dimension of
# variable comovements (see lmomco package)---"method of L-comoments" for estimation?
lmomco::lcomoms2(simCOP(n=1000, cop=MCDURV, para=0),   nmom=5) # Perfect neg. corr.
lmomco::lcomoms2(simCOP(n=1000, cop=MCDURV, para=0.1023742), nmom=5)
lmomco::lcomoms2(simCOP(n=1000, cop=MCDURV, para=0.5), nmom=5) # Perfect pos. corr.
# T2 (L-correlation), T3 (L-coskew), T4 (L-cokurtosis), and T5 matrices result. For
# Theta = 0 or 0.5 see the matrix symmetry with a sign change for L-coskew and T5 on
# the off diagonals (offdiags). See unities for T2. See near zero for offdiag terms
# in T2 near zero. But then see that T4 offdiags are quite different from those for
# Theta 0.1024 relative to 0 or 0.5. Thus, T4 has captured a unique property of U vs V.}
}
\keyword{multivariate}
\concept{copula theory}
\keyword{L-comoments}
\keyword{Nelsen (2006) Examples and Exercises}
\keyword{return period (conditional)}
\keyword{return period (secondary)}
\keyword{Table of Copulas}
