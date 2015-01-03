\encoding{utf8}
\name{copBasic-package}
\docType{package}
\alias{copBasic-package}
\title{Basic Theoretical Copula, Empirical Copula, and Various Utility Functions}
\description{
The \pkg{copBasic} package is heavily oriented around copula theory and mathematical operations of copulas closely following one of the standard texts in the field by Nelsen (2006) and from 2015\eqn{+} increasely Joe (2015) as well. Another good text is by Salvadori \emph{et al.} (2007) and is cited herein, but about half of that excellent book is on univariate applications. The primal objective of \pkg{copBasic} is to provide a basic application programming inferface (API) into numerous results shown by authoritative texts on copulas. It is hoped in part that the package will help new inductees to copulas in their self study or potential course work.

A few comments on notation are needed. A bold math typeface is used to represent a copula such as \eqn{\mathbf{\Pi}} (see \code{\link{P}}) for the independence copula. The syntax \eqn{\mathcal{R}\times\mathcal{R} \equiv \mathcal{R}^2} denotes the orthogonal domain of two real numbers, and \eqn{\mathcal{I}\times\mathcal{I} \equiv \mathcal{I}^2} denotes the orthogonal domain on the unit square of probabilities. Thus, limits of integration \eqn{[0,1]} or \eqn{[0,1]^2} are shown as \eqn{\mathcal{I}} and \eqn{\mathcal{I}^2}, respectively.

The random variables \eqn{X} and \eqn{Y} respectively denote the horizontal and vertical directions in \eqn{\mathcal{R}^2}. Their probabilistic counterparts are uniformly distributed random variables on \eqn{[0,1]}, are respectively denoted as \eqn{U} and \eqn{V}, and necessarily also are the directions in \eqn{\mathcal{I}^2}. Often realizations of these random variables are respectively \eqn{x} and \eqn{y} for \eqn{X} and \eqn{Y} and \eqn{u} and \eqn{v} for \eqn{U} and \eqn{V}.

There is a distinction between nonexceedance probability \eqn{F} and exceedance probability \eqn{1-F}. Both \eqn{u} and \eqn{v} are measures in nonexceedance (cumulative probability). Arguments to many functions herein are \code{u} \eqn{= u} and \code{v} \eqn{= v} and are almost \emph{exclusively nonexceedance} but there are instances for which \code{u} \eqn{= 1 - u = u'} and \code{v} \eqn{= 1 - v = v'}. The \dQuote{prime} notation on the exceedance probabilities differs from the traditional \eqn{\overline{t}} (overbar) notation that is used for survival distributions (e.g. \eqn{\overline{F}(x)}) because of a need to refer to these probabilities from time to time in the Examples sections---and thus nonmathematical typeface (e.g. \samp{v'}).

Italic typeface is used extensively and usually near the opening of function-by-function documentation to identify vocabulary words, such as \emph{survival copula} (see \code{\link{surCOP}}). This syntax tries to mimic and accentuate Roger B. Nelsen's word usage in Nelsen (2006) and to a lesser extent Joe (2015). The italics then is used to highlight this vocabulary in order to draw connections between concepts.\cr

\bold{Helpful Navigation of the copBasic Package}

Some helpful guideposts into the package are listed in the following table:
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
Blomqvist’s Beta \tab \eqn{\beta_\mathbf{C}} \tab \code{\link{blomCOP}} \tab bivariate association\cr
Gini's Gamma \tab \eqn{\gamma_\mathbf{C}} \tab \code{\link{giniCOP}} \tab bivariate association\cr
Hoeffding's Phi \tab \eqn{\phi_\mathbf{C}} \tab \code{\link{hoefCOP}} \tab bivariate association\cr
Lp distance \tab  \eqn{\phi_\mathbf{C} \rightarrow L_p} \tab \code{\link{LpCOP}} \tab bivariate association\cr
Kendall's Tau \tab \eqn{\tau_\mathbf{C}} \tab \code{\link{tauCOP}} \tab bivariate association\cr
Semi-correlations \tab \eqn{\rho_N^{-}(a)} \tab \code{\link{semicorCOP}} \tab bivariate tail association\cr
Semi-correlations \tab \eqn{\rho_N^{+}(a)} \tab \code{\link{semicorCOP}} \tab bivariate tail association\cr
Spearman's Rho \tab \eqn{\rho_\mathbf{C}} \tab \code{\link{rhoCOP}} \tab bivariate association\cr
Schweizer and Wolff's Sigma \tab \eqn{\sigma_\mathbf{C}} \tab \code{\link{wolfCOP}} \tab bivariate association\cr
Lower-bounds copula \tab \eqn{\mathbf{W}(u,v)} \tab \code{\link{W}} \tab copula\cr
Independence copula \tab \eqn{\mathbf{\Pi}(u,v)} \tab \code{\link{P}} \tab copula\cr
Upper-bounds copula \tab \eqn{\mathbf{M}(u,v)} \tab \code{\link{M}} \tab copula\cr
\enc{Fréchet}{Frechet} Family copula \tab \eqn{\mathbf{FF}(u,v)} \tab \code{\link{FRECHETcop}} \tab copula\cr
Gumbel-Hougaard copula \tab \eqn{\mathbf{GH}(u,v)} \tab \code{\link{GHcop}} \tab copula\cr
Plackett copula \tab \eqn{\mathbf{PL}(u,v)} \tab \code{\link{PLACKETTcop}} \tab copula\cr
PSP copula \tab \eqn{\mathbf{PSP}(u,v)} \tab \code{\link{PSP}} \tab copula\cr
Empirical copula \tab \eqn{\mathbf{C}_n(u,v)} \tab \code{\link{EMPIRcop}} \tab copula\cr
Parametric simulation \tab \eqn{{-}{-}} \tab \code{\link{simCOP}} \tab copula simulation\cr
Parametric simulation \tab \eqn{{-}{-}} \tab \code{\link{simCOPmicro}} \tab copula simulation\cr
Empirical simulation \tab \eqn{{-}{-}} \tab \code{\link{EMPIRsim}} \tab copula simulation\cr
Empirical simulation \tab \eqn{{-}{-}} \tab \code{\link{EMPIRsimv}} \tab copula simulation\cr
Parametric copulatic surface \tab \eqn{{-}{-}} \tab \code{\link{gridCOP}} \tab copulatic surface\cr
Empirical copulatic surface \tab \eqn{{-}{-}} \tab \code{\link{EMPIRgrid}} \tab copulatic surface\cr
Density \tab \eqn{c(u,v)} \tab \code{\link{densityCOP}} \tab copula density\cr
Density visualization \tab \eqn{{-}{-}} \tab \code{\link{densityCOPplot}} \tab copula density\cr
}

Many functions in the package make the distinction between \eqn{V} with respect to (\emph{wrt}) \eqn{U} and \eqn{U} \emph{wrt} \eqn{V}, and a guide through the nomenclature involving \emph{wrt} distinctions is listed in the following table:
\tabular{lclr}{
\bold{Name} \tab \bold{Symbol} \tab \bold{Function} \tab \bold{Concept} \cr
Copula inversion \tab \eqn{V} \emph{wrt} \eqn{U} \tab \code{\link{COPinv}} \tab copula operator\cr
Copula inversion \tab \eqn{U} \emph{wrt} \eqn{V} \tab \code{\link{COPinv2}} \tab copula operator\cr
Copula derivative \tab \eqn{\delta \mathbf{C}/\delta u} \tab \code{\link{derCOP}} \tab copula operator\cr
Copula derivative \tab \eqn{\delta \mathbf{C}/\delta v} \tab \code{\link{derCOP2}} \tab copula operator\cr
Copula derivative inversion \tab \eqn{V} \emph{wrt} \eqn{U} \tab \code{\link{derCOPinv}} \tab copula operator\cr
Copula derivative inversion \tab \eqn{U} \emph{wrt} \eqn{V} \tab \code{\link{derCOPinv2}} \tab copula operator\cr
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

The two tables do not include all of the myriad of special functions to support similar operations on empirical copulas. All empirical copula operators and utilites are prepended with \code{EMPIR} in the function name. An additional note concerning package nomenclature is that an appended \code{2} to a function name uniquely indicates
\eqn{U} \emph{wrt} \eqn{V} (e.g. \code{\link{EMPIRgridderinv2}} for an inversion of the partial derivatives \eqn{\delta \mathbf{C}/\delta v} across the grid of the empirical copula).

Some additional functions that compute often salient features or characteristics of a copulas include those listed in the following table:
\tabular{lclr}{
\bold{Name} \tab \bold{Symbol} \tab \bold{Function} \tab \bold{Concept} \cr
Left-tail decreasing \tab \eqn{V} \emph{wrt} \eqn{U}  \tab \code{\link{isCOP.LTD}} \tab bivariate association\cr
Left-tail decreasing \tab \eqn{U} \emph{wrt} \eqn{V}  \tab \code{\link{isCOP.LTD}} \tab bivariate association\cr
Right-tail increasing \tab \eqn{V} \emph{wrt} \eqn{U}  \tab \code{\link{isCOP.RTI}} \tab bivariate association\cr
Right-tail increasing \tab \eqn{U} \emph{wrt} \eqn{V}  \tab \code{\link{isCOP.RTI}} \tab bivariate association\cr
Tail (lower) dependency \tab \eqn{\lambda^L_\mathbf{C}} \tab \code{\link{taildepCOP}} \tab bivariate tail association\cr
Tail (upper) dependency \tab \eqn{\lambda^U_\mathbf{C}} \tab \code{\link{taildepCOP}} \tab bivariate tail association\cr
Tail (lower) order \tab \eqn{\kappa^L_\mathbf{C}} \tab \code{\link{tailordCOP}} \tab bivariate tail association\cr
Tail (upper) order \tab \eqn{\kappa^U_\mathbf{C}} \tab \code{\link{tailordCOP}} \tab bivariate tail association\cr
Negatively quadrant dependency \tab NQD \tab \code{\link{isCOP.PQD}} \tab bivariate association\cr
Positively quadrant dependency \tab PQD \tab \code{\link{isCOP.PQD}} \tab bivariate association\cr
Permutation symmetry \tab \eqn{\mathrm{permsym}} \tab \code{\link{isCOP.permsym}} \tab copula symmetry\cr
Radial symmetry \tab \eqn{\mathrm{radsym}} \tab \code{\link{isCOP.radsym}} \tab copula symmetry\cr
Skewness (Joe, 2015) \tab \eqn{\eta(p; \psi)} \tab \code{\link{uvskewness}} \tab bivariate skewness\cr
Kullback-Leibler divergence \tab \eqn{\mathrm{KL}(f|g)} \tab \code{\link{kullCOP}} \tab bivariate inference\cr
K-L sample size \tab \eqn{n_{fg}} \tab \code{\link{kullCOP}} \tab bivariate inference\cr
Vuong's Procedure \tab \eqn{{-}{-}} \tab \code{\link{vuongCOP}} \tab bivariate inference\cr
L-comoments (samp. distr.) \tab \eqn{{-}{-}} \tab \code{\link{lcomCOPpv}} \tab experimental bivariate inference\cr
}

The following table lists some important relations between various joint probability concepts, the copula, nonexceedance probabilities \eqn{u} and \eqn{v}, and exceedance probabilities \eqn{u'} and \eqn{v'}. A compact summary of these probability relations has obvious usefulness.
\tabular{rcl}{
\bold{Probability} \tab \bold{and} \tab \bold{Symbol Convention}\cr
\eqn{\mathrm{Pr}[\,U \le u, V \le v\,]} \tab \eqn{=} \tab \eqn{\mathbf{C}(u,v)}\cr
\eqn{\mathrm{Pr}[\,U < u, V < v\,]} \tab \eqn{=} \tab \eqn{\hat\mathbf{C}(u',v')}\cr
\eqn{\mathrm{Pr}[\,U \le u, V > v\,]} \tab \eqn{=} \tab \eqn{u - \mathbf{C}(u,v)}\cr
\eqn{\mathrm{Pr}[\,U > u, V \le v\,]} \tab \eqn{=} \tab \eqn{v - \mathbf{C}(u,v)}\cr
\eqn{\mathrm{Pr}[\,U \le u \mid V \le v\,]} \tab \eqn{=} \tab \eqn{\mathbf{C}(u,v)/v}\cr
\eqn{\mathrm{Pr}[\,V \le v \mid U \le u\,]} \tab \eqn{=} \tab \eqn{\mathbf{C}(u,v)/u}\cr
\eqn{\mathrm{Pr}[\,U \le u \mid V > v\,]} \tab \eqn{=} \tab \eqn{(u - \mathbf{C}(u,v))/(1 - v)}\cr
\eqn{\mathrm{Pr}[\,V \le v \mid U > u\,]} \tab \eqn{=} \tab \eqn{(v - \mathbf{C}(u,v))/(1 - u)}\cr
\eqn{\mathrm{Pr}[\,V \le v \mid U = u\,]} \tab \eqn{=} \tab \eqn{\delta \mathbf{C}(u,v)/\delta u}\cr
\eqn{\mathrm{Pr}[\,U \le u \mid V = v\,]} \tab \eqn{=} \tab \eqn{\delta \mathbf{C}(u,v)/\delta v}\cr
\eqn{\mathrm{Pr}[\,U > u \mathrm{\ or\ } V > v\,]} \tab \eqn{=} \tab \eqn{1 - \mathbf{C}(u',v')}\cr
\eqn{\mathrm{Pr}[\,U \le v \mathrm{\ or\ } V \le v\,]} \tab \eqn{=} \tab \eqn{u + v - \mathbf{C}(u,v)}\cr
}

One or two copulas can be blended or \dQuote{composited} in interesting ways to create highly unique joint probability relations. The package provides the following functions for copula composition. And these compositing functions are all compatible with joint probability simulation as supported by \code{\link{simCOP}}. Further functions \code{\link{composite2COP}} and \code{\link{composite3COP}} supporting two copulas each provide for each copula having its own parameter set.
\tabular{ccl}{
\bold{No. of copulas} \tab \bold{Compositing parameters} \tab \bold{Function}\cr
1 \tab \eqn{\alpha, \beta} \tab \code{\link{composite1COP}}\cr
2 \tab \eqn{\alpha, \beta} \tab \code{\link{composite2COP}}\cr
2 \tab \eqn{\alpha, \beta, \kappa, \gamma} \tab \code{\link{composite3COP}}\cr
}
}
\references{
Cherubini, U., Luciano, E., and Vecchiato, W., 2004, Copula methods in finance: Hoboken, NJ, Wiley, 293 p.

\enc{Hernández-Maldonado}{Hernandez-Maldonado}, V., \enc{Díaz-Viera}{Diaz-Viera}, M., and Erdely, A., 2012, A joint stochastic simulation method using the Berstein copula as a flexible tool for modeling nonlinear dependence structures between petrophysical properties: Journal of Petroleum Science and Engineering, v. 90--91, pp. 112--123.

Joe, H., 2015, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

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
set.seed(6234)
print(uniroot(afunc, c(0,0.5))) # result by simulation = 0.1023742
# Nelsen reports that if theta appox. 0.103 then covariance of U and V is zero.
# So one will have mutually completely dependent uncorrelated uniform variables!
rhoCOP(cop=MCDURV,  para=0.1023742) # Spearman Rho = 0.005854481 (near zero)
tauCOP(cop=MCDURV,  para=0.1023742) # Kendall Tau  = 0.2648521
wolfCOP(cop=MCDURV, para=0.1023742) # S & W Sigma  = 0.4690174
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
# Theta 0.1024 relative to 0 or 0.5. Thus, T4 has captured a unique property of U vs V.
}




















}
\keyword{multivariate}
\concept{copula theory}
\keyword{L-comoments}
\keyword{Nelsen (2006) Examples and Exercises}
