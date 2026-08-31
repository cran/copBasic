\encoding{utf8}
\name{wolfCOPtest_check}
\alias{wolfCOPtest_check}
\title{Empirical Comparison by Simulation between Independence Test by Schweizer--Wolff Sigma from this package to Independence Test from copula Package}
\description{
Report on a simulation study of empirical performance an \emph{Independence test} based on a \emph{Schweizer--Wolff Sigma} test for \eqn{\sigma_\mathbf{C}} stemming from a Null hypothesis of \eqn{\mathrm{NULL} : \mathbf{C} = \mathbf{\Pi} = uv} (\emph{Independence copula}, \code{\link{P}}), where \eqn{\sigma_\mathbf{C}} is defined and computed under \code{\link{wolfCOP}}. The study uses a \emph{Clayton copula} (\code{\link{CLcop}}) is
\eqn{\mathbf{C}_{\Theta}(u,v)} with parameter \eqn{\Theta} set through \emph{Kendall Tau} (\code{\link{tauCOP}}) by \eqn{\tau = \Theta/(\Theta+2)} and \eqn{\Theta = 2\tau/(1-\tau)}.

The \pkg{copBasic} package provides an \emph{Independence test} with approximated p-values in \code{\link{wolfCOPtest}}, and the \pkg{copula} package provides an \emph{Independence test} by \pkg{copula} function \code{indepTest}. Both tests can detect dependency even if \emph{Kendall Tau} and \emph{Spearman Rho} are equal to zero. Example of comparison between the two tests is in the \bold{Examples} in \code{\link{wolfCOPtest}}.

The \code{\link{wolfCOP}} uses \emph{Schweizer--Wolff Sigma} \eqn{\sigma_\mathbf{C}} of a copula according to Nelsen (2006, p. 209) by

\deqn{\sigma_\mathbf{C} = 12\int\!\!\int_{\mathcal{I}^2} \bigl|\mathbf{C}(u,v) - uv\bigr|\,\mathrm{d}u\mathrm{d}v\mbox{,}}

which is \eqn{0 \le \sigma_\mathbf{C} \le 1}. The \code{\link{wolfCOPtest}} is based on simulation of a sampling estimator of \eqn{\sigma_\mathbf{C}} and detailed parameterization of a three-parameter probability distribution by the \emph{method of L-moments}. The sampling estimator used (refer to \code{\link{wolfCOP}}) was
\deqn{\hat{\sigma}_\mathbf{C} = \frac{12}{n^2 - 1}
        \sum_{i=1}^n\sum_{j=1}^n \bigg|\hat{\mathbf{C}}_n\biggl(\frac{i}{n}, \frac{j}{n}\biggr) -
                 \frac{i}{n}\times\frac{j}{n}\bigg|\mbox{,}}
where \eqn{\hat{\mathbf{C}}_n} is the simplest empirical copula of
\deqn{\hat{\mathbf{C}}_n\biggl(\frac{i}{n}, \frac{j}{n}\biggr) =
	\frac{1}{n}\bigl[\# \mathrm{\ of\ } (U_k \le U_i, V_k \le V_j)\bigr]\mbox{.}}

The \pkg{copula} package in function \code{indepTestSim} is based around the test statistic (as a bivariate form) (Hofert \emph{et al.}, 2017, p. 174) (for such a form refer also to \code{\link{hoefCOP}})

\deqn{S_n^{\mathbf{\Pi}} = \int\!\!\int_{\mathcal{I}^2} n\bigl(\mathbf{C}_n(u,v) - uv\bigr)^2\,\mathrm{d}u\mathrm{d}v\mbox{,}}

where \eqn{\mathbf{C}_n(u,v)} is an empirical copula.

Originating literature leading to \eqn{S_n^{\mathbf{\Pi}}} through citation by Hofert \emph{et al.}, (2017, p. 173--178) are Deheuvels (1981), Genest and R\enc{é}{e}millard (2004), and Genest \emph{et al.} (2007). Genest and R\enc{é}{e}millard (2004) show, repeated in Hofert \emph{et al.} (2017, p. 175), and implemented in \pkg{copula} function \code{indepTest} that approximate p-values can be computed by simulation of size \eqn{N} by

\deqn{\frac{1}{N+1}\biggl(\sum_{k=1}^N \mathbf{1}\bigl(S_n^{\mathbf{\Pi},(k)} \ge S_n^{\mathbf{\Pi}}\bigl) + \frac{1}{2}\biggr)\mbox{,}}

for \eqn{S_n^{\mathbf{\Pi}}} being a large simulation of the \eqn{\Pi} copula. The simulation study described in the next section, used a sample size of \eqn{N = 100{,}000} for the \pkg{copula} package function \code{J <- }\code{indepTestSim(n,}\code{ 2, N=100000)} where \eqn{n} is the sample size of the pending test data. This \code{J} object was used for the \code{indepTest} calls. This is a required step unique to \pkg{copula} package implementation as described by Hofert \emph{et al.} (2017, p. 175) and package documentation. So, the \eqn{N} simulation size in the aforementioned equation is the simulation size on which \code{indepTest} bases its p-values and therefore is unique to needs of the \pkg{copula} package.

\bold{This Simulation Study}---A sequence of \eqn{\tau \in (0,0.02, \cdots, 0.48,0.50)} were converted to parameters \eqn{\Theta} of the \emph{Clayton copula} (\code{\link{CLcop}}). Values for \eqn{\tau} were selected to start the \eqn{\mathbf{C}_{\Theta=0} \equiv \mathbf{\Pi}}, which provides a chance to study results with the condition of the Null being \eqn{\mathbf{\Pi}}. The purpose is to compare for a range of sample sizes (\eqn{n}) the rejection performance of \code{\link{wolfCOPtest}} to the \pkg{copula} function \code{indepTest}. The script is provided in \code{inst/}\code{make_wolfCOPtest/}\code{chck_wolfCOPtestCL.R} of this \pkg{copBasic} package source code.

For each of the \eqn{\tau} values and hence \emph{Clayton copula} parameter \eqn{\Theta} values, a \eqn{\mathbf{\Pi}} sample is drawn for each of sample sequences of sizes \eqn{n \in (5,20;1), (22,50;2), (55,100;5)}. For a given sample, the \code{\link{wolfCOPtest}} and \code{indepTest} were made and p-values compared to the alpha = 0.05 statistical significance level; a binary then is recorded as whether the null hypothesis of independence is rejected or not.

The aforementioned process was repeated for large simulation sizes of \eqn{m = 150{,}000} for all \eqn{\tau} used (total simulation \eqn{\sum m = 6{,}519{,}000}). The differences in simulation size are heuristic to enhance the precision of the results for small \eqn{\tau} values. In the end, for each of the \eqn{\tau} for each of the \eqn{n} for simulation size \eqn{m}, the total number of rejections of the null hypothesis was counted for each method and divided by the number of simulations, which converts the binary into a rejection rate. The rejection rate between methods at the alpha = 0.05 significance level becomes the measure on which to assess performance of \code{\link{wolfCOPtest}} to \code{indepTest}.

The relation between simulated rejection rates for independence at alpha = 0.05 significance level for a selected subset of \eqn{\tau \in (0.00, 0.48; 0.04)} is shown in figure 1. The \eqn{\tau} were subsetted for graphical reasons. The lower left corner as a zoom region demarcated that is shown in figure 2. Each of the two figures as the alpha = 0.05 significance level drawn as a horizontal line.

The figures have the rejection rates for \code{\link{wolfCOPtest}} drawn as solid lines and rejections rates for \code{indepTest} drawn as the dashed lines. The line color ramps with the \eqn{\tau} value and these Kendall Taus are labeled and value plotted between the two curves. The two curve types show that both methods have a similarity of performance. On scrutiny, the curves for \code{\link{wolfCOPtest}} generally plot just above those for the \code{indepTest}. This is an indication of higher rejection rates of the null hypothesis as departure from independence (\eqn{\tau > 0}) seems to increases but only for a range of sample sizes. Figure 1 shows that as sample sizes become large relative to the \eqn{\tau} is a convergence towards 100-percent rejection (a rejection rate \eqn{= 1}).

Figure 2 is a zoom into the lower left corner of figure 1 and is an emphasis of the method performance for very small sample sizes. It appears that \code{indepTest} for \eqn{\tau \le 0.02} barely outperforms \code{\link{wolfCOPtest}}. A general conclusion, however, from the figures is that the \code{\link{wolfCOPtest}} by \pkg{copBasic} is either better tuned empirically than how \code{indepTest} is implemented by \pkg{copula} or that the absolute value operation (difference between empirical copula and \eqn{\mathbf{\Pi} = uv}) in the definition of \eqn{\sigma_\mathbf{C}} in lieu of the square operation (squared difference between empirical copula and \eqn{\mathbf{\Pi} = uv}) in \eqn{S_n^{\mathbf{\Pi}}}.


The mean difference between \code{\link{wolfCOPtest}} and \code{indepTest} rejection is about 1 percent for the \eqn{\tau \in (0,0.02, \cdots, 0.48,0.50)} with \eqn{n \in (5,20;1), (22,50;2), (55,100;5)} totalling about 3.7 million simulations. This is evidence that both methods perform similar to each other, which provides for documentation such as this for \pkg{copBasic} that the \emph{Schweizer--Wolff Sigma} test is well implemented beyond the evidence documented in \bold{Examples} of \code{\link{wolfCOPtest}}.


The results in figures 1 and 2 are compelling. Both figures as exemplified by empirical simulation that \code{\link{wolfCOPtest}} does out perform the independence test of \code{indepTest} as implemented within the \pkg{copula} package. Though the rejection rates of \emph{Kendall Tau} or \emph{Spearman Rho} are not otherwise indicted in those two figures (figs. 1 and 2), the practitioner could study the scripted results of \code{inst/make_wolfCOPtest/}\code{chck_}\code{wolfCOPtestCIRC.R}, which is built around application of the \emph{Circula copula} (\code{\link{CIRCcop}}) to further explore the performance of the \emph{Schweizer--Wolff Sigma} test. Those results for \eqn{m = 150{,}000} are shown in figure 3 and show for that copula (\code{\link{CIRCcop}}) that \code{\link{wolfCOPtest}} continues to outperform \code{indepTest} in terms of rejection at the 0.05 significance level.

Figure 3 also has highlighted at 50\eqn{\times} the computed rejection of independence, the trajectory of the rejection of independence from the Circular copula by well-known statistics of \emph{Kendall tau} and \emph{Spearman Rho}. The trajectories of the simulations for those two statistics require a multiplication of \eqn{50} to avoid changes in vertical scale and plotting of a second vertical axis on the right.

A quite curious aspect is that the rejection rates dip below the alpha = 0.05 for sample sizes \eqn{n \in 6, \cdots, 19} and possibly this is related to algorithmic estimation of nonexceedance probabilities for small samples. Beyond \eqn{n = 20} both methods reject with increasing regularity of independence. By \eqn{n \approx 80} but methods are rejecting independence at 100 percent. However, within the sample range of about \eqn{n \in (20+, \cdots, 80)}, \code{\link{wolfCOPtest}} is superior.

Ultimately, it is not known for all settings or sample size whether or not \code{\link{wolfCOPtest}} will show to be generally more precise at rejection of \emph{Independence} for all sample sizes. This assertion is based entirely on the heuristics of empirical simulation. Evidence is provided, however, in this \pkg{copBasic} documentation that the implementation of a \emph{Schweizer--Wolff Sigma} test appears to have marginally better performance than \pkg{copula} package \code{indepTest}.


\if{html}{\figure{chck_wolfCOPtestCL_plotA.jpg}{options: width=500}}
\if{latex}{\figure{chck_wolfCOPtestCL_plotA.pdf}{options: width=5.25in, trim=0in 0in 0in 0in, clip}}\cr
\bold{Figure 1. Relation between simulated rejection rates for independence at alpha = 0.05 significance level for selected sample sizes 5--100 for a Clayton copula parameterized by given Kendall Tau as indicated}


\if{html}{\figure{chck_wolfCOPtestCL_plotB.jpg}{options: width=500}}
\if{latex}{\figure{chck_wolfCOPtestCL_plotB.pdf}{options: width=5.25in, trim=0in 0in 0in 0in, clip}}\cr
\bold{Figure 2. Relation between simulated rejection rates for independence at alpha = 0.05 significance level for selected sample sizes 5--20 for a Clayton copula parameterized by given Kendall Tau as indicated}\cr


\if{html}{\figure{chck_wolfCOPtestCIRC_plot.jpg}{options: width=500}}
\if{latex}{\figure{chck_wolfCOPtestCIRC_plot.pdf}{options: width=5.25in, trim=0in 0in 0in 0in, clip}}\cr
\bold{Figure 3. Relation between simulated rejection rates for independence at alpha = 0.05 significance level for selected sample sizes 5--100 for a Circular copula comprised of nothing but singular components}
}
\references{
Deheuvels, P., 1981, A non parametric test for independence: Publications de l'Institut de Statistique de l'Universit\enc{é}{e} de Paris, v. 26, no. 2, pp. 29--50. [Also available at \url{https://hal.science/hal-04083801v1}]

Genest, C. and R\enc{é}{e}millard, B., 2004, Tests of independence and randomness based on the empirical copula process: Test, v. 13, pp. 335--369, \doi{10.1007/BF02595777}.

Genest, C., Quessy, J.-F., and R\enc{é}{e}millard, B., 2007, Asymptotic local efficiency of Cram\enc{é}{e}r--von Mises tests for multivariate independence: The Annals of Statistics, v. 35, pp. 166--191, \doi{10.1214/009053606000000984}.

Hofert, M., Kojadinovic, I., M\enc{ä}{a}chler, M., and Yan, J., 2017, Elements of copula modeling with R: Cham, Switzerland, Springer, 267 p., \doi{10.1007/978-3-319-89635-9}.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{wolfCOPtest}}, \code{\link{CLcop}}
}
\keyword{copula (hypothesis test)}
\keyword{Schweizer and Wolff Sigma}
\keyword{Schweizer and Wolff Sigma (hypothesis test)}
\concept{Hypothesis test for Schweizer and Wolff Sigma}
\keyword{independence test}
\keyword{Package copula}
