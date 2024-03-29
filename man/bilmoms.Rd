\encoding{utf8}
\name{bilmoms}
\alias{bilmoms}
\title{Bivariate L-moments and L-comoments of a Copula}
\description{
\bold{Attention:} This function is deprecated in favor of \code{\link{lcomCOP}}, which uses only direct numerical \code{integrate()} on the integrals shown below. The \code{bilmoms} function is strictly based on Monte Carlo integration.

Compute the \emph{bivariate L-moments (ratios)} (\eqn{\delta^{[\ldots]}_{k;\mathbf{C}}}) of a copula \eqn{\mathbf{C}(u,v; \Theta)} and remap these into the \emph{L-comoment} matrix counterparts (Serfling and Xiao, 2007; Asquith, 2011) including \emph{L-correlation} (\emph{Spearman Rho}), \emph{L-coskew}, and \emph{L-cokurtosis}.  As described by Brahimi \emph{et al.} (2015), the first four bivariate L-moments \eqn{\delta^{[12]}_k} for random variable \eqn{X^{(1)}} or \eqn{U} with respect to (\emph{wrt}) random variable \eqn{X^{(2)}} or \eqn{V} are defined as
\deqn{\delta^{[12]}_{1;\mathbf{C}} = 2\int\!\!\int_{\mathcal{I}^2}
\mathbf{C}(u,v)\,\mathrm{d}u\mathrm{d}v - \frac{1}{2}\mbox{,}}
\deqn{\delta^{[12]}_{2;\mathbf{C}} = \int\!\!\int_{\mathcal{I}^2}
(12v - 6)
\mathbf{C}(u,v)\,\mathrm{d}u\mathrm{d}v - \frac{1}{2}\mbox{,}}
\deqn{\delta^{[12]}_{3;\mathbf{C}} = \int\!\!\int_{\mathcal{I}^2}
(60v^2 - 60v + 12)
\mathbf{C}(u,v)\,\mathrm{d}u\mathrm{d}v - \frac{1}{2}\mbox{, and}}
\deqn{\delta^{[12]}_{4;\mathbf{C}} = \int\!\!\int_{\mathcal{I}^2}
(280v^3 - 420v^2 + 180v - 20)
\mathbf{C}(u,v)\,\mathrm{d}u\mathrm{d}v - \frac{1}{2}\mbox{,}}
where the bivariate L-moments are related to the L-comoment ratios by
\deqn{6\delta^{[12]}_k = \tau^{[12]}_{k+1}\mbox{\quad and \quad}6\delta^{[21]}_k = \tau^{[21]}_{k+1}\mbox{,}}
where in otherwords, \dQuote{the third bivariate L-moment \eqn{\delta^{[12]}_3} is one sixth the L-cokurtosis \eqn{\tau^{[12]}_4}.} The first four bivariate L-moments yield the first five L-comoments (there is no first order L-comoment ratio). The terms and nomenclature are not easy and also the English grammar adjective \dQuote{ratios} is not always consistent in the literature. The \eqn{\delta^{[\ldots]}_{k;\mathbf{C}}} are \bold{ratios}, and the returned \code{bilcomoms} element by this function holds matrices for the marginal means, marginal L-scales and L-coscales, and then the \bold{ratio} L-comoments.

Similarly, the \eqn{\delta^{[21]}_k} are computed by switching \eqn{u \rightarrow v} in the polynomials within the above integrals multiplied to the copula in the system of equations with \eqn{u}. In general, \eqn{\delta^{[12]}_k \not= \delta^{[21]}_k} for \eqn{k > 1} unless in the case of \emph{permutation symmetric} (\code{\link{isCOP.permsym}}) copulas. By theory, \eqn{\delta^{[12]}_1 = \delta^{[21]}_1 = \rho_\mathbf{C}/6} where \eqn{\rho_\mathbf{C}} is a \emph{Spearman Rho} \code{\link{rhoCOP}}.

The integral for \eqn{\delta^{[12]}_{4;\mathbf{C}}} does not appear in Brahimi \emph{et al.} (2015) but this and the other forms are verified in the \bold{Examples} and discussion in \bold{Note}. The four \eqn{k \in [1,2,3,4]} for \eqn{U} \emph{wrt} \eqn{V} and \eqn{V} \emph{wrt} \eqn{U} comprise a full spectrum of system of seven (not eight) equations. One equation is lost because \eqn{\delta^{[12]}_1 = \delta^{[21]}_1}.
}
\usage{
bilmoms(cop=NULL, para=NULL, only.bilmoms=FALSE, n=1E5,
                  sobol=TRUE, scrambling=0, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{only.bilmoms}{A logical to trigger return of the \eqn{\delta_k} and skip L-comoment computation;}
  \item{n}{The Monte Carlo integration size. The default seems to be at least an order of magnitude greater than needed for many applied problems;}
  \item{sobol}{A logical trigging \emph{Sobol sequences} for the Monte Carlo integration instead of the bivariate uniform distribution (independence). The Sobol sequences are dependent on the \code{sobol()} function of the \pkg{randtoolbox} package, and the Sobol sequences canvas the \eqn{\mathcal{I}^2} domain for smaller \eqn{n} values than required if statistical independence is used for the Monte Carlo integration. Note, \pkg{randtoolbox} at least at version 2.0.+ has \dQuote{scrambling} of Sobol sequences temporarily disabled, and hence \code{scrambling=0} as default for \code{bilmoms};}
  \item{scrambling}{The argument of the same name for \code{randtoolbox::sobol()}; and}
  \item{...}{Additional arguments to pass to the \code{\link{densityCOP}} function.}
}
\value{
  An \R \code{list} of the bivariate L-moments is returned.
  \item{bilmomUV}{The bivariate L-moments \eqn{\delta^{[12]}_k} of \eqn{U} with respect to \eqn{V} for \eqn{k \in [1,2,3,4]};}
  \item{bilmomVU}{The bivariate L-moments \eqn{\delta^{[21]}_k} of \eqn{V} with respect to \eqn{U} for \eqn{k \in [1,2,3,4]};}
  \item{error.rho}{An \dQuote{error} term in units of \eqn{\delta^{[12 \& 21]}_1} used to judge whether the sample size for the Monte Carlo integration is sufficient based on a comparison to the \emph{Spearman Rho} from direct numerical integration (not Monte Carlo based) using \code{\link{rhoCOP}} of the copula. Values for \code{error.rho} \eqn{< 1E{-}5} seem to be sufficient to judge whether \code{n} is large enough;}
  \item{bilcomoms}{If not \code{only.bilmoms}, another \R \code{list} holding the L-comoments (see \bold{Note}) computed by simple remapping of the \eqn{\delta^{[\ldots]}_k} and parallel in structure to the function \code{lcomoms2()} of the \pkg{lmomco} package; and}
  \item{source}{An attribute identifying the computational source of the bivariate L-moments and bivariate L-comoments: \dQuote{bilmoms.}}
}
\note{
The mapping of the bivariate L-moments to their L-comoment matrix counterparts is simple but nuances should be discussed and the meaning of the \code{error.rho} needs further description.  The extra effort to form L-comoment matrices (Serfling and Xiao, 2007; Asquith, 2011) is made so that output matches the structure of the sample L-comoment matrices from the \code{lcomoms2()} function of the \pkg{lmomco} package.

Concerning the triangular or tent-shaped copula of Nelsen (2006, exer. 3.7, pp. 64--65) for demonstration, simulate from the triangular copula a sample of size \eqn{m = 20{,}000} and compute some sample L-comoments using the following CPU intensive code. The function \code{\link{asCOP}} completes the vectorization needed for non-Monte Carlo integration for \code{\link{rhoCOP}}.
\preformatted{
  "trianglecop" <- function(u,v, para=NULL, ...) {
    # If para is set, then the triangle is rotated 90d clockwise.
    if(! is.null(para) && para == 1) { t <- u; u <- v; v <- t }
    if(length(u) > 1 | length(v) > 1) stop("only scalars for this function")
    v2<-v/2; if(0   <= u    & u    <= v2 & v2 <= 1/2) { return(u    )
    } else   if(0   <=   v2 &   v2 <  u  &  u < 1-v2) { return(v2   )
    } else   if(1/2 <= 1-v2 & 1-v2 <= u  &  u <= 1  ) { return(u+v-1)
    } else { stop("should not be here in logic") }
  }
  "TriCop" <- function(u,v, ...) { asCOP(u,v, f=trianglecop, ...) }
  m=20000; SampleUV <- simCOP(n=m, cop=TriCop, graphics=FALSE)
  samLC  <- lcomoms2(SampleUV, nmom=5)
  theoLC <- bilmoms(cop=TriCop)
}

\emph{The Error in Rho Computation}---The \eqn{\rho_\mathbf{C}} of the copula by numerical integration is computed internally to \code{bilmoms} as
\preformatted{
  rhoC <- rhoCOP(cop=TriCop) # -1.733858e-17
}
and used to compute the \code{error.rho} for \code{bilmoms} (see next code snippet). The  \eqn{\rho_\mathbf{C}} is obviously zero for this copula. Therefore, the bivariate association of \code{TriCop} is zero and thus is an example of a perfectly dependent situation yet of zero correlation. The bivariate L-moments and L-comoments of this copula are computed as
\preformatted{
  mean(replicate(20, bilmoms(cop=TriCop)$error.rho)) # 7.650723e-06
}
where the \code{error.rho} is repeated trials appears firmly \eqn{<}\code{1e-5}, which is near zero (\eqn{\epsilon_\rho \approx 0}). The \code{error.rho} term is defined by taking the first bivariate L-moment and numerically integrated \eqn{\rho_\mathbf{C}} through \code{\link{rhoCOP}} and computing the terms
\deqn{\epsilon^{[12]}_\rho = |\delta^{[12]}_{1;\mathbf{C}} - (\rho_\mathbf{C}/6)|\mbox{,}}
\deqn{\epsilon^{[21]}_\rho = |\delta^{[21]}_{1;\mathbf{C}} - (\rho_\mathbf{C}/6)|\mbox{, and}}
\deqn{\epsilon_\rho = \frac{\epsilon^{[12]}_\rho  + \epsilon^{[21]}_\rho}{2}\mbox{,}}
where the \code{error.rho} \eqn{=} \eqn{\epsilon_\rho}, and values near zero are obviously favorable because this indicates that the Monte Carlo integration sample size \eqn{n} argument is sufficiently large to effectively canvas the \eqn{\mathcal{I}^2} domain. For the situation here, the theoretical \eqn{\rho_\mathbf{C} = 0}, but for \code{n} \eqn{= n = 100}, the \code{error.rho} \eqn{\approx} 0.006 (\emph{e.g.} \code{bilmoms(n=100,} \code{cop=TriCop)$error.rho}) through a 20-unit replication, which is a hint that 100 samples are not large enough and that should be obvious.

The reasoning behind using the \code{error.rho} between conventional numerical integration and the Monte Carlo integration (\code{error.rho}) is that \eqn{\rho_\mathbf{C}} is symmetrical. This choice of \dQuote{convergence} assessment reduces somewhat the sample size needed for Monte Carlo integration into single number representing error.

\emph{Discussion of Theoretical L-comoments}---The theoretical L-comoments in the format structure of the sample L-comoments by the \code{lcomoms2()} function of the \pkg{lmomco} package are formed by the \code{bilmoms} function, and the theoretical values are shown below in sequence with details listed by L-comoment. Now we extract the L-comoment matrices and show the first L-comoment matrix (the matrix of the means):
\preformatted{
  theoLClcm <- theoLC$bilcomoms
  print(theoLClcm$L1)
            [,1]      [,2]
  [1,] 0.4999939        NA
  [2,]        NA 0.5000032
}
where the diagonal should be filled with 1/2, if the \eqn{n} is suitably large, because 1/2 is the mean of the marginal uniform random variables. By definition the secondary diagonal has \code{NA}s. The values shown above are extremely close supporting the idea that default \eqn{n} is large enough. The matrix of means is otherwise uninformative.

The second L-comoment matrix (L-scales and L-coscales) is
\preformatted{
  print(theoLClcm$L2)
                [,1]          [,2]
  [1,]  1.666677e-01 -3.466202e-06
  [2,] -3.466209e-06  1.666674e-01
}
where the diagonal should be filled with 1/6, if the \eqn{n} is suitably large, because 1/6 is the univariate L-scale of the marginal uniform random variables. These values further support that default \eqn{n} is large enough. The diagonal is computed from the univariate L-moments of the margins of the Monte Carlo-generated edges and is otherwise uninformative. The secondary diagonal is a rescaling of the \eqn{\delta^{[\ldots]}_1} by the univariate L-moments of the margins to form L-coscales (nonratios). The copula is perfectly dependent but uncorrelated; so the secondary diagonal has near zeros.

The second L-comoment ratio matrix (coefficient of L-variations and L-correlations) is
\preformatted{
  print(theoLClcm$T2)
                [,1]          [,2]
  [1,]  1.000000e+00 -2.079712e-05
  [2,] -2.079712e-05  1.000000e+00
}
where the diagonal by definition has unities (correlation is unity for a variable on itself) but the secondary diagonal for the L-correlations has near zeros because again the copula is uncorrelated, and the secondary diagonal is computed from the \eqn{\delta^{[\ldots]}_1}. These L-correlations are the \emph{Spearman Rho} values computed external to the algorithms within \code{\link{rhoCOP}}.

The third L-comoment ratio matrix (L-skews and L-coskews) is
\preformatted{
  print(theoLClcm$T3)
                [,1]          [,2]
  [1,]  3.021969e-06 -2.829783e-05
  [2,] -7.501135e-01  4.518901e-06
}
where the diagonal by definition should have nero zeros because the univariate L-skew of a uniform variable is zero. These values further support that default \eqn{n} is large enough.  The secondary diagonal holds L-coskews. The copula has L-coskew of \eqn{U} \emph{wrt} \eqn{V} of numerically near zero (symmetry) but measurable asymmetry of L-coskew of \eqn{V} \emph{wrt} \eqn{U} of \eqn{\tau^{[21]}_3 \approx -0.75}.

The fourth L-comoment ratio matrix (L-kurtosises and L-cokurtosises) is
\preformatted{
  print(theoLClcm$T4)
                [,1]          [,2]
  [1,] -2.623665e-06 -3.325177e-05
  [2,] -2.162954e-04 -2.811630e-06
}
where the diagonal by definition should have nero zeros because the univariate L-kurtosis of a uniform variable is zero---it has no peakedness. These values further support that default \eqn{n} is large enough. The secondary diagonal holds L-cokurtosises and are near zero for this particular copula.

The fifth L-comoment ratio matrix (unnamed) is
\preformatted{
  print(theoLClcm$T5)
               [,1]          [,2]
  [1,] 1.813344e-06 -1.296025e-04
  [2,] 1.246436e-01  1.475012e-06
}
where the diagonal by definition should have nero zeros because the univariate L-kurtosis of a uniform variable is zero---such a random variable has no asymmetry. These values further support that default \eqn{n} is large enough. The secondary diagonal holds the fifth L-comoment ratios.  The copula has \eqn{\tau^{[12]}_5 = 0} of \eqn{U} \emph{wrt} \eqn{V} of numerically near zero (symmetry) but measurable fifth-order L-comoment asymmetry of \eqn{V} \emph{wrt} \eqn{U} of \eqn{\tau^{[21]}_5 \approx 0.125}.

\emph{Comparison of Sample and Theoretical L-comoments}---The previous section shows theoretical values computed as \eqn{\tau^{[21]}_3 \approx -0.75} and \eqn{\tau^{[21]}_5 \approx 0.125} for the two L-comoments substantially away from zero. As sample L-comoments these values are \code{samLC$T3[2,1]} \eqn{=} \eqn{\hat\tau^{[21]}_3 \approx -0.751} and \code{samLC$T5[2,1]} \eqn{=} \eqn{\hat\tau^{[21]}_5 \approx 0.123}. CONCLUSION: \emph{The sample L-comoment algorithms in the \pkg{lmomco} package are validated.}
}
\references{
Asquith, W.H., 2011, Distributional analysis with L-moment statistics using the R environment for statistical computing: Createspace Independent Publishing Platform, ISBN 978--146350841--8.

Brahimi, B., Chebana, F., and Necir, A., 2015, Copula representation of bivariate L-moments---A new estimation method for multiparameter two-dimensional copula models: Statistics, v. 49, no. 3, pp. 497--521.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Serfling, R., and Xiao, P., 2007, A contribution to multivariate L-moments---L-comoment matrices: Journal of Multivariate Analysis, v. 98, pp. 1765--1781.
}
\author{W.H. Asquith}
\seealso{\code{\link{lcomCOP}}, \code{\link{uvlmoms}}
}
\examples{
\dontrun{
bilmoms(cop=PSP, n=10000, para=NULL, sobol=TRUE)$bilcomoms$T3
# results: Tau3[12]=-0.132, Tau3[21]=-0.132 (Monte Carlo)
lcomCOP(cop=PSP, para=NULL, orders=3)
# results: Tau3[12]=-0.129, Tau3[21]=-0.129 (direct integration)}

\dontrun{
# This stopped running sometime before June 2023. IS THIS IN COP()?
para <- list(alpha=0.5, beta=0.93, para1=4.5, cop1=GLcop, cop2=PSP)
bilmoms(cop=composite2COP, n=10000, para=para, sobol=TRUE)$bilcomoms$T3
# results: Tau3[12]=0.154, Tau3[21]=-0.0691 (Monte Carlo)
lcomCOP(cop=composite2COP, para=para, orders=3)
# results: Tau3[12]=0.156, Tau3[21]=-0.0668 (direct integration)}

\dontrun{
UVsim <- simCOP(n=20000, cop=composite2COP, para=para, graphics=FALSE)
samLcom <- lmomco::lcomoms2(UVsim, nmom=5) # sample algorithm
# results: Tau3[12]=0.1489, Tau3[21]=-0.0679 (simulation)}
}
\keyword{bivariate asymmetry (measures)}
\keyword{bivariate skewness}
\keyword{bivariate L-moments}
\keyword{Nelsen (2006) Examples and Exercises}
\keyword{Sobol sequence (Monte Carlo integration)}
\keyword{L-comoments}
\keyword{Spearman Rho}
\keyword{Package randtoolbox}
\concept{bivariate L-correlation}
\concept{bivariate L-skew}
\concept{bivariate L-kurtosis}
\concept{copula L-comoments}
\concept{copula L-correlation}
\concept{copula L-coskew}
\concept{copula L-cokurtosis}
\concept{Lcomoments}
