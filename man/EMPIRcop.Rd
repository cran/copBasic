\encoding{utf8}
\name{EMPIRcop}
\alias{EMPIRcop}
\title{ The Bivariate Empirical Copula }
\description{
The \emph{bivariate empirical copula} (Nelsen, 2006, p. 219) for a bivariate sample of length \eqn{n} is defined for random variables \eqn{X} and \eqn{Y} as

\deqn{\mathbf{C}_n\biggl(\frac{i}{n}, \frac{j}{n}\biggr) = \frac{\mathrm{number\ of\ pairs\ (}x,y\mathrm{)\ with\ }x \le x_{(i)}\mathrm{\ and\ }y \le y_{(j)}}{n}\mbox{,}}

where \eqn{x_{(i)}} and \eqn{y_{(i)}}, \eqn{1 \le i,j \le n} or expressed as
\deqn{\mathbf{C}_n\biggl(\frac{i}{n}, \frac{j}{n}\biggr) =
     \frac{1}{n}\sum_{i=1}^n \mathbf{1}\biggl(\frac{R_i}{n} \le u_i, \frac{S_i}{n} \le v_i \biggr)\mbox{,}}
where \eqn{R_i} and \eqn{S_i} are ranks of the data for \eqn{U} and \eqn{V}, and \eqn{\mathbf{1}(.)} is an \emph{indicator function} that score 1 if condition is true otherwise scoring zero. Using more generic notation, the empirical copula can be defined by
\deqn{\mathbf{C}_{n}(u,v) =
     \frac{1}{n}\sum_{i=1}^n \mathbf{1}\bigl(u^\mathrm{obs}_{i} \le u_i, v^\mathrm{obs}_{i} \le v_i \bigr)\mbox{,}}
where \eqn{u^\mathrm{obs}} and \eqn{v^\mathrm{obs}} are thus some type of nonparametric nonexceedance probabilities based on counts of the underlying data expressed in probabilities.


\emph{Hazen Empirical Copula}---The \dQuote{Hazen form} of the empirical copula is
\deqn{\mathbf{C}^\mathcal{H}_{n}(u,v) =
     \frac{1}{n}\sum_{i=1}^n \mathbf{1}\biggl(\frac{R_i - 0.5}{n} \le u_i, \frac{S_i - 0.5}{n} \le v_i \biggr)\mbox{,}}
which can be triggered by \code{ctype="hazen"}. This form is named for this package because of direct similarity of the \emph{Hazen plotting position} to the above definition. Joe (2014, pp. 247--248) uses the Hazen form. Joe continues by saying \dQuote{[the] adjustment of the uniform score [\eqn{(R - 0.5)/n]}] could be done in an alternative form, but there is [asymptotic] equivalence[, and that] \eqn{\mathbf{C}^\mathcal{H}_{n}} puts mass of \eqn{n^{-1}} at the tuples \eqn{([r_{i1} - 0.5]/n, \ldots, [r_{id} - 0.5]/n)} for \eqn{i = 1, \ldots, n}.} A footnote by Joe (2014) says that \dQuote{the conversion [\eqn{R/(n+1)}] is commonly used for the empirical copula.} This later form is the \dQuote{Weibull form} described next. Joe's preference for the Hazen form is so that the sum of squared normal scores is closer to unity for large \eqn{n} than such a sum would be attained using the Weibull form.


\emph{Weibull Empirical Copula}---The \dQuote{Weibull form} of the empirical copula is
\deqn{\mathbf{C}^\mathcal{W}_{n}(u,v) =
     \frac{1}{n}\sum_{i=1}^n \mathbf{1}\biggl(\frac{R_i}{n+1} \le u_i, \frac{S_i}{n+1} \le v_i \biggr)\mbox{,}}
which can be triggered by \code{ctype="weibull"}. This form is named for this package because of direct similarity of the \emph{Weibull plotting position} to the definition, and this form is the default (see argument description).

\emph{Bernstein Empirical Copula}---The empirical copula can be extended nonparametrically as the \emph{Bernstein empirical copula} (\enc{Hernández-Maldonado}{Hernandez-Maldonado}, \enc{Díaz-Viera}{Diaz-Viera}, and Erdely, 2012) and is formulated as

\deqn{\mathbf{C}^\mathcal{B}_n(u,v; \eta) = \sum_{i=1}^n\sum_{j=1}^n \mathbf{C}_{n}\biggl(\frac{i}{n},\frac{j}{n}\biggr) \times \eta(i,j; u,v)\mbox{,}}
where the individual \emph{Bernstein weights} \eqn{\eta(i,j)} for the \eqn{k}th paired value of the \eqn{u} and \eqn{v} vectors are

\deqn{\eta(i,j; u,v) = {n \choose i} u^i (1-u)^{n-i} \times {n \choose j} u^j (1-u)^{n-j}\mbox{.}}

The Bernstein extension, albeit conceptually pure in its shuffling by binomial coefficients and left- and right-tail weightings, is quite CPU intensive as inspection of the equations above indicates a nest of four \code{for()} loops in \R. (The native \R code of \pkg{copBasic} uses the \code{sapply()} function in \R liberally for substantial but not a blazing fast speed increase.) The Bernstein extension results in a smoother surface of the empirical copula and can be triggered by \code{ctype="bernstein"}.

\emph{Checkerboard Empirical Copula}---A simple smoothing to the empirical copula is the checkerboard empirical copula (Segers et al., 2017) that has been adapted from the \pkg{copula} package. It is numerically intensive like the Bernstein and possibly of limited usefulness for large sample sizes. The checkerboard extension can be triggered by \code{ctype="checkerboard"} and is formulated as

\deqn{\mathbf{C}^\sharp_{n}(U) = \frac{1}{n+o} \sum_{i=1}^n\prod_{i=1}^d \mathrm{min}[\mathrm{max}[n U_j - R^{(n)}_{i,j} + 1,0],1]\mathrm{,}}

where \eqn{U} is a \eqn{d=2} column matrix of \eqn{u} and \eqn{v}, \eqn{R} is a rank function, and \eqn{o} is an offset term on \eqn{[0,1]}.


The \emph{empirical copula frequency} can be defined (Nelson, 2006, p. 219) as
\deqn{\mathbf{c}_n(u, v) = \mathbf{C}_n\biggl(\frac{i}{n}, \frac{j}{n}\biggr) -
                           \mathbf{C}_n\biggl(\frac{i-1}{n}, \frac{j}{n}\biggr) -
                           \mathbf{C}_n\biggl(\frac{i}{n}, \frac{j-1}{n}\biggr) +
                           \mathbf{C}_n\biggl(\frac{i-i}{n}, \frac{j-1}{n}\biggr)\mbox{.}}
}
\usage{
EMPIRcop(u, v, para=NULL,
               ctype=c("weibull", "hazen", "1/n", "bernstein", "checkerboard"),
                          bernprogress=FALSE, checkerboard.offset=0, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (single element) of parameters---the U-statistics of the data (see \bold{Examples}). Alternatively, \code{para} can be a \code{list} holding a \code{para} as would be done if it were a vector, but arguments \code{bernstein} and \code{bernprogress} can be optionally included---this feature is provided so that the Bernstein refinement can be controlled within the context of other functions calling \code{EMPIRcop} such as by \code{\link{level.curvesCOP}};}
  \item{ctype}{An alternative means for trigging the definition of \eqn{\mathbf{C}^\mathcal{H}_n} (default), \eqn{\mathbf{C}^\mathcal{W}_n}, \eqn{\mathbf{C}_n}, \eqn{\mathbf{C}^\mathcal{B}_n}, or the checkerboard empirical copula (a type of smooth);}
  \item{bernprogress}{The Bernstein copula extension is CPU intensive(!), so a splash counter is pushed to the console via the \code{message()} function in \R so as to not discourage the user;}
  \item{checkerboard.offset}{A scaling of the ratio \code{sum(....)/(n+offset)} for the checkerboard empirical copula; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned.
}
\note{
Not all theoretical measures of copula dependence (both measures of association and measures of asymmetry), which use numerical integration by the \code{integrate()} function in \R, can be used for all empirical copulas because of \dQuote{divergent} integral errors; however, examples using \emph{Hoeffding's Phi} (\eqn{\Phi_\mathbf{C}}; \code{\link{hoefCOP}}) and shown under \bold{Examples}. Other measures of copula dependence include \code{\link{blomCOP}}, \code{\link{footCOP}}, \code{\link{giniCOP}}, \code{\link{rhoCOP}}, \code{\link{tauCOP}}, \code{\link{wolfCOP}}, \code{\link{joeskewCOP}}, and \code{\link{uvlmoms}}. Each of these measures fortunately has a built-in sample estimator.

It is important to distinquish between a sample estimator and the estimation of the measure using the empirical copula itself via the \code{EMPIRcop} function. The sample estimators (triggered by the \code{as.sample} arguments for the measures) are reasonably fast and numerically preferred over using the empirical copula. Further, the generally slow numerical integrations for the theoretical definitions of these copula measures might have difficulties. Limited testing, however, suggests prevalence of numerical integration not erroring using the Bernstein extension of the empirical copula, which must be a by-product of the enhanced and sufficient smoothness for \R's default numerical integration to succeed. Many of the measures have \code{brute} option for a brute-force numerical integration on a regular grid across the empirical copula---these are slow but should not trigger errors. As a general rule, users should still use the sample estimators instead.
}
\references{
\enc{Hernández-Maldonado}{Hernandez-Maldonado}, V., \enc{Díaz-Viera}{Diaz-Viera}, M., and Erdely, A., 2012, A joint stochastic simulation method using the Bernstein copula as a flexible tool for modeling nonlinear dependence structures between petrophysical properties: Journal of Petroleum Science and Engineering, v. 90--91, pp. 112--123.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.

Segers, J., Sibuya, M., and Tsukahara, H., 2017, The empirical beta copula: Journal of Multivariate Analysis, v. 155, pp. 35--51.
}
\author{ W.H. Asquith }
\seealso{ \code{\link{diagCOP}}, \code{\link{level.curvesCOP}}, \code{\link{simCOP}}  }
\examples{
\dontrun{
set.seed(62)
EMPIRcop(0.321,0.78, para=simCOP(n=90, cop=N4212cop,
                                 para=2.32, graphics=FALSE)) # [1] 0.3222222
N4212cop(0.321,0.78, para=2.32)                              # [1] 0.3201281}

\dontrun{
set.seed(62) # See note below about another seed to try.
psp <- simCOP(n=34, cop=PSP, ploton=FALSE, points=FALSE) * 150
# Pretend psp is real data, the * 150 is to clearly get into an arbitrary unit system.

# The sort=FALSE is critical in the following two calls. Although the Weibull
# plotting positions are chosen, internally EMPIRcop uses ranks, but the model
# here is to imagine one having a sample in native units of the random variables
# and then casting them into probabilities for other purposes.
fakeU <- lmomco::pp(psp[,1], sort=FALSE) # Weibull plotting position i/(n+1)
fakeV <- lmomco::pp(psp[,2], sort=FALSE) # Weibull plotting position i/(n+1)
uv <- data.frame(U=fakeU, V=fakeV); # our U-statistics

# The next four values should be very close if n above were say 1000, but the
# ctype="bernstein"" should not be used if n >> 34 because of inherently long runtime.
PSP(0.4,0.6)              # 0.3157895 (compare to listed values below)

# Two seeds are shown so that the user can see that depending on the distribution
# of the values given by para that different coincidences of which method is
# equivalent to another exist.
# For set.seed(62) --- "hazen" == "weibull" by coincidence
#    "hazen"     --> 0.3529412
#    "weibull"   --> 0.3529412
#    "1/n"       --> 0.3235294
#    "bernstein" --> 0.3228916
# For set.seed(85) --- "1/n" == "hazen" by coincidence
#    "hazen"     --> 0.3529412
#    "weibull"   --> 0.3823529
#    "1/n"       --> 0.3529412
#    "bernstein" --> 0.3440387

# For set.seed(62) --- not all measures of association can be used for all
# empirical copulas because of 'divergent' integral errors, but this is an example
# for Hoeffding's Phi. These computations are CPU intensive, esp Bernstein.
hoefCOP(as.sample=TRUE, para=uv) #  (sample estimator is fast)  # 0.4987755
hoefCOP(cop=EMPIRcop,   para=uv, ctype="hazen")                 # 0.5035348
hoefCOP(cop=EMPIRcop,   para=uv, ctype="weibull")               # 0.4977145
hoefCOP(cop=EMPIRcop,   para=uv, ctype="1/n")                   # 0.4003646
hoefCOP(cop=EMPIRcop,   para=uv, ctype="bernstein")             # 0.4563724
hoefCOP(cop=EMPIRcop,   para=uv, ctype="checkerboard")          # 0.4952427}

# All other example suites shown below are dependent on the pseudo-data in the
# variable uv. It is suggested to not run with a sample size much larger than the
# above n=34 if the Bernstein comparison is intended (wanted) simply because of
# lengthy(!) run times, but the n=34 does provide a solid demonstration how the
# level curves for berstein weights are quite smooth.

\dontrun{
# Now let us construct as many as three sets of level curves to the sample
# resided in the uv sample from above using the PSP copula.
level.curvesCOP(cop=PSP); # TRUE, parametric, fast, BLACK CURVES

# Empirical copulas can consume lots of CPU.
# RED CURVES, if n is too small, uniroot() errors might be triggered and likely
# will be using the sample size of 34 shown above.
level.curvesCOP(cop=EMPIRcop, para=uv, delu=0.03, col=2, ploton=FALSE)

# GREEN CURVES (large CPU committment)
# Bernstein progress is uninformative because level.curvesCOP() has taken over control.
bpara <- list(para=uv, ctype="bernstein", bernprogress=FALSE)
level.curvesCOP(cop=EMPIRcop, para=bpara, delu=0.03, col=3, ploton=FALSE)
# The delu is increased for faster execution but more important,
# notice the greater smoothness of the Bernstein refinement.}

\dontrun{
# Experimental from R Graphics by Murrell (2005, p.112)
"trans3d" <-                         # blackslashes seem needed for the package
function(x,y,z, pmat) {              # for user manual building but bad syntax
  tmat <- cbind(x,y,z,1) \%*\% pmat  # because remember the percent sign is a
  return(tmat[,1:2] / tmat[,4])      # a comment character in LaTeX.
}

the.grid <- EMPIRgrid(para=uv, ctype="checkerboard")
the.diag <- diagCOP(cop=EMPIRcop, para=uv, ploton=FALSE, lines=FALSE)

the.persp <- persp(the.grid$empcop, theta=-25, phi=20,
                   xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")
the.trace <- trans3d(the.diag$t, the.diag$t, the.diag$diagcop, the.persp)
lines(the.trace, lwd=2, col=2) # The diagonal of the copula

# The following could have been used as an alternative to call persp()
the.persp <- persp(x=the.grid$u, y=the.grid$v, z=the.grid$empcop, theta=-25, phi=20,
                   xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")
lines(the.trace, lwd=2, col=2) # The diagonal of the copula #}
}
\keyword{empirical copula}
\keyword{copula (empirical)}
\keyword{copula}
\concept{Bernstein copula}
\concept{Bernstein empirical copula}
\concept{Hazen copula}
\concept{Hazen empirical copula}
\concept{Weibull copula}
\concept{Weibull empirical copula}

