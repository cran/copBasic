\encoding{utf8}
\name{kullCOP}
\alias{kullCOP}
\alias{kullCOPint}
\title{ Kullback--Leibler Divergence, Jeffrey's Divergence, and Kullback--Leibler Sample Size }
\description{
Compute the \emph{Kullback--Leibler divergence}, \emph{Jeffrey's divergence}, and \emph{Kullback--Leibler sample size} following Joe (2014, pp. 234--237). Consider two densities \eqn{f = c_1(u,v; \Theta_f)} and \eqn{g = c_2(u,v; \Theta_g)} for two different bivariate copulas \eqn{\mathbf{C}_1(\Theta_1)} and \eqn{\mathbf{C}_2(\Theta_2)} having respective parameters \eqn{\Theta}, then the Kullback--Leibler divergence of \eqn{f} relative to \eqn{g} is
\deqn{\mathrm{KL}(f|g) = \int\!\!\int_{\mathcal{I}^2} g\, \log(g/f)\,\mathrm{d}u\mathrm{d}v\mbox{,}}
and Kullback--Leibler divergence of \eqn{g} relative to \eqn{f} is
\deqn{\mathrm{KL}(g|f) = \int\!\!\int_{\mathcal{I}^2} f\, \log(f/g)\,\mathrm{d}u\mathrm{d}v\mbox{,}}
where the limits of integration \eqn{\mathcal{I}^2} theoretically are closed on \eqn{[0,1]^2} but an open interval \eqn{(0,1)^2} might be needed for numerical integration. Note that in general \eqn{\mathrm{KL}(f|g) \ne \mathrm{KL}(g|f)}. The \eqn{\mathrm{KL}(f|g)} is the expected log-likelihood ratios of \eqn{g} to \eqn{f} when \eqn{g} is the true density (Joe, 2014, p. 234), whereas \eqn{\mathrm{KL}(g|f)} is the opposite.

This asymmetry leads to Jeffrey's divergence, which is defined as a symmetrized version of the two Kullback--Leibler divergences, and is
\deqn{J(f,g) = \mathrm{KL}(f|g) + \mathrm{KL}(g|f) = \int\!\!\int_{\mathcal{I}^2} (g-f)\, \log(g/f)\,\mathrm{d}u\mathrm{d}v\mbox{.}}

The variances of the Kullback--Leibler divergences are defined as
\deqn{\sigma^2_{\mathrm{KL}(f|g)} = \int\!\!\int_{\mathcal{I}^2} g\,[\log(g/f)]^2\,\mathrm{d}u\mathrm{d}v - [\mathrm{KL}(f|g)]^2\mbox{,}}
and
\deqn{\sigma^2_{\mathrm{KL}(g|f)} = \int\!\!\int_{\mathcal{I}^2} f\,[\log(f/g)]^2\,\mathrm{d}u\mathrm{d}v - [\mathrm{KL}(g|f)]^2\mbox{.}}

For comparison of copula families \eqn{f} and \eqn{g} and taking an \eqn{\alpha = 0.05}, the Kullback--Leibler sample size is defined as
\deqn{n_{f\!g} = \bigl[\Phi^{(-1)}(1-\alpha) \times \eta_\mathrm{KL}\bigr]^2\mbox{,}}
where \eqn{\Phi^{(-1)}(t)} is the quantile function for the standard normal distribution \eqn{\sim} N(0,1) for nonexceedance probability \eqn{t}, and \eqn{\eta_\mathrm{KL}} is the maximum of
\deqn{\eta_\mathrm{KL} = \mathrm{max}\bigl[\sigma_{\mathrm{KL}(f|g)}/\mathrm{KL}(f|g),\, \sigma_{\mathrm{KL}(g|f)}/\mathrm{KL}(g|f)\bigr]\mbox{.}}
The \eqn{n_{f\!g}} gives an indication of the sample size needed to distinguish \eqn{f} and \eqn{g} with a probability of at least \eqn{1 - \alpha = 1 - 0.05 = 0.95} or 95 percent.

The \pkg{copBasic} features a \enc{naïve}{naive} \emph{Monte Carlo integration} scheme in the primary interface \code{kullCOP}, although the function \code{kullCOPint} provides for nested numerical integration. This later function is generally fast but suffers too much for general application from integral divergencies issued from the \code{integrate()} function in \R---this must be judged in the light that the package focuses only on implementation of the function of the copula itself and neither numerical estimation of copula density (\code{\link{densityCOP}}) and not analytical copula densities or hybrid representations thereof. Sufficient \dQuote{bread crumbs} are left among the code and documentation for users to re-implement if speed is paramount.  Numerical comparison to the results of Joe (2014) (see \bold{Examples}) suggests that the default Monte Carlo sample size should be more than sufficient for general inference with the expense of considerable CPU time; however, a couple of repeated calls of \code{kullCOP} would be advised and compute say the mean of the resulting sample sizes.
}
\usage{
kullCOP(cop1=NULL, cop2=NULL, para1=NULL, para2=NULL, alpha=0.05,
           del=0, n=1E5, verbose=TRUE, sobol=FALSE, ...)

kullCOPint(cop1=NULL, cop2=NULL, para1=NULL, para2=NULL, alpha=0.05,
           del=.Machine$double.eps^0.25, verbose=TRUE, ...)
}
\arguments{
  \item{cop1}{A copula function corresponding to copula \eqn{f} in Joe (2014);}
  \item{para1}{Vector of parameters or other data structure, if needed, to pass to the copula \eqn{f};}
  \item{cop2}{A copula function corresponding to copula \eqn{g} in Joe (2014);}
  \item{para2}{Vector of parameters or other data structure, if needed, to pass to the copula \eqn{g};}
  \item{alpha}{The \eqn{\alpha} in the Kullback--Leibler sample size equation;}
  \item{del}{A small value used to denote the lo and hi values of the numerical integration: \code{lo = del} and \code{hi = 1 - del}. If \code{del == 0}, then \code{lo = 0} and \code{hi = 1}, which corresponds to the theoretical limits \eqn{\mathcal{I}^2 = [0,1]^2} and are defaulted here to \eqn{[0,1]^2} because the Monte Carlo algorithm is preferred for general application. The end point control, however, is maintained just in case pathological situations should arise;}
  \item{n}{\code{kullCOP} (Monte Carlo integration) only---The Monte Carlo integration simulation size;}
  \item{verbose}{A logical trigging a couple of status lines of output through the \code{message()} function in \R;}
  \item{sobol}{A logical trigging \emph{Sobol sequences} for the Monte Carlo integration instead of the bivariate uniform distribution. The Sobol sequences are dependent on the \pkg{randtoolbox} package and the \code{sobol()} function; and}
  \item{...}{Additional arguments to pass to the \code{\link{densityCOP}} function.}
}
\value{
  An \R \code{list} is returned having the following components:
  \item{MonteCarlo.sim.size}{\code{kullCOP} (Monte Carlo integration) only---The simulation size for numerical integration;}
  \item{divergences}{A vector of the Kullback--Leibler divergences and their standard deviations: \eqn{\mathrm{KL}(f|g)}, \eqn{\sigma_{\mathrm{KL}(f|g)}}, \eqn{\mathrm{KL}(g|f)}, and \eqn{\sigma_{\mathrm{KL}(g|f)}}, respectively;}
  \item{stdev.divergences}{\code{kullCOP} (Monte Carlo integration) only---The standard deviation of the divergences and the variances;}
  \item{Jeffrey.divergence}{Jeffrey's divergence \eqn{J(f,g)};}
  \item{KL.sample.size}{Kullback--Leibler sample size \eqn{n_{f\!g}}; and}
  \item{integrations}{\code{kullCOPint} (numerical integration) only---An \R \code{list} of the outer call of the \code{integrate()} function for the respective numerical integrals shown in this documentation.}
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{densityCOP}}, \code{\link{vuongCOP}}}
\examples{
# See another demonstration under the Note section of statTn().
\dontrun{
# Joe (2014, p. 237, table 5.2)
# The Gumbel-Hougaard copula and Plackett copula below each have a Kendall Tau of
# about 0.5. Joe (2014) lists in the table that Jeffrey divergence is about 0.110
# and the Kullback-Leibler sample size is 133. Joe (2014) does not list the
# parameters for either copula, just that Kendall Tau = 0.5.
KL <- kullCOP(cop1=GHcop, para1=2, cop2=PLACKETTcop, para2=11.40484)
# Reports Jeffrey divergence        =   0.1063858
#      Kullback-Leibler sample size = 136 (another run produces 131)

S <- replicate(20, kullCOP(cop1=GHcop, para1=2, cop2=PLACKETTcop,
                           para2=11.40484, verbose=FALSE)$KL.sample.size)
print(as.integer(c(mean(S), sd(S)))) # 134 plus/minus 5

# Joe (2014, p. 237, table 5.3)
# The Gumbel-Hougaard copula and Plackett copula below each have a Spearman Rho of
# about 0.5. Joe (2014) lists in the table that Jeffrey divergence is about 0.063
# and the Kullback-Leibler sample size is 210. Joe (2014) does not list the
# parameters for either copula, just that for Spearman Rho = 0.5.
KL <- kullCOP(cop1=GHcop, para1=1.541071, cop2=PLACKETTcop, para2=5.115658)
# Reports Jeffrey divergence        =   0.06381151
#      Kullback-Leibler sample size = 220 (another run produces 203)

S <- replicate(20, kullCOP(cop1=GHcop, para1=1.541071, cop2=PLACKETTcop,
                           para2=5.115658, verbose=FALSE)$KL.sample.size)
print(as.integer(c(mean(S), sd(S))))  # 220 plus/minus 16

# Joe (2014) likely did the numerical integrations using analytical solutions to the
# probability densities and not rectangular approximations as in densityCOP().}

\dontrun{
# Compare Jeffery Divergence estimates as functions of sample size when computed
# using Sobol sequences or not---Sobol sequences have less sampling variability.
GHpar <- PApar <- 2 # Spearman Rho = 0.6822339
Ns <- as.integer(10^c(seq(2.0, 3.5,by=0.01), seq(3.6, 5,by=0.2)))
JDuni <- sapply(1:length(Ns), function(i) {
                  kullCOP(cop1=GHcop, para1=GHpar, verbose=FALSE,
                          cop2=PARETOcop, para2=PApar, n=Ns[i],
                          sobol=FALSE)$Jeffrey.divergence })
JDsob <- sapply(1:length(Ns), function(i) {
                  kullCOP(cop1=GHcop, para1=GHpar, verbose=FALSE,
                          cop2=PARETOcop, para2=PApar, n=Ns[i],
                          sobol=TRUE )$Jeffrey.divergence })
plot(Ns, JDuni, type="l", log="x", # black line, notice likely outliers too
     xlab="Sample Size", ylab="Jeffery Divergence")
lines(Ns, JDsob, col=2) # red line
print(c(mean(JDuni), sd(JDuni))) # [1] 0.05923462 0.01544651
print(c(mean(JDsob), sd(JDsob))) # [1] 0.05863623 0.01184879
# So we see a slightly smaller variation when the Sobol sequence is used. }
}
\keyword{inference}
\keyword{copula (inference)}
\keyword{copula (density)}
\keyword{Joe (2014) Examples and Exercises}
\concept{Kullback--Leibler divergence}
\concept{Kullback--Leibler sample size}
\concept{Kullback-Leibler divergence}
\concept{Kullback-Leibler sample size}
\concept{Jeffery divergence}
\concept{Jeffery's divergence}
\concept{copula divergence}
\keyword{Sobol sequence (Monte Carlo integration)}
