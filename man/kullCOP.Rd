\encoding{utf8}
\name{kullCOP}
\alias{kullCOP}
\alias{kullCOPint}
\title{Kullback--Leibler Divergence, Jeffrey Divergence, and Kullback--Leibler Sample Size}
\description{
Compute the \emph{Kullback--Leibler Divergence}, \emph{Jeffrey Divergence}, and \emph{Kullback--Leibler sample size} following Joe (2014, pp. 234--237). Consider two densities \eqn{f = c_1(u,v; \Theta_f)} and \eqn{g = c_2(u,v; \Theta_g)} for two different bivariate copulas \eqn{\mathbf{C}_1(\Theta_1)} and \eqn{\mathbf{C}_2(\Theta_2)} having respective parameters \eqn{\Theta}, then the Kullback--Leibler Divergence of \eqn{f} relative to \eqn{g} is
\deqn{\mathrm{KL}(f {\mid} g) = \int\!\!\int_{\mathcal{I}^2} g\, \log(g/f)\,\mathrm{d}u\mathrm{d}v\mbox{,}}
and Kullback--Leibler Divergence of \eqn{g} relative to \eqn{f} is
\deqn{\mathrm{KL}(g {\mid} f) = \int\!\!\int_{\mathcal{I}^2} f\, \log(f/g)\,\mathrm{d}u\mathrm{d}v\mbox{,}}
where the limits of integration \eqn{\mathcal{I}^2} theoretically are closed on \eqn{[0,1]^2} but an open interval \eqn{(0,1)^2} might be needed for numerical integration. Note, in general \eqn{\mathrm{KL}(f {\mid} g) \ne \mathrm{KL}(g {\mid} f)}. The \eqn{\mathrm{KL}(f {\mid} g)} is the expected log-likelihood ratios of \eqn{g} to \eqn{f} when \eqn{g} is the true density (Joe, 2014, p. 234), whereas \eqn{\mathrm{KL}(g {\mid} f)} is the opposite.

This asymmetry leads to Jeffrey Divergence, which is defined as a symmetrized version of the two Kullback--Leibler Divergences, and is
\deqn{J(f,g) = \mathrm{KL}(f {\mid} g) + \mathrm{KL}(g {\mid} f) = \int\!\!\int_{\mathcal{I}^2} (g-f)\, \log(g/f)\,\mathrm{d}u\mathrm{d}v\mbox{.}}

The variances of the Kullback--Leibler Divergences are defined as
\deqn{\sigma^2_{\mathrm{KL}(f {\mid} g)} = \int\!\!\int_{\mathcal{I}^2} g\,[\log(g/f)]^2\,\mathrm{d}u\mathrm{d}v - [\mathrm{KL}(f|g)]^2\mbox{,}}
and
\deqn{\sigma^2_{\mathrm{KL}(g {\mid} f)} = \int\!\!\int_{\mathcal{I}^2} f\,[\log(f/g)]^2\,\mathrm{d}u\mathrm{d}v - [\mathrm{KL}(g|f)]^2\mbox{.}}

For comparison of copula families \eqn{f} and \eqn{g} and taking an \eqn{\alpha = 0.05}, the Kullback--Leibler sample size is defined as
\deqn{n_{f\!g} = \bigl[\Phi^{(-1)}(1-\alpha) \times \eta_\mathrm{KL}\bigr]^2\mbox{,}}
where \eqn{\Phi^{(-1)}(t)} is the quantile function for the standard normal distribution \eqn{\sim} N(0,1) for nonexceedance probability \eqn{t}, and \eqn{\eta_\mathrm{KL}} is the maximum of
\deqn{\eta_\mathrm{KL} = \mathrm{max}\bigl[\sigma_{\mathrm{KL}(f {\mid} g)}/\mathrm{KL}(f {\mid} g),\, \sigma_{\mathrm{KL}(g {\mid} f)}/\mathrm{KL}(g {\mid} f)\bigr]\mbox{.}}
The \eqn{n_{f\!g}} gives an indication of the sample size needed to distinguish \eqn{f} and \eqn{g} with a probability of at least \eqn{1 - \alpha = 1 - 0.05 = 0.95} or 95 percent.

The \pkg{copBasic} features a \enc{naïve}{naive} \emph{Monte Carlo integration} scheme in the primary interface \code{kullCOP}, although the function \code{kullCOPint} provides for nested numerical integration. This later function is generally fast but suffers too much for general application from integral divergencies issued from the \code{integrate()} function in \R---this must be judged in the light that the \pkg{copBasic} package focuses only on implementation of the function of the copula itself and numerical estimation of copula density (\code{\link{densityCOP}}) and not analytical copula densities or hybrid representations thereof. Sufficient \dQuote{bread crumbs} are left among the code and documentation for users to re-implement if speed is paramount. Numerical comparison to the results of Joe (2014) (see \bold{Examples}) suggests that the default Monte Carlo sample size should be more than sufficient for general inference with the expense of considerable CPU time; however, a couple of repeated calls of \code{kullCOP} would be advised and compute say the mean of the resulting sample sizes.
}
\usage{
kullCOP(cop1=NULL, cop2=NULL, para1=NULL, para2=NULL, alpha=0.05,
           del=0, n=1E5, verbose=TRUE, sobol=FALSE, scrambling=0, ...)

kullCOPint(cop1=NULL, cop2=NULL, para1=NULL, para2=NULL, alpha=0.05,
           del=.Machine$double.eps^0.25, verbose=TRUE, ...)
}
\arguments{
  \item{cop1}{A copula function corresponding to copula \eqn{f} in Joe (2014);}
  \item{para1}{Vector of parameters or other data structure, if needed, to pass to the copula \eqn{f};}
  \item{cop2}{A copula function corresponding to copula \eqn{g} in Joe (2014);}
  \item{para2}{Vector of parameters or other data structure, if needed, to pass to the copula \eqn{g};}
  \item{alpha}{The \eqn{\alpha} in the Kullback--Leibler sample size equation;}
  \item{del}{A small value used to denote the \code{lo} and \code{hi} values of the numerical integration: \code{lo = del} and \code{hi = 1 - del}. If \code{del == 0}, then \code{lo = 0} and \code{hi = 1}, which corresponds to the theoretical limits \eqn{\mathcal{I}^2 = [0,1]^2} and are defaulted here to \eqn{[0,1]^2} because the Monte Carlo algorithm is preferred for general application. The end point control, however, is maintained just in case pathological situations should arise;}
  \item{n}{\code{kullCOP} (Monte Carlo integration) only---the Monte Carlo integration simulation size;}
  \item{verbose}{A logical trigging a couple of status lines of output through the \code{message()} function in \R;}
  \item{sobol}{A logical trigging \emph{Sobol sequences} for the Monte Carlo integration instead of the bivariate uniform distribution. The Sobol sequences are dependent on the \pkg{randtoolbox} package and the \code{sobol()} function of the \pkg{randtoolbox} package, and the Sobol sequences canvas the \eqn{\mathcal{I}^2} domain for smaller \eqn{n} values than required if statistical independence is used for the Monte Carlo integration. Note, the \pkg{randtoolbox} at least at version 2.0.+ has \dQuote{scrambling} of Sobol sequences temporarily disabled, and hence \code{scrambling=0} as default for \code{kullCOP};}
  \item{scrambling}{The argument of the same name for \code{randtoolbox::sobol}; and}
  \item{...}{Additional arguments to pass to the \code{\link{densityCOP}} function.}
}
\value{
  An \R \code{list} is returned having the following components:
  \item{MonteCarlo.sim.size}{\code{kullCOP} (Monte Carlo integration) only---The simulation size for numerical integration;}
  \item{divergences}{A vector of the Kullback--Leibler Divergences and their standard deviations: \eqn{\mathrm{KL}(f {\mid} g)}, \eqn{\sigma_{\mathrm{KL}(f {\mid} g)}}, \eqn{\mathrm{KL}(g {\mid} f)}, and \eqn{\sigma_{\mathrm{KL}(g {\mid} f)}}, respectively;}
  \item{stdev.divergences}{\code{kullCOP} (Monte Carlo integration) only---The standard deviation of the divergences and the variances;}
  \item{Jeffrey.divergence}{Jeffrey Divergence \eqn{J(f,g)};}
  \item{KL.sample.size}{Kullback--Leibler sample size \eqn{n_{f\!g}}; and}
  \item{integrations}{\code{kullCOPint} (numerical integration) only---An \R \code{list} of the outer call of the \code{integrate()} function for the respective numerical integrals shown in this documentation.}
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{densityCOP}}, \code{\link{vuongCOP}}}
\examples{
# See another demonstration under the Note section of statTn().
\dontrun{
# Joe (2014, p. 237, table 5.2)
# Gumbel-Hougaard and Plackett copulas below each have a Kendall Tau of about 0.5, and
# Joe (2014) lists in the table that Jeffrey Divergence is about 0.110 and Kullback-Leibler
# sample size is 133. Joe (2014) does not list the copula parameters just says Tau = 0.5.
# Joe (2014) likely did the numerical integrations using analytical solutions to probability
# densities and not rectangular approximations as in copBasic::densityCOP().
set.seed(1)
KL <- kullCOP(cop1=GHcop,       para1=2,
              cop2=PLACKETTcop, para2=11.40484, sobol=FALSE)
message("Jeffery Divergence is ",          round(KL$Jeffrey.divergence, digits=4),
        " and Kullback-Leibler sample size is ", KL$KL.sample.size, ".")
# Jeffery Divergence is 0.1106 and Kullback-Leibler sample size is 137.
set.seed(1)
KL <- kullCOP(cop1=GHcop,       para1=2,
              cop2=PLACKETTcop, para2=11.40484, sobol=TRUE )
message("Jeffery Divergence is ",          round(KL$Jeffrey.divergence, digits=4),
        " and Kullback-Leibler sample size is ", KL$KL.sample.size, ".")
# Jeffery Divergence is 0.3062 and Kullback-Leibler sample size is 136.


set.seed(1)
S <- replicate(20, kullCOP(cop1=GHcop, para1=2, cop2=PLACKETTcop, sobol=FALSE,
                           para2=11.40484, verbose=FALSE)$KL.sample.size)
print(as.integer(c(mean(S), sd(S)))) # 132 plus/minus 5
S <- replicate(2 , kullCOP(cop1=GHcop, para1=2, cop2=PLACKETTcop,  sobol=TRUE,
                           para2=11.40484, verbose=FALSE)$KL.sample.size)
# The two S in the later replication are both the same (136) for a sobol=TRUE
# does not produce variation and this is thought (June 2023) as a result
# of the disabled scrambling in the randtoolbox::sobol() function. }

\dontrun{
# Joe (2014, p. 237, table 5.3)
# Gumbel-Hougaard and Plackett copulas below each have a Spearman Rho of about 0.5, and
# Joe (2014) lists in the table that Jeffrey Divergence is about 0.063 and Kullback-Leibler
# sample size is 210. Joe (2014) does not list the parameters and just says that Rho = 0.5.
# Joe (2014) likely did the numerical integrations using analytical solutions to probability
# densities and not rectangular approximations as in copBasic::densityCOP().
set.seed(1)
KL <- kullCOP(cop1=GHcop,       para1=1.541071,
              cop2=PLACKETTcop, para2=5.115658, sobol=FALSE)
message("Jeffery Divergence is ",          round(KL$Jeffrey.divergence, digits=4),
        " and Kullback-Leibler sample size is ", KL$KL.sample.size, ".")
# Jeffery Divergence is 0.0642 and Kullback-Leibler sample size is 213.
set.seed(1)
KL <- kullCOP(cop1=GHcop,       para1=1.541071,
              cop2=PLACKETTcop, para2=5.115658, sobol=TRUE )
message("Jeffery Divergence is ",          round(KL$Jeffrey.divergence, digits=4),
        " and Kullback-Leibler sample size is ", KL$KL.sample.size, ".")
# Jeffery Divergence is 0.2001 and Kullback-Leibler sample size is 206.


set.seed(1)
S <- replicate(20, kullCOP(cop1=GHcop, para1=1.541071, cop2=PLACKETTcop,
                           para2=5.115658, verbose=FALSE)$KL.sample.size)
print(as.integer(c(mean(S), sd(S))))  # 220 plus/minus 19 }

\dontrun{
# Compare Jeffery Divergence estimates as functions of sample size when computed
# using Sobol sequences or not for Gumbel-Hougaard and Pareto copulas.
GHpar <- PApar <- 2 # Spearman Rho = 0.6822339
Ns <- as.integer(10^c(seq(2.0, 3.5, by=0.01), seq(3.6, 5, by=0.05)))
JDuni <- sapply(1:length(Ns), function(i) {
                  kullCOP(cop1=GHcop, para1=GHpar, verbose=FALSE,
                          cop2=PAcop, para2=PApar, n=Ns[i],
                          sobol=FALSE)$Jeffrey.divergence })
JDsob <- sapply(1:length(Ns), function(i) {
                  kullCOP(cop1=GHcop, para1=GHpar, verbose=FALSE,
                          cop2=PAcop, para2=PApar, n=Ns[i],
                          sobol=TRUE )$Jeffrey.divergence })
plot(Ns, JDuni, type="l", log="x", # black line, notice likely outliers too
     xlab="Simulation Sample Size", ylab="Jeffery Divergence")
lines(Ns, JDsob, col="red") # red line
legend("topright", c("Monte Carlo", "Sobol sequence"),
                   lwd=c(1,1), col=c("black", "red"), bty="n")
print( c( mean(JDuni), sd(JDuni) ) ) # [1] 0.05915608 0.01284682
print( c( mean(JDsob), sd(JDsob) ) ) # [1] 0.07274190 0.01838939

# The developer notes that plotting KL.sample.size for sobol=TRUE shows
# what appears to be numerical blow up but the Jeffery Divergence does not.
KLuni <- sapply(1:length(Ns), function(i) {
                  kullCOP(cop1=GHcop, para1=GHpar, verbose=FALSE,
                          cop2=PAcop, para2=PApar, n=Ns[i],
                          sobol=FALSE)$KL.sample.size })
KLsob <- sapply(1:length(Ns), function(i) {
                  kullCOP(cop1=GHcop, para1=GHpar, verbose=FALSE,
                          cop2=PAcop, para2=PApar, n=Ns[i],
                          sobol=TRUE )$KL.sample.size })
plot(Ns, KLuni, type="l", log="xy", # black line, notice likely outliers too
     xlab="Simulation Sample Size", ylab="Kullback-Leibler Sample Size")
lines(Ns, KLsob, col="red") # red line
nideal <- kullCOPint(cop1=GHcop, para1=GHpar, cop2=PAcop, para2=PApar)$KL.sample.size
abline(h=nideal, col="green", lwd=3) # nideal sample size is about 210
legend("topright", c("Monte Carlo", "Sobol sequence", "Judged ideal sample size"),
                   lwd=c(1,1,3), col=c("black", "red", "green"), bty="n")

# Let us attempt a visualization to highlight the differences in the two copula by
# simulation. First, using this n = nideal, being the apparent sample size to distinguish
# generally between the two copula having the same Spearman Rho. Do the segments help
# to visually highlight the differences? Next, ask would one judge the parents in the
# simulation being different knowing same Spearman Rho? (Note, the segments are all
# vertical because the U axis is the simulation and the V axis is the conditional
# probability given the U.)
set.seed(1); UVgh <- simCOP(nideal, GHcop, para=GHpar, graphics=FALSE)
set.seed(1); UVpa <- simCOP(nideal, PAcop, para=PApar, graphics=FALSE)
plot(c(0,1), c(0,1), type="n", xlab="U, nonexceedance probability",
                               ylab="V, nonexceedance probability")
segments(UVgh[,1], UVgh[,2], x1=UVpa[,1], y1=UVpa[,2])
points(UVgh, col="lightgreen", pch=16, cex=0.8) # dots
points(UVpa, col="darkgreen",  pch= 1, lwd=0.8) # circles
# Repeat the above n = nideal visualizations but with a change to n = nideal*10, and see
# then that there are visually striking shifts systematically in both both tails but also
# in the U in the interval (0.3, 0.7) belt but to a smaller degree than seen in the tails. }
}
\keyword{inference}
\keyword{copula (inference)}
\keyword{copula (density)}
\keyword{Joe (2014) Examples and Exercises}
\keyword{Sobol sequence (Monte Carlo integration)}
\keyword{Package randtoolbox}
\concept{Kullback--Leibler Divergence}
\concept{Kullback--Leibler sample size}
\concept{Kullback-Leibler Divergence}
\concept{Kullback-Leibler sample size}
\concept{Jeffery Divergence}
\concept{Jeffery's Divergence}
\concept{copula divergence}
\concept{relative entropy}
\concept{I-divergence}
