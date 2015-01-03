\encoding{utf8}
\name{kullCOP}
\alias{kullCOP}
\title{ Kullback-Leibler Divergence, Jeffrey's Divergence, and Kullback-Leibler Sample Size }
\description{
Compute the \emph{Kullback-Leibler divergence}, \emph{Jeffrey's divergence}, and \emph{Kullback-Leibler sample size} following Joe (2015, pp. 234--237). Consider two densities \eqn{f = c_1(u,v; \Theta_f)} and \eqn{g = c_2(u,v; \Theta_g)} for two different bivariate copulas \eqn{\mathbf{C_1(\Theta_1)}} and \eqn{\mathbf{C_2(\Theta_2)}} having respective parameters \eqn{\Theta}, then the Kullback-Leibler divergence of \eqn{f} relative to \eqn{g} is
\deqn{\mathrm{KL}(f|g) = \int\!\!\int_{\mathcal{I}^2} g\, \log(g/f)\,\mathrm{d}u\mathrm{d}v\mbox{,}}
and Kullback-Leibler divergence of \eqn{g} relative to \eqn{f} is
\deqn{\mathrm{KL}(g|f) = \int\!\!\int_{\mathcal{I}^2} f\, \log(f/g)\,\mathrm{d}u\mathrm{d}v\mbox{,}}
where the limits of integration \eqn{\mathcal{I}^2} theoretically are closed on \eqn{[0,1]^2} but an open interval \eqn{(0,1)} might be needed for numerical stability for the nested calls of the \code{integrate} function because of the use of rectangular density estimation by \code{\link{densityCOP}}. Note that in general \eqn{\mathrm{KL}(f|g) \ne \mathrm{KL}(g|f)}. The \eqn{\mathrm{KL}(f|g)} is the expected log-likelihood ratios of \eqn{g} to \eqn{f} when \eqn{g} is the true density (Joe, 2015, p. 234), whereas \eqn{\mathrm{KL}(g|f)} is the opposite. This asymmetry leads to Jeffrey's divergence, which is defined as a symmetrized version of the Kullback-Leibler divergences and is
\deqn{J(f,g) = \mathrm{KL}(f|g) + \mathrm{KL}(g|f) = \int\!\!\int_{\mathcal{I}^2} (g-f)\, \log(g/f)\,\mathrm{d}u\mathrm{d}v\mbox{.}}

The variances of the Kullback-Leibler divergences are defined as
\deqn{\sigma^2_{\mathrm{KL}(f|g)} = \int\!\!\int_{\mathcal{I}^2} g\,[\log(g/f)]^2\,\mathrm{d}u\mathrm{d}v - [\mathrm{KL}(f|g)]^2\mbox{,}}
and
\deqn{\sigma^2_{\mathrm{KL}(g|f)} = \int\!\!\int_{\mathcal{I}^2} f\,[\log(f/g)]^2\,\mathrm{d}u\mathrm{d}v - [\mathrm{KL}(g|f)]^2\mbox{.}}

For comparison of copula families \eqn{f} and \eqn{g} and taking an \eqn{\alpha = 0.05}, the Kullback-Leibler sample size is defined as
\deqn{n_{fg} = [\Phi^{(-1)}(1-\alpha) \times \eta_\mathrm{KL}]^2\mbox{,}}
where \eqn{\Phi^{(-1)}(t)} is the quantile function for the Standard Normal distribution for nonexceedance probability \eqn{t}, and \eqn{\eta} is the maximum of
\deqn{\eta_\mathrm{KL} = \mathrm{max}[\sigma_{\mathrm{KL}(f|g)}/\mathrm{KL}(f|g),\, \sigma_{\mathrm{KL}(g|f)}/\mathrm{KL}(g|f)]\mbox{.}}
The \eqn{n_{fg}} gives an indication of the sample size needed to distinguish \eqn{f} and \eqn{g} with a probability of at least \eqn{1 - \alpha = 1 - 0.05 = 0.95} or 95 percent.
}
\usage{
kullCOP(cop1=NULL, cop2=NULL, para1=NULL, para2=NULL, alpha=0.05,
        del=.Machine$double.eps^0.25, ...)
}
\arguments{
  \item{cop1}{A copula function corresponding to copula \eqn{f} in Joe (2015);}
  \item{para1}{Vector of parameters or other data structure, if needed, to pass to the copula \eqn{f};}
  \item{cop2}{A copula function corresponding to copula \eqn{g} in Joe (2015);}
  \item{para2}{Vector of parameters or other data structure, if needed, to pass to the copula \eqn{g};}
  \item{alpha}{The \eqn{\alpha} in the Kullback-Leibler sample size equation;}
  \item{del}{A small value used to denote the lo and hi values of the numerical integration: \code{lo = del} and \code{hi = 1 - del}. If \code{del == 0}, then \code{lo = 0} and \code{hi = 1}, which corresponds to the theoretical limits \eqn{\mathcal{I}^2 = [0,1]^2}, but are defaulted here to \eqn{(0,1)^2};}
  \item{...}{Additional arguments to pass to the \code{\link{densityCOP}} function.}
}
\value{
  An \R \code{list} is returned having the following components:
  \item{divergences}{A vector of the Kullback-Leibler divergences and their standard deviations: \eqn{\mathrm{KL}(f|g)}, \eqn{\sigma_{\mathrm{KL}(f|g)}}, \eqn{\mathrm{KL}(g|f)}, \eqn{\sigma_{\mathrm{KL}(g|f)}};}
  \item{Jeffrey.divergence}{Jeffrey's divergence \eqn{J(f,g)};}
  \item{KL.sample.size}{Kullback-Leibler sample size \eqn{n_{fg}}; and}
  \item{integrations}{An \R \code{list} of the outer \code{integrate} function call for the respective numerical integrals shown in this documentation.}
}
\references{
Joe, H., 2015, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{densityCOP}}, \code{\link{vuongCOP}}}
\examples{
\dontrun{
# Joe (2015, p. 237, table 5.2)
# The Gumbel-Hougaard copula and Plackett copula below each have a Kendall's tau
# of about 0.5. Joe (2015) lists in the table that Jeffrey's divergence is about
# 0.110 and the Kullback-Leibler sample size is 133. Joe (2015) does not list
# the parameters for either copula, just that for tau = 0.5.
KL <- kullCOP(cop1=GHcop, para1=2, cop2=PLACKETTcop, para2=11.40484)
# Reports Jeffrey's divergence      =   0.1087776
#      Kullback-Leibler sample size = 132
# using the default open set for the nested integrations. The closed set [0,1]
# causes integral divergence error for the Plackett copula.

# Joe (2015, p. 237, table 5.3)
# The Gumbel-Hougaard copula and Plackett copula below each have a Spearman's rho
# of about 0.5. Joe (2015) lists in the table that Jeffrey's divergence is about
# 0.063 and the Kullback-Leibler sample size is 210. Joe (2015) does not list
# the parameters for either copula, just that for rho = 0.5.
KL <- kullCOP(cop1=GHcop, para1=1.541071, cop2=PLACKETTcop, para2=5.115658,
                                                      del=0.0002, deluv=0.00001)
# Reports Jeffrey's divergence      =   0.06057848
#      Kullback-Leibler sample size = 207
# using the default open set for the nested integrations. The closed set [0,1]
# causes integral divergence error for the Plackett copula. Adjustments are made
# to the del for the numerical integrations and the deluv of the rectangular
# probability densities.  Joe (2015) likely did the numerical integrations using
# analytical solutions to the probability densities and not rectangular
# approximations as used in this package by the densityCOP() function.
}
}
\keyword{copula (inference)}
\keyword{copula (density)}
\keyword{Joe (2015) Examples and Exercises}
\concept{Kullback-Leibler}

