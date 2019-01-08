\encoding{utf8}
\name{semicorCOP}
\alias{semicorCOP}
\title{ The Lower and Upper Semi-Correlations of a Copula }
\description{
Compute the \emph{lower semi-correlations}
\deqn{\rho^{N-}_\mathbf{C}(u,v; a) = \rho_N^{-}(a) \mbox{ and}}
compute the \emph{upper semi-correlations}
\deqn{\rho^{N+}_\mathbf{C}(u,v; a) = \rho_N^{+}(a)}
of a copula \eqn{\mathbf{C}(u,v)} (Joe, 2014, p. 73) using numerical simulation. The semi-correlations are defined as
\deqn{\rho_N^{-}(a) = \mathrm{cor}[Z_1, Z_2|Z_1 < -a, Z_2 < -a]\mbox{,}}
\deqn{\rho_N^{+}(a) = \mathrm{cor}[Z_1, Z_2|Z_1 > +a, Z_2 > +a]\mbox{,\ and}}
\deqn{\rho_N(a > -\infty) = \mathrm{cor}[Z_1, Z_2]\mbox{,}}
where \eqn{\mathrm{cor[z_1, z_2]}} is the familiar \emph{Pearson's correlation} function, which is in \R the syntax \code{cor(...,} \code{method="pearson")}, parameter \eqn{a \ge 0} is a truncation point that identifies \emph{truncated tail regions} (Joe, 2014, p. 73), and lastly \eqn{(Z_1, Z_2) \sim \mathbf{C}(\Phi, \Phi)} and thus from the standard normal distribution \eqn{(Z_1, Z_2) = (\Phi^{-1}(u), \Phi^{-1}(v))} where the random variables \eqn{(U,V) \sim \mathbf{C}}.
}
\usage{
semicorCOP(cop=NULL, para=NULL, truncation=0, n=0, as.sample=FALSE, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{truncation}{The truncation value for \eqn{a}, which is in standard normal variates;}
  \item{n}{The sample size \eqn{n} for simulation estimates of the \eqn{\rho_N};}
  \item{as.sample}{A logical controlling whether an optional \code{data.frame} in \code{para} is used to compute the \eqn{\hat\rho_N} (see \bold{Note}); and}
  \item{...}{Additional arguments to pass to the copula.}
}
\value{
  The value(s) for \eqn{rho_N}, \eqn{\rho_N^{-}}, \eqn{\rho_N^{+}} are returned.
}
\note{
The sample semi-correlations can be computed from a two column table that is passed into the function using the \code{para} argument. Although the truncation point \eqn{a \ge 0}, as \eqn{a} increases and focus is increasingly made into one or the other truncated tail regions, the sample version with data becomes decreasing well estimated because the available sample size diminishes. The \code{para} argument can contain probabilities or raw data because internally the function computes the \emph{Hazen plotting positions} (\emph{e.g.} \eqn{u_i = (i-0.5)/n} for rank \eqn{i} and sample size \eqn{n}) because Joe (2014, pp. 9, 17, 245, 247--248) repeatedly emphasizes this form of plotting position when normal scores are involved.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{giniCOP}}, \code{\link{rhoCOP}}, \code{\link{tauCOP}}
}
\examples{
\dontrun{
# Gumbel-Hougaard copula with Pearson rhoN = 0.4 (by definition)
run <- sapply(1:20, function(i) semicorCOP(cop=GHcop, para=1.350, n=600))
mean(unlist(run[1,])) # cor.normal.scores
mean(unlist(run[2,])) # minus.semicor
mean(unlist(run[3,])) # plus.semicor
sd(  unlist(run[1,])) # cor.normal.scores (These are our sampling variations
sd(  unlist(run[2,])) # minus.semicor      for the n=600 used as a Monte
sd(  unlist(run[3,])) # plus.semicor       Carlo simulation.)
# The function returns:    rhoN = 0.3945714, rhoN-= 0.1312561, rhoN+= 0.4108908
#  standard deviations           (0.0378331)       (0.0744049)       (0.0684766)
# Joe (2014, p. 72) shows: rhoN = 0.4,       rhoN-= 0.132,     rhoN+= 0.415
#  standard deviations           (not avail)       (0.08)            (0.07)
# We see alignment with Joe's results with his n=600. #}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{copula (tail characteristics)}
\keyword{copula (tail properties)}
\keyword{semi-correlation coefficient}
\concept{semi-correlation}
\keyword{Joe (2014) Examples and Exercises}
