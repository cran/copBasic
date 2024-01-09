\encoding{utf8}
\name{semicorCOP}
\alias{semicorCOP}
\title{Lower and Upper Semi-Correlations of a Copula}
\description{
Compute the \emph{lower semi-correlations} (bottom-left)
\deqn{\rho^{N{-}{-}}_\mathbf{C}(u,v; a) = \rho_N^{{-}{-}}(a) \mbox{ and}}
compute the \emph{upper semi-correlations} (top-right)
\deqn{\rho^{N{+}{+}}_\mathbf{C}(u,v; a) = \rho_N^{{+}{+}}(a)}
of a copula \eqn{\mathbf{C}(u,v)} (Joe, 2014, p. 73) using numerical simulation. The semi-correlations are defined as
\deqn{\rho_N^{{-}{-}}(a) = \mathrm{cor}[Z_1, Z_2 \mid Z_1 < -a, Z_2 < -a]\mbox{,}}
\deqn{\rho_N^{{+}{+}}(a) = \mathrm{cor}[Z_1, Z_2 \mid Z_1 > +a, Z_2 > +a]\mbox{,\ and}}
\deqn{\rho_N(a > -\infty) = \mathrm{cor}[Z_1, Z_2]\mbox{,}}
where \eqn{\mathrm{cor[z_1, z_2]}} is the familiar \emph{Pearson correlation function}, which is in \R the syntax \code{cor(...,} \code{method="pearson")}, parameter \eqn{a \ge 0} is a truncation point that identifies \emph{truncated tail regions} (Joe, 2014, p. 73), and lastly \eqn{(Z_1, Z_2) \sim \mathbf{C}(\Phi, \Phi)} and thus from the standard normal distribution \eqn{(Z_1, Z_2) = (\Phi^{-1}(u), \Phi^{-1}(v))} where the random variables \eqn{(U,V) \sim \mathbf{C}}.

The semi-correlations are extended for the \pkg{copBasic} package into bottom right and top left versions as well by
\deqn{\rho_N^{{+}{-}}(a) = \mathrm{cor}[Z_1, Z_2 \mid Z_1 > +a, Z_2 < -a]\mbox{,\ and}}
\deqn{\rho_N^{{-}{+}}(a) = \mathrm{cor}[Z_1, Z_2 \mid Z_1 < -a, Z_2 > +a]\mbox{.}}
As a result, the notations \eqn{{-}{-}}, \eqn{{+}{+}}, \eqn{{+}{-}}, and \eqn{{-}{+}} can be used to represent each of the respective corners bottom-left, top-right, bottom-right, and top-left of the \eqn{(u,v)} domain with the respective truncation. These words are used in the variable names of the returned list from the function.
}
\usage{
semicorCOP(cop=NULL, para=NULL, truncation=0, n=0, as.sample=FALSE, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{truncation}{The truncation value for \eqn{a}, which is in standard normal variates, and the default of zero is the origin (medians);}
  \item{n}{The sample size \eqn{n} for simulation estimates of the \eqn{\rho_N};}
  \item{as.sample}{A logical controlling whether an optional \code{data.frame} in \code{para} is used to compute the \eqn{\hat\rho_N} (see \bold{Note}); and}
  \item{...}{Additional arguments to pass to the copula.}
}
\value{
  The value(s) for \eqn{\rho_N}, \eqn{\rho_N^{{-}{-}}}, \eqn{\rho_N^{{+}{+}}}, \eqn{\rho_N^{{+}{-}}}, and \eqn{\rho_N^{{-}{+}}} are returned.
}
\note{
The sample semi-correlations can be computed from a two-column table that is passed into the function using the \code{para} argument. Although the truncation point \eqn{a \ge 0}, as \eqn{a} increases and focus is increasingly made into one or the other truncated tail regions, the sample version with data becomes decreasing well estimated because the available sample size diminishes. The \code{para} argument can contain probabilities or raw data because internally the function computes the \emph{Hazen plotting positions} (\emph{e.g.} \eqn{u_i = (i-0.5) / n} for rank \eqn{i} and sample size \eqn{n}) because Joe (2014, pp. 9, 17, 245, 247--248) repeatedly emphasizes this form of plotting position when normal scores are involved.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{giniCOP}}, \code{\link{rhoCOP}}, \code{\link{tauCOP}}, \code{\link{COP}}
}
\examples{
\dontrun{
# Gumbel-Hougaard copula with Pearson rhoN = 0.4 (by definition)
run <- sapply(1:50, function(i) semicorCOP(cop=GHcop, para=1.350, n=600))
mean(unlist(run[1,])) # cor.normal.scores
mean(unlist(run[2,])) #  botleft.semicor
mean(unlist(run[3,])) # topright.semicor
sd(  unlist(run[1,])) # cor.normal.scores (These are our sampling variations
sd(  unlist(run[2,])) #  botleft.semicor   for the n=600 used as a Monte
sd(  unlist(run[3,])) # topright.semicor   Carlo simulation.)
# The function returns:    rhoN = 0.392112, rhoN--= 0.117674, rhoN++= 0.404733
#  standard deviations           (0.038883)        (0.073392)        (0.073942)
# Joe (2014, p. 72) shows: rhoN = 0.4_____, rhoN--= 0.132___, rhoN++= 0.415___
#  standard deviations           (not avail)       (0.08____)        (0.07____)
# We see alignment with the results of Joe with his n=600. #}

\dontrun{
p <- 0.5 # Reasonable strong positive association for the Raftery copula,
# but then we are going to be reflecting and rotating the copula.
# See similar Example under COP() function.
"RFcop1" <- function(u,v, para) COP(u,v, cop=RFcop, para=para, reflect="1")
"RFcop2" <- function(u,v, para) COP(u,v, cop=RFcop, para=para, reflect="2")
"RFcop3" <- function(u,v, para) COP(u,v, cop=RFcop, para=para, reflect="3")
"RFcop4" <- function(u,v, para) COP(u,v, cop=RFcop, para=para, reflect="4")
RF <- NULL; n <- 10000
RF <- rbind(RF, as.data.frame(semicorCOP(cop=RFcop1, para=p, n=n))[,1:5])
RF <- rbind(RF, as.data.frame(semicorCOP(cop=RFcop2, para=p, n=n))[,1:5])
RF <- rbind(RF, as.data.frame(semicorCOP(cop=RFcop3, para=p, n=n))[,1:5])
RF <- rbind(RF, as.data.frame(semicorCOP(cop=RFcop4, para=p, n=n))[,1:5])
print(RF[,1:3]) # total sample and lower and upper semi-correlations
#   cor.normal.scores botleft.semicor topright.semicor
# 1         0.5587837      0.74124686       0.10027641
# 2         0.5567889      0.10302772       0.73729702
# 3        -0.5807201     -0.04683536      -0.01714573 # see near zeros --,++
# 4        -0.5698139      0.03040520       0.05125916 # see near zeros --,++
print(RF[,2:5]) # now look at all four corners
#   botleft.semicor topright.semicor topleft.semicor botright.semicor
# 1      0.74124686       0.10027641      0.01529508       0.02046530
# 2      0.10302772       0.73729702     -0.05195628       0.01747874
# 3     -0.04683536      -0.01714573     -0.11106842      -0.74077321
# 4      0.03040520       0.05125916     -0.74516061      -0.07636233
# Notice how the tight tail of the copula, being reflected and rotated
# into each of the four corners, shows semi-correlation magnitude of 0.74.
# See the copula density plots of COP() Examples section. }
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{copula (tail characteristics)}
\keyword{copula (tail properties)}
\keyword{semi-correlation coefficient}
\concept{semi-correlation}
\keyword{Joe (2014) Examples and Exercises}
