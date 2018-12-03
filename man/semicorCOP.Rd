\encoding{utf8}
\name{semicorCOP}
\alias{semicorCOP}
\title{ The Lower and Upper Semi-Correlations of a Copula }
\description{
Compute the \emph{lower semi-correlations}
\deqn{\rho^{N-}_\mathbf{C}(u,v; a) = \rho_N^{-}(a) \mbox{ and}}
compute the \emph{upper semi-correlations}
\deqn{\rho^{N+}_\mathbf{C}(u,v; a) = \rho_N^{+}(a)}
of a copula \eqn{\mathbf{C}(u,v)} (Joe, 2014, p. 73). The semi-correlations are defined as
\deqn{\rho_N^{-}(a) = \mathrm{cor}[Z_1, Z_2|Z_1 > a, Z_2 > a]\mbox{,}}
\deqn{\rho_N^{+}(a) = \mathrm{cor}[Z_1, Z_2|Z_1 < a, Z_2 < a]\mbox{,\ and}}
\deqn{\rho_N(a > -\infty) = \mathrm{cor}[Z_1, Z_2]\mbox{,}}
where \eqn{\mathrm{cor[z_1, z_2]}} is the familiar \emph{Pearson's correlation} function, which is in \R the syntax \code{cor(...,} \code{method="pearson")}, parameter \eqn{a \ge 0} is a truncation point that identifies \emph{truncated tail regions} (Joe, 2014, p. 73), and lastly \eqn{(Z_1, Z_2) \sim \mathbf{C}(\Phi, \Phi)} and thus from the standard normal distribution \eqn{(Z_1, Z_2) = (\Phi^{-1}(u), \Phi^{-1}(v))} where the random variables \eqn{(U,V) \sim \mathbf{C}}.
}
\usage{
semicorCOP(cop=NULL, para=NULL, truncation=0, n=0, samcor=FALSE, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{truncation}{The truncation value for \eqn{a};}
  \item{n}{The sample size \eqn{n} for simulation estimates of the \eqn{\rho_N};}
  \item{samcor}{A logical controlling whether an optional \code{data.frame} in \code{para} is used to compute the \eqn{\hat\rho_N} (see \bold{Note}); and}
  \item{...}{Additional arguments to pass to the copula.}
}
\value{
  The value(s) for \eqn{\rho_N^{-}} and \eqn{\rho_N^{+}} are returned.
}
\note{
The sample semi-correlations can be computed from a two column table that is passed into the function using the \code{para} argument. Although the truncation point \eqn{a \ge 0}, as \eqn{a} increases and focus is increasingly made into one or the other truncated tail regions, the sample version with data would not be estimated as well because the available sample size diminishes. The \code{para} argument can contain probabilities or raw data because internally the function computes the \emph{Hazen plotting positions} (\emph{e.g.} \eqn{u_i = (i-0.5)/n} for rank \eqn{i} and sample size \eqn{n}) because Joe (2014, pp. 9, 17, 245, 247--248) repeatedly emphasizes this form of plotting position when normal scores are involved.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{giniCOP}}, \code{\link{rhoCOP}}, \code{\link{tauCOP}}
}
\examples{
\dontrun{
semicorCOP(cop=PSP, n=50)$minus.semicor # with large sampling variability!}

\dontrun{
set.seed(45)
semicorCOP(cop=PLACKETTcop, para=3.755, n=100000)
# The function returns:    rhoN = 0.397, rhoN-=0.191, rhoN+=0.184
# Joe (2014, p. 72) shows: rhoN = 0.4  , rhoN-=0.189, rhoN+=0.189

set.seed(45)
semicorCOP(cop=PLACKETTcop, para=8.543, n=100000)
# The function returns:    rhoN = 0.597, rhoN-=0.373, rhoN+=0.373
# Joe (2014, p. 72) shows: rhoN = 0.6  , rhoN-=0.371, rhoN+=0.371}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{copula (tail characteristics)}
\keyword{copula (tail properties)}
\keyword{semi-correlation coefficient}
\concept{semi-correlation}
\keyword{Joe (2014) Examples and Exercises}
