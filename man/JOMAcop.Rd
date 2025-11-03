\encoding{utf8}
\name{JOMAcop}
\alias{JOMAcop}
\title{The Joe--Ma Copula}
\description{
The \emph{Joe--Ma copula} (Joe, 2014, p. 177), crediting Joe and Ma (2000), is

\deqn{\mathbf{C}_{\Theta}(u,v) = \mathbf{JOMA}(u,v) =
1 - F_{\Gamma}\biggl(\biggl\{
\bigl[F_\Gamma^{(-1)}(1-u; \Theta)\bigr]^\Theta +
\bigl[F_\Gamma^{(-1)}(1-v; \Theta)\bigr]^\Theta
              \biggr\}^{(1/\Theta)}; \Theta\biggr)\mbox{,}
}

where \eqn{F_\Gamma(x;\Theta)} is the \emph{cumulative distribution function} of the \emph{Gamma} distribution, \eqn{F_\Gamma^{(-1)}(f;\Theta)} is the \emph{quantile function} of the Gamma distribution, and \eqn{\Theta \in (-1, +1)}, which is a different parameter range than shown in Joe (2014, p. 177) because of numerical difficulties with the Gamma distribution and functional performance in context of \pkg{copBasic} design. The copula limits, as \eqn{\Theta \rightarrow -1}, to the \emph{countermonotonicity coupla} (\eqn{\mathbf{W}(u,v)}; \code{\link{W}}), as \eqn{\Theta \rightarrow 1}, to the \emph{comonotonicity copula} (\eqn{\mathbf{M}(u,v)}; \code{\link{M}}), and as \eqn{\Theta \rightarrow 0^{\pm}}, to the \emph{independence copula} (\eqn{\mathbf{\Pi}(u,v)}; \code{\link{P}}), and as \eqn{\Theta \rightarrow +\infty}. The parameter \eqn{\Theta} is readily computed from \emph{Kendall Tau} (\code{\link{tauCOP}}) or \emph{Spearman Rho} (\code{\link{rhoCOP}}) by root-solving methods. Because the formulation here uses a differing parameter range than shown in Joe (2014), integral formulations for Kendall Tau are not shown in this documentation.
}
\usage{
JOMAcop(u, v, para=NULL, rhotau=NULL,
              cortype=c("kendall", "spearman", "tau", "rho"), ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (single element) of parameters---the \eqn{\Theta} parameter of the copula;}
  \item{rhotau}{Optional Kendall Tau or Spearman Rho and parameter \code{para} is returned depending on the setting of \code{cortype}. The \code{u} and \code{v} can be used for estimation of the parameter as computed through the setting of \code{cortype};}
  \item{cortype}{A character string controlling, if the parameter is not given, to use a Kendall Tau or Spearman Rho for estimation of the parameter. The name of this argument is reflective of an internal call to \code{stats::cor()} to the correlation (association) setting for Kendall Tau or Spearman Rho; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned. Otherwise if \code{tau} is given, then the \eqn{\Theta} is computed and a \code{list} having
  \item{para}{The parameter \eqn{\Theta}, and}
  \item{tau}{Kendall Tau.}
and if \code{para=NULL} and \code{tau=NULL}, then the values within \code{u} and \code{v} are used to compute Kendall Tau and then compute the parameter, and these are returned in the aforementioned list. Or if \code{rho} is given, then the \eqn{\Theta} is computed and a similar \code{list} is returned having similar structure but with Spearman Rho instead.
}
\note{
The implementation of the copula here makes use of the negative association part of Joe's parameterization and rotates the copula to positive association. Experiments show Gamma distribution limitations that are not symmetrical to the problem at hand as the shape parameter in the distribution gets large. The negative association part of the copula makes use of conversion to Joe (2014, p. 179) formula for the \emph{lower bounds} of the copula. Internally, the parameter range is converted to \eqn{\Theta \in (-1, +1)} and therefore \eqn{\Theta = 0} is in the middle and independence. There are two thresholds for absolute value of the transformed \eqn{\Theta} in the source code and these stem from estimation of where the Gamma distribution simply breaks down. The benefit of the parameterization for this copula is we get near complete comprehensiveness that is symmetrical in Kendall Tau or Spearman Rho, which does seem likely in the \R source code that Joe (2014) has accompanying that book.

\bold{Remark}, because reflection of \eqn{1-v} is used for the construction of the positive association, for \eqn{0 < \Theta \le +1}, the copula is the copula of \eqn{(u, 1-v)}; care will be needed in some situations to work with this fact. Also, though the formula above suggests \emph{permutation symmetry} between \eqn{(u,v)} and \eqn{(v,u)}, simulations shown some tendency for skewing off the diagonal of the association. This is currently thought to be tied to the numerical derivative direction used in \pkg{copBasic} simulation; counter point is the following recipe:

\preformatted{
  UV <- simCOP(2000, cop=JOMAcop, para=-0.9, seed=1) # appears to
  # "cry" !!!! towards the upper-right corner and this is the native
  # formulation because the Theta is less than 0. Let us then have a
  # check of permutation symmetry through all four rotations.
  isCOP.permsym(cop=COP, para=list(cop=JOMAcop, para=-0.9, reflect=1))
  # [1] TRUE
  isCOP.permsym(cop=COP, para=list(cop=JOMAcop, para=-0.9, reflect=2))
  # [1] TRUE
  isCOP.permsym(cop=COP, para=list(cop=JOMAcop, para=-0.9, reflect=3))
  # [1] FALSE
  isCOP.permsym(cop=COP, para=list(cop=JOMAcop, para=-0.9, reflect=4))
  # [1] FALSE
  isCOP.permsym(cop=COP, para=list(cop=JOMAcop, para=+0.9, reflect=1))
  # [1] FALSE
  isCOP.permsym(cop=COP, para=list(cop=JOMAcop, para=+0.9, reflect=2))
  # [1] FALSE
  isCOP.permsym(cop=COP, para=list(cop=JOMAcop, para=+0.9, reflect=3))
  # [1] TRUE
  isCOP.permsym(cop=COP, para=list(cop=JOMAcop, para=+0.9, reflect=4))
  # [1] TRUE
}

All this tells us is that when the principle direction of the points are along the primary diagonal from lower left to upper right that variables are not exchangeable and hence there appears some directionality at the Gamma distribution itself within the what seems to be a symmetrical formula undoubtedly for the native form. Is this caused by the skewness itself of the Gamma? Will this be a source of L-coskewness of this copula implementation relative to \emph{Gaussian} (\code{\link{NORMcop}}) or \emph{Frank} (\code{\link{FRcop}}) copulas? Lastly, consult the following
\preformatted{
  lcomCOP(cop=JOMAcop, para=-0.6)
  lcomCOP(cop=FRcop, para=FRcop(rhotau=tauCOP(cop=JOMAcop, para=-0.6))$para)
}
that shows us that the two copula for the same Kendall Tau have differing L-comoments of course but that the Frank as L-coskews of zero and the Joe--Ma does not. One more check is now made to provide more informative about the diagonal and symmetry or not:
\preformatted{
  d <- -0.9; t <- seq(0.001, 0.999, by=0.001)
  UV <- simCOP(1000, cop=JOMAcop, para=d) # "crying" towards the upper right
  abline(1, -1, col="red") # corner, so not symmetrical about the -1 diagonal.
  u <- 0.5; der <- derCOP(cop=JOMAcop, u, t, para=-0.9, derdir="left")
  plot(qnorm(t), qnorm(der), type="l", )
}
that tells us that a vertical second line at \eqn{u = c} has a longer tail above the median and hence confirming that the Gamma distribution involvement must be producing the asymmetry.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Joe, H., and Ma, C., 2000, Multivariate survival functions with a min-stable property: Journal of Multivariate Analyses, v. 75, no. 1, pp. 13--35, \doi{https://doi.org/10.1006/jmva.1999.1891}.
}
\author{W.H. Asquith}
\seealso{\code{\link{M}}, \code{\link{P}}, \code{\link{W}}
}
\examples{
JOMAcop(rhotau=0.7, method="kendall")$para # 0.8645555

\dontrun{
  n <- 100 # Demonstrate "comprehensiveness" for the copula
  d <- sort( sample(c(-1,1), n, replace=TRUE) * 10^runif(n, min=-0.5, max=0) )
  for(i in 1:n) {
    UV <- simCOP(100, cop=JOMAcop, para=d[i]); mtext(d[i])
  } # }

\dontrun{
  # Close to maximum L-comoments and sample counterparts for general check
  # against the postive and negative aspects and signs. It is interesting
  # that there are some larger than anticipated differences with some of
  # of the sample to the theory. Perhaps numerical improvements are needed?
  UVp <- simCOP(100000, cop=JOMAcop, para=+0.6, graphics=FALSE, seed=1)
  UVn <- simCOP(100000, cop=JOMAcop, para=-0.6, graphics=FALSE, seed=1)
  #                                Lcomom:T3[12,21] Lcomom:T4[12,21]
  lcomCOP(cop=JOMAcop, para=+0.6)  #    0.014081858      0.014592670
  #                                #   -0.014081858      0.014592670
  lcomCOP(cop=JOMAcop, para=-0.6)  #    0.014081858     -0.014592670
  #                                #    0.014081858     -0.014592670
  lmomco::lcomoms2(UVp, nmom=4) #$T3    0.0155001646     0.0161519394
  #                                #   -0.0145945494     0.0148050507
  lmomco::lcomoms2(UVn, nmom=4) #$T4    0.010777365      0.0143115158
  #                                #   -0.0168424348    -0.0131075545 # }

\dontrun{
  # A quick look at L-comoments of the copula with some remarks about limits.
  # The numerical intergrations for the L-comoments "diverage" for
  #   T3.12 NAs begin at -0.988, 0.987
  #   T3.21 NAs begin at -0.988, -----
  #   T4.12 NAs begin at -0.988, -----
  #   T4.12 NAs begin at -0.988, -----
  d <- seq(-1, 1, by=0.001)
  t3.12 <- t4.12 <- rep(NA, length(d))
  t3.21 <- t4.21 <- rep(NA, length(d))
  for(i in seq_len(length(d))) {
    lcm <- lcomCOP(cop=JOMAcop, para=d[i])
    t3.12[i] <- lcm$lcomUV[3]; t4.12[i] <- lcm$lcomUV[4]
    t3.21[i] <- lcm$lcomVU[3]; t4.21[i] <- lcm$lcomVU[4]
  }
  plot(d,  t3.12, col="skyblue4", type="l", lwd=3, ylim=c(-0.02, +0.02),
       xlab="JOMAP parameter", ylab="L-coskew and L-kurtosis")
  lines(d, t4.12, col="skyblue4", lty=2,    lwd=3)
  lines(d, t3.21, col="red")
  lines(d, t4.21, col="red",      lty=2) # }
}
\keyword{Joe-Ma copula}
\keyword{copula (formulas)}
\keyword{copula}
\keyword{copula (comprehensive)}
\concept{comprehensive copula}
