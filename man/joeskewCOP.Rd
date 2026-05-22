\encoding{utf8}
\name{joeskewCOP}
\alias{joeskewCOP}
\alias{nuskewCOP}
\alias{nustarCOP}
\title{Joe's Nu-Skew and the copBasic Nu-Star of a Copula}
\description{
Compute the measure of \emph{permutation asymmetry}, which can be thought of as \emph{bivariate skewness}, named for the \pkg{copBasic} package as \emph{Nu-Skew} \eqn{\nu_\mathbf{C}} of a copula according to Joe (2014, p. 66) by
\deqn{\nu_\mathbf{C} = 3\mathrm{E}[UV^2 - U^2V] = 6\int\!\!\int_{\mathcal{I}^2} (v-u)\mathbf{C}(u,v)\, \mathrm{d}u\mathrm{d}v\mbox{.}}
This definition is effectively the \code{type="nu"} for the function for which the multiplier \eqn{6} has been converted to \eqn{96} as explained in the \bold{Note}.

Numerical results indicate \eqn{\nu_\mathbf{W} \approx 0} (\code{\link{W}}), \eqn{\nu_\mathbf{\Pi} = 0} (\code{\link{P}}), \eqn{\nu_\mathbf{M} \approx 0} (\code{\link{M}}), \eqn{\nu_\mathbf{PL} \approx 0} for all \eqn{\Theta} (\code{\link{PLcop}}), and the \eqn{\nu^\star_\mathbf{GH} = 0} (\code{\link{GHcop}}); copulas with mirror symmetry across the equal value line have \eqn{\nu_\mathbf{C} = 0}.

Asymmetric copulas do exist. For example, consider an asymmetric Gumbel--Hougaard \eqn{\mathbf{GH}} copula with \eqn{\Theta_p = (5,0.8,p)}:
\preformatted{
  optimize(function(p) { nuskewCOP(cop=GHcop, para=c(5,0.8, p)) },
           c(0,0.99) )$minimum
  UV <- simCOP(n=10000, cop=GHcop, c(5,0.8, 0.2836485)) # inspect the graphics
  48*mean(UV$U*$V^2 - UV$U^2*UV$V) # -0.2847953 (not the 3rd parameter)
}
The minimization yields \eqn{\nu_{\mathbf{GH}(5, 0.8, 0.2836485)} = -0.2796104}, which is close the expectation computed where \eqn{48 = 96/2}.

A complementary definition is supported, triggered by \code{type="nustar"}, and is computed by
\deqn{\nu^\star_\mathbf{C} = 12\int\!\!\int_{\mathcal{I}^2} (v+u)\mathbf{C}(u,v)\, \mathrm{d}u\mathrm{d}v - 4\mbox{,}}
which has been for the \pkg{copBasic} package, \eqn{\nu^\star_\mathbf{C}} is named as \emph{Nu-Star}, which the \eqn{12} and the \eqn{-4} have been chosen so that numerical results indicate \eqn{\nu^\star_\mathbf{W} = -1} (\code{\link{W}}), \eqn{\nu^\star_\mathbf{\Pi} = 0} (\code{\link{P}}), and \eqn{\nu^\star_\mathbf{M} = +1} (\code{\link{M}}).

Lastly, the \code{\link{uvlmoms}} function provides for a quantile-based measure of bivariate skewness based on the difference \eqn{U - V} that also is discussed by Joe (2014, p. 66).
}
\usage{
joeskewCOP(cop=NULL, para=NULL, type=c("nu", "nustar", "nuskew"),
                               as.sample=FALSE, brute=FALSE, delta=0.002, ...)

nuskewCOP(cop=NULL, para=NULL, as.sample=FALSE, brute=FALSE, delta=0.002, ...)
nustarCOP(cop=NULL, para=NULL, as.sample=FALSE, brute=FALSE, delta=0.002, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{type}{The type of metric to compute (\code{nu} and \code{nuskew} are synonymous for \eqn{\nu_\mathbf{C}} and \code{nustar} is for \eqn{\nu^\star_\mathbf{C}});}
  \item{brute}{Should brute force be used instead of two nested \code{integrate()} functions to perform the double integration;}
  \item{delta}{The \eqn{\mathrm{d}u} and \eqn{\mathrm{d}v} for the brute force integration using \code{brute};}
  \item{as.sample}{A logical controlling whether an optional \R \code{data.frame} in \code{para} is used to compute the sample \eqn{\hat\nu} or \eqn{\hat\nu^\star} (see \bold{Note}). If set to \code{-1}, then the message concerning CPU effort will be surpressed; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for \eqn{\nu_\mathbf{C}} or \eqn{\nu^\star_\mathbf{C}} is returned.
}
\details{
The implementation of \code{joeskewCOP} for \pkg{copBasic} provides the second metric of asymmetry, but why?  Consider the results that follow:
\preformatted{
  joeskewCOP(cop=GHcop, para=c(5, 0.8,    0.2836485), type="nu")
     # -0.2796104
  joeskewCOP(cop=GHcop, para=c(5, 0.2836485,    0.8), type="nu")
     # +0.2796103
  joeskewCOP(cop=GHcop, para=c(5, 0.8,    0.2836485), type="nu")
     #  0.3571276
  joeskewCOP(cop=GHcop, para=c(5, 0.2836485,    0.8), type="nu")
     #  0.3571279
  tauCOP(    cop=GHcop, para=c(5, 0.2836485,    0.8))
     #  0.2443377
}
The demonstration shows---at least for the symmetry (switchability) of the 2nd and 3rd parameters (\eqn{\pi_2} and \eqn{\pi_3}) of the asymmetric \eqn{\mathbf{GH}} copula---that the first definition \eqn{\nu} is magnitude symmetric but carries a sign change. The demonstration shows magnitude and sign stability for \eqn{\nu^\star}, and ends with \emph{Kendall Tau} (\code{\link{tauCOP}}). Collectively, Kendall Tau (or the other \emph{symmetric measures of association}, \emph{e.g.} \code{\link{blomCOP}}, \code{\link{footCOP}}, \code{\link{giniCOP}}, \code{\link{hoefCOP}}, \code{\link{rhoCOP}}, \code{\link{wolfCOP}}) when combined with \eqn{\nu} and \eqn{\nu^\star} might provide a framework for parameter optimization of the asymmetric \eqn{\mathbf{GH}} copula (see below).

The asymmetric \eqn{\mathbf{GH}_{(5, 0.2836485, 0.8)}} is not radial (\code{\link{isCOP.radsym}}) or permutation (\code{\link{isCOP.permsym}}), but if \eqn{\pi_2 = \pi_3} then the resulting \eqn{\mathbf{GH}} copula is not radially symmetric but is permutation symmetric:
\preformatted{
  isCOP.radsym( cop=GHcop, para=c(5, 0.2836485, 0.8)) # FALSE
  isCOP.permsym(cop=GHcop, para=c(5, 0.2836485, 0.8)) # FALSE
  isCOP.radsym( cop=GHcop, para=c(5, 0.8,       0.8)) # FALSE
  isCOP.permsym(cop=GHcop, para=c(5, 0.8,       0.8)) # TRUE
}

The use of \eqn{\nu_\mathbf{C}} and \eqn{\nu^\star_\mathbf{C}} with a \emph{measure of association} is just suggested above for parameter optimization. Suppose we have \eqn{\mathbf{GH}_{(5,0.5,0.7)}} with \emph{Spearman Rho} \eqn{\rho = 0.4888}, \eqn{\nu = 0.001475}, and \eqn{\nu^\star = 0.04223}, and the asymmetric \eqn{\mathbf{GH}} coupla is to be fit. Parameter estimation for the asymmetric \eqn{\mathbf{GH}} is accomplished by
\preformatted{
  "fitGHcop" <- function(hats, assocfunc=rhoCOP, init=NA, eps=1E-4, ...) {
     H <- GHcop # shorthand for the copula
     "objfunc" <- function(par) {
        par[1]   <- ifelse(par[1] < 1, return(Inf), exp(par[1])) # edge check
        par[2:3] <-  pnorm(par[2:3]) # detransform
        hp <- c(assocfunc(H, par), nuskewCOP(H, par), nustarCOP(H, par))
        return(sum((hats-hp)^2))
     }
     # Theta=1 and Pi2 = Pi3 = 1/2 # as default initial estimates
     if(is.na(init)) init <- c(1, rep(1/2, times=2))
     opt  <- optim(init, objfunc, ...); par <- opt$par
     para <- c( exp(par[1]), pnorm(par[2:3]) )
     names(para) <- c("Theta", "Pi2", "Pi3")
     fit <- c(assocfunc(H, para), nuskewCOP(H, para), nustarCOP(H, para))
     txt <- c("AssocMeasure", "NuSkew", "NuStar")
     names(fit) <- txt; names(hats) <- txt
     if(opt$value > eps) warning("inspect the fit")
     return(list(para=para, fit=fit, given=hats, optim=opt))
  }
  father <- c(5,.5,.7)
  densityCOPplot(cop=GHcop, para=father, contour.col=8)
  fRho  <- rhoCOP(   cop=GHcop, father)
  fNu   <- nuskewCOP(cop=GHcop, father)
  fStar <- nustarCOP(cop=GHcop, father)

  child <- fitGHcop(c(fRho, fNu, fStar))$para
  densityCOPplot(cop=GHcop, para=child, ploton=FALSE)

  cRho  <- rhoCOP(   cop=GHcop, child)
  cNu   <- nuskewCOP(cop=GHcop, child)
  cStar <- nustarCOP(cop=GHcop, child)
  message("Father stats: ", paste(fRho, fNu, fStar, sep=", "))
  message("Child  stats: ", paste(cRho, cNu, cStar, sep=", "))
  message("Father para: ",  paste(father,      collapse=", "))
  message("Child  para: ",  paste(child,       collapse=", "))
}

The initial parameter estimate has the value \eqn{\Theta = 1}, which is \emph{independence} for the one parameter \eqn{\mathbf{GH}}. The two other parameters are set as \eqn{\pi_2 = \pi_3 = 1/2} to be in the mid-point of their domain. The transformations using the \code{log()} \eqn{\leftrightarrow} \code{exp()} and \code{qnorm()} \eqn{\leftrightarrow} \code{pnorm()} functions in \R are used to keep the optimization in the viable parameter domain. The results produce a fitted copula of \eqn{\mathbf{GH}_{(4.907, 0.5006, 0.7014)}}. This fit aligns well with the parent, and the three statistics are essentially matched during the fitting.

The \eqn{\nu^\star_\mathbf{C}} can be similar to \code{\link{rhoCOP}}, but differences do exist. In the presence of radial symmetry, (\eqn{\nu_\mathbf{C} == 0}), the \eqn{\nu^\star_\mathbf{C}} is nearly equal to \emph{Spearman Rho} for some copulas. Let us test further:
\preformatted{
  p <- 10^seq(0,2,by=.01)
  s <- sapply(p, function(t) nustarCOP(cop=GHcop, para=c(t)))
  r <- sapply(p, function(t)    rhoCOP(cop=GHcop, para=c(t)))
  plot(p,s, log="x", type="l", col=3, lwd=3); lines(p,r)
}
Now let us add some asymmetry
\preformatted{
  s <- sapply(p, function(t) nustarCOP(cop=GHcop, para=c(t, 0.25, 0.75)))
  r <- sapply(p, function(t)    rhoCOP(cop=GHcop, para=c(t, 0.25, 0.75)))
  plot(p,s, log="x", type="l", col=3, lwd=3); lines(p,r)
}
Now let us choose a different (the \emph{Clayton}) copula
\preformatted{
  s <- sapply(p, function(t) nustarCOP(cop=CLcop, para=c(t)))
  r <- sapply(p, function(t)    rhoCOP(cop=CLcop, para=c(t)))
  plot(p,s, log="x", type="l", col=3, lwd=3); lines(p,r)
}
}
\note{
The \eqn{\nu_\mathbf{C}} definition is given with a multiplier of \eqn{6} on the integrals in order to agree with Joe (2014) relation that is also shown. However, in mutual parameter estimation experiments using a simple sum-of-square errors as shown in the \bold{Details}, it is preferred to have \eqn{\nu_\mathbf{C}} measured on a larger scale. Where does the \eqn{96} then come from? It is heuristically made so that the upright and rotated \code{cophalf} (see \bold{Examples} under \code{\link{asCOP}} and \code{\link{bilmoms}} for this copula) acquires \eqn{\nu_\mathbf{C}} values of \eqn{+1} and \eqn{-1}, respectively. As a result to make back comparisons to Joe results, the ratios of \eqn{96} are made in this documentation.

The source code shows slightly different styles of division by the sample size as part of the sample estimation of the statistics. The \eqn{\hat\nu} using just division by the sample size as testing indicates that this statistic is reasonably unbiased for simple copula. The \eqn{\hat\nu^\star} with similar division is a biased statistic and the bias is not symmetrical in magnitude or sign it seems whether the \eqn{\hat\nu^\star} is positive or negative. The salient code is \code{spm <- ifelse(corsgn == -1, +2.4, +1.1)} within the sources for which the corrections were determined heuristically through simulation, and \code{corsgn} is the sign of the sample Spearman Rho through the \code{cor()} function of \R.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{uvskew}}, \code{\link{blomCOP}}, \code{\link{footCOP}}, \code{\link{giniCOP}},
\code{\link{hoefCOP}}, \code{\link{rhoCOP}}, \code{\link{tauCOP}}, \code{\link{wolfCOP}}
}
\examples{
nuskewCOP(cop=GHcop,para=c(1.43,1/2,1))*(6/96) # 0.005886 (Joe, 2014, p. 184; 0.0059)

\dontrun{
joeskewCOP(            cop=GHcop, para=c(8, .7, .5)) # -0.1523491
joeskewCOP(            cop=GHcop, para=c(8, .5, .7)) # +0.1523472
# UV <- simCOP(n=1000, cop=GHcop, para=c(8, .7, .5)) # see the switch in
# UV <- simCOP(n=1000, cop=GHcop, para=c(8, .5, .7)) # curvature}

\dontrun{
para=c(19,0.3,0.8); set.seed(341)
nuskew <-  nuskewCOP( cop=GHcop, para=para) # 0.3057744
UV <- simCOP(n=10000, cop=GHcop, para=para) #   a large simulation
mean((UV$U - UV$V)^3)/(6/96)                # 0.3127398

# Two other definitions of skewness follow and are not numerically the same.
uvskew(u=UV$U, v=UV$V, umv=TRUE)  # 0.3738987  (see documentation uvskew)
uvskew(u=UV$U, v=UV$V, umv=FALSE) # 0.3592739  ( or documentation uvlmoms)
# Yet another definition of skew, which requires large sample approximation
# using the L-comoments (3rd L-comoment is L-coskew).
lmomco::lcomoms2(UV)$T3 # L-coskew of the simulated values [1,2] and [2,1]
#             [,1]        [,2]
#[1,]  0.007398438  0.17076600
#[2,] -0.061060260 -0.00006613
# See the asymmetry in the two L-coskew values and consider this in light of
# the graphic produced by the simCOP() called for n=10,000. The T3[1,1] is
# the sampled L-skew (univariate) of the U margin and T3[2,2] is the same
# but for the V margin. Because the margins are uniform (ideally) then these
# for suitable large sample must be zero because the L-skew of the uniform
# distribution is by definition zero.
#
# Now let us check the sample estimator for sample of size n=300, and the
# t-test will (should) result in acceptance of the NULL hypothesis.
S <- replicate(60, nuskewCOP(para=simCOP(n=300, cop=GHcop, para=para,
                                         graphics=FALSE), as.sample=TRUE))
t.test(S, mu=nuskew)
# t = 0.004633, df = 59, p-value = 0.9963
# alternative hypothesis: true mean is not equal to 0.3057744
# 95 percent confidence interval:
#  0.2854282 0.3262150
# sample estimates:
# mean of x
# 0.3058216 }

\dontrun{
# Let us run a large ensemble of copula properties that use the whole copula
# (not tail properties). We composite a Plackett with a Gumbel-Hougaard for
# which the over all association (correlation) sign is negative, but amongst
# these statistics with nuskew and nustar at the bottom, there are various
# quantities that can be extracted. These could be used for fitting.
set.seed(873)
para <- list(cop1=PLcop, cop2=GHcop, alpha=0.6, beta=0.9,
             para1=.005, para2=c(8.3,0.25,0.7))
UV <- simCOP(1000, cop=composite2COP, para=para) # just to show
  blomCOP(composite2COP, para)            # -0.4078657
  footCOP(composite2COP, para)            # -0.2854227
  hoefCOP(composite2COP, para)            # +0.5713775
  lcomCOP(composite2COP, para)$lcomUV[3]  # +0.1816084
  lcomCOP(composite2COP, para)$lcomVU[3]  # +0.1279844
   rhoCOP(composite2COP, para)            # -0.5688417
rhobevCOP(composite2COP, para)            # -0.2005210
   tauCOP(composite2COP, para)            # -0.4514693
  wolfCOP(composite2COP, para)            # +0.5691933
nustarCOP(composite2COP, para)            # -0.5172434
nuskewCOP(composite2COP, para)            # +0.0714987 }
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{permutation asymmetry}
\keyword{radial asymmetry}
\concept{copula skew}
\keyword{Moments of combined U and V}
\keyword{bivariate skewness}
\keyword{Joe (2014) Examples and Exercises}
\keyword{Nu-Skew and Nu-Star (skewnesses)}
