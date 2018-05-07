\name{joeskewCOP}
\alias{joeskewCOP}
\alias{nuskewCOP}
\alias{nustarCOP}
\title{ The Joe's Nu-Skew or Nu-Skew-Star of a Copula }
\description{
Compute the measure of \emph{permutation asymmetry} named for the \pkg{copBasic} package as \emph{Joe's Nu-Skew} \eqn{\nu_\mathbf{C}} of a copula according to Joe (2014, p. 66) by
\deqn{\nu_\mathbf{C} = 3\mathrm{E}[UV^2 - U^2V] = 6\int\!\!\int_{\mathcal{I}^2} (v-u)\mathbf{C}(u,v)\, \mathrm{d}u\mathrm{d}v\mbox{.}}
This definition is the \code{type="nu"} for the function. Permutation asymmetry can be thought of as \emph{bivariate skewness}, which is a more recognizable term/concept to many readers.

Numerical results indicate \eqn{\nu_\mathbf{W} \approx 0} (\code{\link{W}}), \eqn{\nu^\star_\mathbf{\Pi} = 0} (\code{\link{P}}), \eqn{\nu_\mathbf{M} \approx 0} (\code{\link{M}}), \eqn{\nu^\star_\mathbf{PL} \approx 0} for all \eqn{\Theta} (\code{\link{PLACKETTcop}}), and the \eqn{\nu^\star_\mathbf{GH} = 0} (\code{\link{GHcop}}); copulas with mirror symmetry across the 1:1 line have \eqn{\nu_\mathbf{C} = 0}. Asymmetric copulas do exist. Consider an asymmetric Gumbel--Hougaard \eqn{\mathbf{GH}} with \eqn{\Theta_p = (5,0.8,p)}
\preformatted{
  optimize(function(p) { joeskewCOP(cop=GHcop, para=c(5,0.8, p), type="nu") },
           c(0,0.99) )$minimum
  UV <- simCOP(n=1000, cop=GHcop, c(5,0.8, 0.2836)) # inspect the graphics
}
then minimization yields \eqn{\nu_{\mathbf{GH}(5, 0.8, 0.2836)} = -0.0175}.

Another defintion supported here is for \code{type="nustar"} and is computed by
\deqn{\nu^\star_\mathbf{C} = 6\int\!\!\int_{\mathcal{I}^2} (v+u-1)\mathbf{C}(u,v)\, \mathrm{d}u\mathrm{d}v - \frac{1}{2}\mbox{.}}
For the \pkg{copBasic} package, \eqn{\nu^\star_\mathbf{C}} is named as \emph{Joe's Nu-Skew-Star}.

Numerical results indicate \eqn{\nu^\star_\mathbf{W} \approx 0} (\code{\link{W}}), \eqn{\nu^\star_\mathbf{\Pi} = 0} (\code{\link{P}}), \eqn{\nu^\star_\mathbf{M} \approx 0} (\code{\link{M}}), \eqn{\nu^\star_\mathbf{PL} \approx 0} for all \eqn{\Theta} (\code{\link{PLACKETTcop}}), and for comparison the \eqn{\nu^\star_{\mathbf{GH}(1.646)} = 0.01173} (\code{\link{GHcop}}), which is the maximum attained by the symmetrical \eqn{\mathbf{GH}} copula for \eqn{\Theta = 1.646}:
\preformatted{
  optimize(function(p) {
         -1 * joeskewCOP(cop=GHcop, para=p, type="nustar")}, c(1,40) )$minimum
}

Lastly, the \code{\link{uvlmoms}} function provides for a quantile-based measure of bivariate skewness based on the difference \eqn{U - V} that also is discussed by Joe (2014, p. 66).
}
\usage{
joeskewCOP(cop=NULL, para=NULL, type=c("nu", "nustar"), as.sample=FALSE,
                                                brute=FALSE, delta=0.002, ...)

nuskewCOP(cop=NULL, para=NULL, as.sample=FALSE, brute=FALSE, delta=0.002, ...)
nustarCOP(cop=NULL, para=NULL, as.sample=FALSE, brute=FALSE, delta=0.002, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{type}{The type of metric to compute;}
  \item{brute}{Should brute force be used instead of two nested \code{integrate()} functions to perform the double integration;}
  \item{delta}{The \eqn{\mathrm{d}u} and \eqn{\mathrm{d}v} for the brute force integration using \code{brute};}
  \item{as.sample}{A logical controlling whether an optional \R \code{data.frame} in \code{para} is used to compute the \eqn{\hat\nu} (see \bold{Note}). If set to \code{-1}, then the message concerning CPU effort will be surpressed; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for \eqn{\nu_\mathbf{C}} is returned.
}
\details{
The implementation of \code{joeskewCOP} for \pkg{copBasic} provides the second definition of asymmetry, but why?  Consider the results that follow:
\preformatted{
  joeskewCOP(cop=GHcop, para=c(5, 0.8,    0.2836))
     # -0.01747565
  joeskewCOP(cop=GHcop, para=c(5, 0.2836,  0.8  ))
     #  0.01747564
  joeskewCOP(cop=GHcop, para=c(5, 0.8,    0.2836), type="nustar")
     #  0.00927368
  joeskewCOP(cop=GHcop, para=c(5, 0.2836, 0.8   ), type="nustar")
     #  0.009273612
  tauCOP(    cop=GHcop, para=c(5, 0.2836, 0.8   ))
     #  0.2443013
}
The demonstration shows---at least for the symmetry (switchability) of the 2nd and 3rd parameters (\eqn{\pi_2} and \eqn{\pi_3}) of the asymmetric \eqn{\mathbf{GH}} copula---that the first definition \eqn{\nu} is magnitude symmetric but carries a sign change. The demonstration show magnitude and sign stablity for \eqn{\nu^\star}, and ends with \emph{Kendall's Tau} (\code{\link{tauCOP}}). Collectively, Kendall's Tau (or the other \emph{symmetric measures of association}, \emph{e.g.} \code{\link{blomCOP}}, \code{\link{footCOP}}, \code{\link{giniCOP}}, \code{\link{hoefCOP}}, \code{\link{rhoCOP}}, \code{\link{wolfCOP}}) when combined with \eqn{\nu} and \eqn{\nu^\star} might provide a framework for parameter optimization of the asymmetric \eqn{\mathbf{GH}} copula (see below).

The asymmetric \eqn{\mathbf{GH}_{(5, 0.2836, 0.8)}} is not radial (\code{\link{isCOP.radsym}}) or permutation (\code{\link{isCOP.permsym}}), but if \eqn{\pi_2 = \pi_3} then the resulting \eqn{\mathbf{GH}} copula is not radially symmetric but is permutation symmetric:
\preformatted{
  isCOP.radsym( cop=GHcop, para=c(5, 0.2836, 0.8)) # FALSE
  isCOP.permsym(cop=GHcop, para=c(5, 0.2836, 0.8)) # FALSE
  isCOP.radsym( cop=GHcop, para=c(5, 0.8,    0.8)) # FALSE
  isCOP.permsym(cop=GHcop, para=c(5, 0.8,    0.8)) # TRUE
}

The use of \eqn{\nu_\mathbf{C}} and \eqn{\nu^\star_\mathbf{C}} with a \emph{measure of association} is just suggested above for parameter optimization. Suppose we have a sample \emph{Spearman's Rho} \eqn{\hat\rho = 1/3}, \eqn{\hat\nu = 6/1000}, and \eqn{\hat\nu^\star = 8/1000} and the asymmetric \eqn{\mathbf{GH}} coupla is to be fit. Parameter estimation for the asymmetric \eqn{\mathbf{GH}} can be accomplished by the following code:
\preformatted{
  "fitGHcop" <- function(hats, assocfunc=rhoCOP, init=NA, eps=1E-4, ...) {
     H <- GHcop # shorthand for the copula
     "objfunc" <- function(par) {
        par[1]   <- ifelse(par[1] < 1, return(Inf), exp(par[1])) # edge check
        par[2:3] <-  pnorm(par[2:3]) # detransform
        hp <- c(assocfunc(H, par), nuskewCOP(H, par), nustarCOP(H, par))
        dv <- hats; dv[dv == 0] <- 1 # changing dv "adapts" the error to
        return(sqrt(sum(((hats-hp)/dv)^2))) # trap division by zero
     }
     # Theta=1 and Pi2 = Pi3 = 1/2 # as default initial estimates
     if(is.na(init)) init <- c(1, rep(1/2, times=2))
     opt  <- optim(init, objfunc, ...); par <- opt$par
     para <- c( exp(par[1]), pnorm(par[2:3]) )
     names(para) <- c("Theta", "Pi2", "Pi3")
     fit <- c(assocfunc(H, para), nuskewCOP(H, para), nustarCOP(H, para))
     txt <- c("AssocMeasure", "JoeNuSkew", "JoeNuSkewStar")
     names(fit) <- txt; names(hats) <- txt
     if(opt$value > eps) warning("inspect the fit")
     return(list(para=para, fit=fit, given=hats, optim=opt))
  }
  Rho <- 1/3; Nu <- +6/1000; NuStar <- +8/1000
  para <- fitGHcop(c(Rho, Nu, NuStar))$para
  densityCOPplot(cop=GHcop, para=para) # inspect the graphics
}

The initial parameter estimate has the value \eqn{\Theta = 1}, which is \emph{independence} for the one parameter \eqn{\mathbf{GH}}. The two other parameters are set as \eqn{\pi_2 = \pi_3 = 1/2} to be in the mid-point of their domain. The transformations using the \code{log()} \eqn{\leftrightarrow} \code{exp()} and \code{qnorm()} \eqn{\leftrightarrow} \code{pnorm()} functions in \R are used to keep the optimization in the viable parameter domain. The results produce a fitted copula of \eqn{\mathbf{GH}_{(3.865, 0.3634, 0.5197)}}.

Although with much brevity, the \code{fitGHcop()} function is an effective solution for the \eqn{\mathbf{GH}}. The function handles alternative measures of association, alternative initial parameters, honors the bounds of the three parameters, and can accommodate zero values for any of the statistics to fit---the question then is, does the \eqn{\mathbf{GH}} truly itself have such solutions as suggested by the input target statistics?  Certainly, \eqn{\nu = 0} is possible because the estimated \eqn{\pi_{1,2}} are returned as equal if \code{Nu = 0}.
}
\note{
A natural estimator for \eqn{\hat{\nu}_\mathbf{C}} is through the \emph{empirical copula}
\deqn{\hat{\nu}_\mathbf{C} = \frac{6}{n^2 - 1}
        \sum_{i=1}^n\sum_{j=1}^n \biggl[\frac{(j-i)}{n}\mathbf{C}_n\biggl(\frac{i}{n}, \frac{j}{n}\biggr)\biggr]\mbox{,}}
where \eqn{\hat{\mathbf{C}}_n} is the simplest empirical copula. This definition is the \code{type="V-U"} for the function. Another sample defintion is supported for \code{type="nustar"} is computed by
\deqn{\hat{\nu}^\star_\mathbf{C} = \frac{6}{n^2 - 1}
        \sum_{i=1}^n\sum_{j=1}^n \biggl[\frac{(j+i-n)}{n}\mathbf{C}_n\biggl(\frac{i}{n}, \frac{j}{n}\biggr)\biggr] - \frac{1}{2}\mbox{.}}
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{ W.H. Asquith}
\seealso{
\code{\link{uvskew}}, \code{\link{blomCOP}}, \code{\link{footCOP}}, \code{\link{giniCOP}},
\code{\link{hoefCOP}}, \code{\link{rhoCOP}}, \code{\link{tauCOP}}, \code{\link{wolfCOP}}
}
\examples{
nuskewCOP(cop=GHcop,para=c(1.43,1/2,1)) # 0.005886 (Joe, 2014, p. 184; 0.0059)

\dontrun{
joeskewCOP(            cop=GHcop, para=c(8, .7, .5)) # -0.009521820
joeskewCOP(            cop=GHcop, para=c(8, .5, .7)) #  0.009521699
# UV <- simCOP(n=1000, cop=GHcop, para=c(8, .7, .5)) # see the switch in
# UV <- simCOP(n=1000, cop=GHcop, para=c(8, .5, .7)) # curvature}

\dontrun{
para=c(19,0.3,0.8); set.seed(341)
nuskew <- joeskewCOP( cop=GHcop, para=para) # 0.01911090
UV <- simCOP(n=10000, cop=GHcop, para=para) #   a large simulation
mean((UV$U - UV$V)^3)                       # 0.01954624
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
S <- replicate(60, joeskewCOP(para=simCOP(n=300, cop=GHcop, para=para,
                                          graphics=FALSE), as.sample=TRUE))
t.test(S, mu=nuskew)
#         One Sample t-test
# t = -0.074863, df = 39, p-value = 0.9407
# alternative hypothesis: true mean is not equal to 0.0191109
# 95 percent confidence interval:
#  0.01713283    0.02094776
# sample estimates:
#  mean of x
#         0.0190403 }
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{permutation asymmetry}
\keyword{radial asymmetry}
\concept{copula skew}
\keyword{Moments of combined U and V}
\keyword{bivariate skewness}
\keyword{Joe (2014) Examples and Exercises}
