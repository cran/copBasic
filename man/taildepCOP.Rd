\encoding{utf8}
\name{taildepCOP}
\alias{taildepCOP}
\title{The Lower- and Upper-Tail Dependency Parameters of a Copula}
\description{
Compute the \emph{lower-} and \emph{upper-tail dependency parameters} (if they exist), respectively, of a copula according to Nelsen (2006, pp. 214--215). Graphical confirmation of the computations is important, and therefore, the function can also generate a plot. The dependency parameters are expressions of conditional probability that \eqn{Y} is greater than the \eqn{100{\times}}th percentile of its distribution \eqn{G} given that \eqn{X} is greater than the \eqn{100{\times}t}-th percentile of its distribution \eqn{F} as \eqn{t} approaches unity. Specifics in terms of quantile functions \eqn{G^{(-1)}(t) = y(t)} and \eqn{F^{(-1)}(t) = x(t)} follow.

The \emph{lower-tail dependence parameter} \eqn{\lambda^L_\mathbf{C}} is defined as
\deqn{\lambda^L_\mathbf{C} = \lim_{t{\rightarrow 0^{+}}} \mathrm{Pr}[Y \le y(t)\mid X \le x(t)]\mbox{, and}}
the \emph{upper-tail dependence parameter} \eqn{\lambda^U_\mathbf{C}} with reversed inequalities is defined as
\deqn{\lambda^U_\mathbf{C} = \lim_{t{\rightarrow 1^{-}}} \mathrm{Pr}[Y > y(t)\mid X > x(t)]\mbox{.}}


Nelsen (2006, p. 214) also notes that both \eqn{\lambda^L_\mathbf{C}} and \eqn{\lambda^U_\mathbf{C}} are nonparametric and depend only on the copula of \eqn{X} and \eqn{Y}, and Nelsen shows that each can be computed if the above limits exist as follows:
\deqn{\lambda^L_\mathbf{C} = \lim_{t{\rightarrow 0^{+}}} \frac{\mathbf{C}(t,t)}{t} = \delta_\mathbf{C}'(0^{+})\mbox{\ and}}
\deqn{\lambda^U_\mathbf{C} = \lim_{t{\rightarrow 1^{-}}} \frac{1 - 2t - \mathbf{C}(t,t)}{1 - t} = 2 - \lim_{t{\rightarrow 1^{-}}} \frac{1 - \mathbf{C}(t,t)}{1-t} = 2 - \delta_\mathbf{C}'(1^{-})\mbox{,}}
where \eqn{\delta_\mathbf{C}'(t)} is the derivative of the diagonal of the copula. Multiple presentations are shown because algebraic variants are shown across the literature.

If \eqn{\lambda^L_\mathbf{C} \in (0,1]}, then \eqn{\mathbf{C}} has lower-tail dependence but if \eqn{\lambda^L_\mathbf{C} = 0}, then \eqn{\mathbf{C}} has \emph{no} lower-tail dependence. Likewise, if \eqn{\lambda^U_\mathbf{C} \in (0,1]}, then \eqn{\mathbf{C}} has upper-tail dependence but if \eqn{\lambda^U_\mathbf{C} = 0}, then \eqn{\mathbf{C}} has \emph{no} upper-tail dependence.
}
\usage{
taildepCOP(cop=NULL, para=NULL, tol=1e-6, divisor=2, plot=FALSE, ylim=NULL,
                                verbose=FALSE, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{tol}{A tolerance on convergence;}
  \item{divisor}{The divisor on the incremental reductions towards \eqn{0^+} and \eqn{0^-} by the algorithm;}
  \item{plot}{A logical plotting a diagnostic plot of the diagonal derivatives and label the limits;}
  \item{ylim}{Optional vertical limits if the plot is turned on. Although the dependence parameters are bounded as described above, numerical stability can be a problem. Stability is especially a problem if an empirical copula is being used; theefore, the bounds of the plot are left open unless the user locks them down with this argument;}
  \item{verbose}{Show incremental progress; and}
  \item{...}{Additional arguments to pass to the copula function.}
}
\value{
  An \R list is returned.
  \item{lambdaL}{The rounded value of \eqn{\lambda^L_\mathbf{C}};}
  \item{lambdaU}{The rounded value of \eqn{\lambda^U_\mathbf{C}};}
  \item{source}{An attribute identifying the computational source: \dQuote{taildepCOP}.}
}
\note{
\emph{IMPLEMENTED ALGORITHM}---The algorithm implemented for \code{taildepCOP} is based on halves (or alternatives by the setting of \code{divisor} argument) and uses the copula function (not an analytical or even numeric derivative of the diagonal, \eqn{\delta_\mathbf{C}'(t)}). Starting from the median or \eqn{t = 0.5}, each limit is respectively computed by successive halving (or the setting of argument \code{divisor}) of the distance towards \eqn{0^{+}} and \eqn{1^{-}} and checking the change in computed value against the tolerance \code{tol} argument. After the change becomes less than the tolerance, convergence is assumed. Other tests are made for \code{NaN} to aid in breaking the successive halvings. The rounding for the numerical results for \eqn{\lambda^U_\mathbf{C}} and \eqn{\lambda^L_\mathbf{C}} is an order of magnitude larger than the tolerance.

Users are encouraged to plot the results and further verify whether the convergence makes sense. The plot produced when \code{plot=TRUE} shows the probability \eqn{t} transformed into standard normal variates by the \code{qnorm()} function in \R so that the distal reaches of each tail and thus limit are readily seen. The terminal points of each limit computation are shown by a small dot and the letter \dQuote{L} and \dQuote{U} also are plotted at the terminal points.


Joe (2014, p. 63) reports that \dQuote{the empirical measure of tail dependence [\eqn{\hat\lambda_\mathbf{C}^L} or \eqn{\hat\lambda_\mathbf{C}^U}] for data does not really exist because of the limit.}  Joe (2014) suggests that other sources in the literature are the \dQuote{best that can be done} (\enc{Dobrić}{Dobric} and Schmid, 2005; Frahm \emph{et al.}, 2005). Another source of discussion is by Schmidt and \enc{Stadtmüller}{Stadtmuller} (2006). The results therein are not yet followed up for the \pkg{copBasic} package. Picking up the simulation dealt with extensively in the \bold{Note} section of \code{\link{vuongCOP}}, a user might try this:
\preformatted{
  set.seed(385); n <- 390
  UV <- simCOP(cop=PSP, n=n, col=8, pch=16, graphics=FALSE)
  taildepCOP(cop=EMPIRcop, para=UV, plot=TRUE, divisor=8, ylim=c(0,1))
  taildepCOP(cop=PSP) # lower=0.5, upper=0
}
The returned tail dependency parameters are numerically of little importance and in strict terms likely misleading. What should be of interest are the plotted trajectories of the lower and upper lines. Note: the lower tail wobbles but seems to show stability towards near \eqn{\hat\lambda_\mathbf{C}^L = 0.5} and upper-tail line wobbles downward towards \eqn{\hat\lambda_\mathbf{C}^U = 0}. These values are, respectively, the tail dependencies of the \eqn{\mathbf{PSP}} copula (\code{\link{PSP}}). A user might try increasing the sample size by an order of magnitude and rerunning the above code. Lastly, Salvadori \emph{et al.} (2006, pp. 173--175) caution on the difficulties of nonparametric tail dependency estimation. Given objectives of the \pkg{copBasic} package, estimation of \eqn{\hat\lambda_\mathbf{C}^L} and \eqn{\hat\lambda_\mathbf{C}^U}, therefore, is an open development opportunity.

\emph{DEMONSTRATION (Tail Dependence)}---The following example shows a comparison between early code examples by Charpentier (2012) concerning copulas and tail dependence using real-world data. Consider the \code{lossalae} data set and the following code requiring the \pkg{evd} package:
\preformatted{
  library(evd); X <- lossalae # Charpentier (2012)
  library(copBasic)
  fakeU <- lmomco::pp(X[,1],sort=FALSE,a=0) # Weibull plotting position i/(n+1)
  fakeV <- lmomco::pp(X[,2],sort=FALSE,a=0) # Weibull plotting position i/(n+1)
  uv <- data.frame(U=fakeU, V=fakeV)  # parameter "object" for Empirical copula
  plot(uv)
  TD <- taildepCOP(cop=EMPIRcop, para=uv, divisor=25,
                   plot=TRUE, ylim=c(0, 7/10))
  U <- rank(X[,1])/(nrow(X)+1); V <- rank(X[,2])/(nrow(X)+1)# Charpentier(2012)
  Lemp <- function(z) sum((U<=z)   & (V<=z))   / sum(U<=z  )# Charpentier(2012)
  Remp <- function(z) sum((U>=1-z) & (V>=1-z)) / sum(U>=1-z)# Charpentier(2012)
  u <- seq(0.001, 0.5, by=.001)                             # Charpentier(2012)
  L <- Vectorize(Lemp)(u); R <- Vectorize(Remp)(rev(u))     # Charpentier(2012)
  lines(qnorm(c(u, u + 0.5 - u[1])), c(L,R)) # modified after Charpentier(2012)
  legend("bottomright", c("Lower-tail dependency by taildepCOP()",
                          "Upper-tail dependency by taildepCOP()",
                          "Charpentier (2012)"), bty="n", cex=0.9,
                          lwd=1, lty=1, col=c("red", "blue", "black"))
}
The figure that will have been generated shows considerably similarity to that from the algorithms of Charpentier. Now, let us extend the discussion by using the \emph{Blomqvist (Schmid--Schmidt) Betas} (\code{\link{blomCOPss}}) that have a formulation permitting lower- and upper-tail dependency parameters in a different manner than the definitions of this documentation for \code{taildepCOP}.
\preformatted{
  edge <- 30 * 1 / (1+nrow(X)) # as few as 30 samples into the tails
  psl <- pnorm(seq(0, qnorm(  edge), by=-0.005))
  psu <- pnorm(seq(0, qnorm(1-edge), by= 0.005))
  lines(qnorm(psl),
        sapply(psl, function(p) { blomCOPss(as.sample=TRUE, para=uv,
                    ctype="checkerboard", uu=rep(p, 2), vv=c(1,1)) }),
                    col="darkgreen", lty=1, lwd=2)
  lines(qnorm(psu),
        sapply(psu, function(p) { blomCOPss(as.sample=TRUE, para=uv,
                    ctype="checkerboard", uu=c(0,0), vv=rep(p, 2)) }),
                    col="darkgreen", lty=1, lwd=2)
  points(0, blomCOP(as.sample=TRUE, para=uv), pch=16, col="magenta", cex=2)
  legend("topleft", c("Tail dependency by Blomqvist (Schmid-Schmidt) Betas",
                      "Blomqvist Beta C(1/2, 1/2)"),
                    bty="n", cex=0.9, lwd=2, lty=c(1,NA), pch=c(NA,16),
                    pt.cex=c(NA,2), col=c("darkgreen", "magenta"))
}
The thick green lines show that the dependency parameters to the left and right are approached along a different trajectory using the definitions in \code{\link{blomCOPss}}. It seems at some stage in analysis that the practioner will need to decide how deep into the tail the sample will permit for a reliable estimate of the dependency parameters. \emph{Blomqvist Beta} (\eqn{\hat\beta_\mathbf{C}}) (\code{\link{blomCOP}} is shown as the magenta dot at the median and henceforth from do the trajectories of \eqn{\hat\lambda^L_{\beta^\diamond_\mathbf{C}}} and \eqn{\hat\lambda^U_{\beta^\diamond_\mathbf{C}}} extend as their \eqn{\mathrm{lim} p \rightarrow 0^+}. Ultimately, for the data shown in the figure, perhaps the \eqn{\hat\lambda_\mathbf{C}^L = 0.1} and the \eqn{\hat\lambda_\mathbf{C}^U = 0.3} and a parametric copula fitted in part to such values.
}
\references{
Charpentier, A., 2012, Copulas and tail dependence, part 1: R-bloggers, dated Sept. 17, 2012, accessed on February 2, 2019 at \cr
\url{https://www.r-bloggers.com/2012/09/copulas-and-tail-dependence-part-1/}

\enc{Dobrić}{Dobric}, J. and Schmid, F., 2005, Nonparametric estimation of the lower tail dependence \eqn{\lambda^L} in bivariate copulas: Journal of Applied Statistics, v. 32, no. 4, pp. 387--407.

Frahm, G., Junker, M., and Schmidt, R., 2005, Estimating the tail-dependence coefficient---\cr Properties and pitfalls: Insurance---Mathematics and Economics, v. 37, no. 1, pp. 80--100.

Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.

Schmidt, R., and \enc{Stadtmüller}{Stadtmuller}, U., 2006, Nonparametric estimation of tail dependence: The Scandinavian Journal of Statistics, v. 33, pp. 307--335.
}
\author{W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{tailconCOP}}, \code{\link{tailordCOP}}, \code{\link{blomCOPss}}}
\examples{
# Plot the tail dependencies by nonexceedance probability for a
# for a positive association Plackett copula and see that both are zero.
taildepCOP(cop=PLACKETTcop, para=3, plot=TRUE)
# So, Plackett has no tail dependency, as Nelsen (2006, p. 215) shows.

\dontrun{
"MOcop" <- function(u,v, para=NULL) { # Marshall-Olkin copula
   alpha <- para[1]; beta <- para[2]; return(min(v*u^(1-alpha), u*v^(1-beta)))
} # The results that follow match those reported by Nelsen (2006, p. 215).
taildepCOP(cop=MOcop, para=c(0.4, 0.9)) # LambL = 0, LambU = 0.4 [min(alpha,beta)]}

\dontrun{
# Analytical solution to Gumbel-Hougaard copula from the copula package:
copula::lambda(copula::gumbelCopula(3))
#   lower    upper
# 0.000000 0.740079
# Numerical approximation (see copBasic::GHcop for analytical formula):
as.data.frame(taildepCOP(GHcop, para=3))
#  lambdaL lambdaU     source
#1 0.00012 0.74008 taildepCOP}

\dontrun{
# Plot the tail dependencies by nonexceedance probability
# for the PSP copula, which has lower but no upper-tail dependence.
taildepCOP(cop=PSP, para=NULL, plot=TRUE) # LambL=0.5, LambU=0
# which is readily confirmed by simCOP(1000, cop=PSP)
# Nelsen (2006, p. 216) reports that this copula has LambL=1/2 and LambU=0,
# and we get the same results here.

# How about some composited Plackett-Plackett copulas?
# Each has upper- and lower-tail dependence parameters equal to zero.
para <- list( cop1=PLACKETTcop,  cop2=PLACKETTcop, alpha=0.9392,
             para1=0.00395,     para2=4.67,         beta=0.5699)
taildepCOP(cop=composite2COP, para=para, plot=TRUE, verbose=TRUE) #}

\dontrun{
# This next Plackett-Plackett is interesting because at its core it looks
# like it should be both tail dependent like M() but the shapes of the curves
# are quite different from those of M(). This example shows numerical
# instability for the upper tail but not the lower tail. So, we extend the
# example to shown the tail dependency trajectories by blomCOPss(). And again
# it is seen that the lower tail as a stable solution but the upper tail
# has instability at 6 standard deviations into the upper tail.
para <- list(  cop1=PLACKETTcop,  cop2=PLACKETTcop, alpha=0.0063,
             para1=0.101,        para2=4493,         beta=0.0167)
taildepCOP(cop=composite2COP, para=para, plot=TRUE)
lsu <- pnorm(seq(-7, 0, by=.01))
psu <- pnorm(seq( 0, 7, by=.01))
lines(qnorm(lsu), sapply(lsu, function(p) {
        blomCOPss(cop=composite2COP, para=para, vv=c(1,1), uu=rep(p, 2)) }),
                    col="darkgreen", lty=2, lwd=1)
lines(qnorm(psu), sapply(psu, function(p) {
        blomCOPss(cop=composite2COP, para=para, uu=c(0,0), vv=rep(p, 2)) }),
                    col="darkgreen", lty=2, lwd=1) #}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{copula (tail characteristics)}
\keyword{copula (tail properties)}
\keyword{Nelsen (2006) Examples and Exercises}
\keyword{tail dependence}
\keyword{package copula (comparison to)}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{copula (tail characteristics)}
\keyword{copula (tail properties)}
\keyword{tail dependence}
\keyword{Blomqvist (Schmid-Schmidt) Beta}
