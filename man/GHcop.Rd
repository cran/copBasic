\encoding{utf8}
\name{GHcop}
\alias{GHcop}
\title{ The Gumbel--Hougaard Extreme Value Copula }
\description{
\emph{SYMMETRIC GUMBEL-HOUGAARD}---The \emph{Gumbel--Hougaard copula} (Nelsen, 2006, pp. 118 and 164) is
\deqn{\mathbf{C}_{\Theta}(u,v) = \mathbf{GH}(u,v) = \mathrm{exp}\{-[(-\log u)^\Theta+(-\log v)^\Theta]^{1/\Theta}\}\mbox{,}}
where \eqn{\Theta \in [1 , \infty)}. The copula here is a \emph{bivariate extreme value copula} (\eqn{BEV}). The parameter \eqn{\Theta} is readily estimated using a \emph{Kendall Tau} (say a sample version \eqn{\hat\tau}) where the \eqn{\tau} of the copula (\eqn{\tau_\mathbf{C}}) is defined as
\deqn{\tau_\mathbf{C} = \frac{\Theta - 1}{\Theta} \rightarrow \Theta = \frac{1}{1-\tau}\mbox{.}}
The copula is readily extended into \eqn{d} dimensions by
\deqn{\mathbf{C}_{\Theta}(u,v) = \mathrm{exp}\{-[(-\log u_1)^\Theta+\cdots+(-\log u_d)^\Theta]^{1/\Theta}\}\mbox{.}}
However, such an implementation is not available in the \pkg{copBasic} package.

Every Gumbel--Hougaard copula is a \emph{multivariate extreme value} (\eqn{MEV}) copula, and hence useful in analysis of extreme value distributions. The Gumbel--Hougaard copula is the \emph{only} Archimedean \eqn{MEV} (Salvadori \emph{et al.}, 2007, p. 192). The Gumbel--Hougaard copula has respective \emph{lower-} and \emph{upper-tail dependency} parameters of \eqn{\lambda^L = 0} and \eqn{\lambda^U = 2 - 2^{1/\Theta}}, respectively. Nelsen (2006, p. 96) shows that \eqn{\mathbf{C}^r_\theta(u^{1/r}, v^{1/r}) = \mathbf{C}_\theta(u,v)} so that every Gumbel--Hougaard copula is has a property known as \emph{max-stable}.  A \emph{dependence measure} uniquely defined for \eqn{BEV} copulas is shown under \code{\link{rhobevCOP}}.

A comparison through simulation between Gumbel--Hougaard implementations by the \R packages \pkg{acopula}, \pkg{copBasic}, \pkg{copula}, and \pkg{Gumbel} is shown in the \bold{Examples} section. At least three divergent techniques for random variate generation are used amongst those packages. The simulations also use \pkg{copBasic}-style random variate generation  (conditional simulation) using an analytical-numerical hybrid solution to conditional inverse described in the \pkg{Note} section.

\emph{TWO-PARAMETER GUMBEL--HOUGAARD}---A \emph{permutation symmetric} (\code{\link{isCOP.permsym}}) but almost certainly \emph{radial asymmetric} (\code{\link{isCOP.radsym}}) version of the copula is readily constructed (Brahimi \emph{et al.}, 2015) into a two-parameter version:
\deqn{\mathbf{C}(u,v; \beta_1, \beta_2) =
\biggl[
\biggl(\bigl(u^{-\beta_2} -1\bigr)^{\beta_1} +
       \bigl(v^{-\beta_2} -1\bigr)^{\beta_1}
\biggr)^{1/\beta_1} + 1
\biggr]^{-1/\beta_2}\mbox{,}}
where \eqn{\beta_1 \ge 1} and \eqn{\beta_2 > 0}. Both parameters controls the general level of association, whereas parameter \eqn{\beta_2} can be thought of as controling left-tail dependency (\code{\link{taildepCOP}}, \eqn{\lambda^{[U\mid L]}_{(\beta_1, \beta_2)}}; \emph{e.g.}
\eqn{\lambda^U_{(1.5; \beta_2)} = 0.413} for all \eqn{\beta_2} but
\eqn{\lambda^L_{(1.5; 0.2)} = 0.811} and
\eqn{\lambda^L_{(1.5; 2.2)} = 0.099}. Brahimi \emph{et al.} (2015) report a \emph{Spearman Rho} (\code{\link{rhoCOP}}) for a \eqn{\mathbf{GH}_{(1.5, 0.2)}(u,v)} is 0.5, which is readily confirmed in \pkg{copBasic} by the function call \code{rhoCOP(cop=GHcop, para=c(1.5,0.2))}. The two-parameter \eqn{\mathbf{GH}} is triggered if the length of the \code{para} argument is exactly 2.

\emph{ASYMMETRIC GUMBEL--HOUGAARD}---An asymmetric version of the copula is readily constructed (Joe, 2014, p. 185--186) into a three-parameter version with \emph{Marshall--Olkin} copulas on the boundaries:
\deqn{\mathbf{C}(u,v; \Theta, \pi_2, \pi_3) = \mathrm{exp}[-\mathcal{A}(-\log u, -\log v; \Theta, \pi_2, \pi_3)]\mbox{,}}
where \eqn{\Theta \ge 1} as before, \eqn{0 \le \pi_2,  \pi_3 \le 1}, and

\deqn{\mathcal{A}(x, y; \Theta, \pi_2, \pi_3) = [(\pi_2 x)^\Theta + (\pi_3 y)^\Theta]^{1/\Theta} + (1-\pi_2)x + (1-\pi_3)y\mbox{.}}

The asymmetric \eqn{\mathbf{GH}} is triggered if the length of the \code{para} argument is exactly 3. The \code{GHcop} function provides no mechanism for estimation of the parameters for the asymmetric version. Reviewing simulations, the bounds on the \eqn{\pi} parameters in Joe (2014, p. 185) \dQuote{[\eqn{0 \le \pi_2 < \pi_3 \le 1}]} might be incorrect---by Joe's own back reference to Joe (2014, eq. 4.35, p. 183) the \eqn{\pi}-limits as stated for \pkg{copBasic} are shown. An algorithm for parameter estimation for the asymmetric \eqn{\mathbf{GH}} using two different measures of \emph{bivariate skewness} as well as an arbitrary \emph{measure of association} is shown in section \bold{Details} in \code{\link{joeskewCOP}}.
}
\usage{
GHcop(u, v, para=NULL, tau=NULL, tau.big=0.985, cor=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (single element or triplet) of parameters---the \eqn{\Theta} parameter of the copula;}
  \item{tau}{Kendall Tau \eqn{\tau} from which to estimate the parameter \eqn{\Theta};}
  \item{tau.big}{The largest value for \eqn{\tau_\mathbf{C}} prior to switching to the \eqn{\mathbf{M}} copula applicable to the the symmetric version of this copula;}
  \item{cor}{A \pkg{copBasic} syntax for \dQuote{the correlation coefficient} suitable for the copula---a synonym for \code{tau}; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned using the \eqn{\Theta} as set by argument \code{para}. Alternative returned values are possible: (1) If \code{para=NULL} and \code{tau} is set, then \eqn{\tau_\mathbf{C} \rightarrow \Theta} and an \R \code{list} is returned. (2) If \code{para=NULL} and \code{tau=NULL}, then an attempt to estimate \eqn{\Theta} from the \code{u} and \code{v} is made by \eqn{\mathrm{cor}(u,v)_\tau \rightarrow \tau_\mathbf{C} \rightarrow \Theta} by either trigger using \code{cor(u,v, method="kendall")} in \R, and an \R \code{list} is returned. The possibly returned \code{list} has the following elements:
  \item{para}{The computed \eqn{\Theta} from the given bivariate data in \code{para}; and}
  \item{tau}{The sample estimate of \eqn{\tau}.}
}
\details{
Numerical experiments seem to indicate for \eqn{\tau_\mathbf{C} > 0.985} that failures in the numerical partial derivatives in \code{\link{derCOP}} and \code{\link{derCOP2}} result---a \eqn{\tau_\mathbf{C}} this large is indeed \emph{large}. As \eqn{\Theta \rightarrow \infty} the Gumbel--Hougaard copula becomes the \emph{\enc{Fréchet}{Frechet}--Hoeffding upper bound copula} \eqn{\mathbf{M}} (see \code{\link{M}}). A \eqn{\tau_\mathbf{C} \approx 0.985} yields \eqn{\Theta \approx 66 + 2/3}, then for \eqn{\Theta > 1/(1-\tau_\mathbf{C})} flips over to the \eqn{\mathbf{M}} copula with a warning issued.
}
\note{
\emph{SYMMETRIC GUMBEL--HOUGAARD}---A function for the derivative of the copula (Joe, 2014, p. 172) given \eqn{u} is
\preformatted{
  "GHcop.derCOP" <- function(u, v, para=NULL, ...) {
     x <- -log(u); y <- -log(v)
     A <- exp(-(x^para + y^para)^(1/para)) * (1 + (y/x)^para)^(1/para - 1)
     return(A/u)
  }
}
that can be tested by the following
\preformatted{
  Theta <- 1/(1-.15) # a Kendall Tau of 0.15
  GHcop.derCOP(     0.5, 0.75, para=Theta) # 0.7787597
  derCOP(cop=GHcop, 0.5, 0.75, para=Theta) # 0.7787597
  # The next two nearly return same value but conversion to GRVs
  # (Gumbel Reduced Variates) to magnify the numerical differences.
  # The GHcop.derCOP is expected to be the more accurate of the two.
  lmomco::prob2grv(GHcop.derCOP(     0.5, 0.9999999, para=Theta)) # 18.83349
  lmomco::prob2grv(derCOP(cop=GHcop, 0.5, 0.9999999, para=Theta)) # 18.71497
  lmomco::prob2grv(derCOP(cop=GHcop, 0.5, 0.9999999, para=Theta,
                                   delu=.Machine$double.eps^.25)) # 18.83341
}
where the last numerical approximation shows that tighter tolerance is needed. A function for the inverse of the derivative (Joe, 2014, p. 172) given \eqn{u} by an analytical-numerical hybrid is
\preformatted{
  "GHcop.derCOPinv" <- function(u,t, para=NULL, verbose=FALSE,
                                     tol=.Machine$double.eps, ...) {
    if(length(u) > 1) warning("only the first value of u will be used")
    if(length(t) > 1) warning("only the first value of t will be used")
    if(is.null(para)) { warning("para can not be NULL"); return(NA) }
    u <- u[1]; t <- t[1]; rt <- NULL
    x <- -log(u); A <- (x + (para - 1)*log(x) - log(t))
    hz <- function(z) { z + (para - 1)*log(z) - A }
    zmax <- x; i <- 0; hofz.lo <- hz(zmax)
    if(sign(hofz.lo) != -1) warning("sign for h(z) is not negative!")
    while(1) {
       i <- i + 1
       if(i > 100) {
          warning("maximum iterations looking for zmax reached"); break
       }
       # increment zmax by 1/2 log cycle, sign(hofz.lo) should be negative!
       if(sign(hz(zmax <- zmax + 1/2)) != sign(hofz.lo)) break
    }
    try(rt <- uniroot(hz, c(x, zmax), tol=tol, ...), silent=FALSE)
    if(verbose) print(rt)
    if(is.null(rt)) {
       warning("NULL on the inversion of the GH copula derivative")
       return(NA)
    }
    zo <- rt$root
    y <- (zo^para - x^para)^(1/para)
    names(y) <- NULL
    return(exp(-y))
  }
}
that can be tested by the following, which also shows how to increase the tolerance on the numerical implementation
\preformatted{
  u <- 0.999; p <- 0.999
  GHcop.derCOPinv(     u, p, para=1.56) # 0.999977
  derCOPinv(cop=GHcop, u, p, para=1.56) # 1 (unity), needs tighter tolerance
  derCOPinv(cop=GHcop, u, p, para=1.56, tol=.Machine$double.eps/10) # 0.999977
}

\emph{ASYMMETRIC GUMBEL--HOUGAARD}---Set \eqn{\tau_\mathbf{C} = 0.35} then for a symmetric and then reflection on the 1:1 line of the asymmetric Gumbel--Hougaard copula and compute the primary parameter \eqn{\Theta}, and lastly, compute three bivariate \eqn{\nu_\mathbf{C}} skewnesses (\code{\link{nuskewCOP}}):
\preformatted{
  Theta1 <- uniroot(function(t) {
                0.35 - tauCOP(cop=GHcop, para=c(t)) },           c(1,10))$root
  Theta2 <- uniroot(function(t) { # asymmetric
                0.35 - tauCOP(cop=GHcop, para=c(t, 0.6, 0.9)) }, c(1,30))$root
  Theta3 <- uniroot(function(t) { # asymmetric reflection on 1:1
                0.35 - tauCOP(cop=GHcop, para=c(t, 0.9, 0.6)) }, c(1,30))$root
  # Theta1 = 1.538462   and   Theta2 = Theta3 = 2.132856
  # Three "skews" based on a combination of U, V, and C(u,v) [nuskew()]
  nuskewCOP(cop=GHcop,   1.538462) # zero bivariate skewness
  nuskewCOP(cop=GHcop, c(2.132856, 0.6, 0.9)) #  0.008245653
  nuskewCOP(cop=GHcop, c(2.132856, 0.9, 0.6)) # -0.008245653
}
So we see, holding \eqn{\tau_\mathbf{C}} constant, that the \eqn{\mathbf{GH}} has a \eqn{\nu_{\mathbf{GH}(1.538)} = 0} but the asymmetric case \eqn{\nu_{\mathbf{GH}(2.133, 0.6, 0.9)} = 0.0082} and \eqn{\nu_{\mathbf{GH}(2.133, 0.9, 0.6)} = -0.0082} where the change in sign represents reflection about the 1:1 line. Finally, compute \emph{L-coskew} by large simulation and the adjective \dQuote{bow} representing the direction of bowing or curvature of the principle copula density.
\preformatted{
  # Because the Tau's are all similar, there is nothing to learn from the
  # L-correlation, so let us inspect the L-coskew:
  coT3.1<-lmomco::lcomoms2(simCOP(n=8000, cop=GHcop, para=c(Theta1      )))$T3
  coT3.2<-lmomco::lcomoms2(simCOP(n=8000, cop=GHcop, para=c(Theta2,.6,.9)))$T3
  coT3.3<-lmomco::lcomoms2(simCOP(n=8000, cop=GHcop, para=c(Theta3,.9,.6)))$T3
  # The simulations for Theta1 have no curvature about the diagonal.
  # The simulations for Theta2 have curvature towards the upper left.
  # The simulations for Theta3 have curvature towards the lower right.
  message("# L-coskews: ",round(coT3.1[1,2],digits=4),"(symmetric) ",
                          round(coT3.2[1,2],digits=4),"(asym.--bow UL) ",
                          round(coT3.3[1,2],digits=4),"(asym.--bow UL)")
  message("# L-coskews: ",round(coT3.1[2,1],digits=4),"(symmetric) ",
                          round(coT3.2[2,1],digits=4),"(asym.--bow LR) ",
                          round(coT3.3[2,1],digits=4),"(asym.--bow LR)")
  # L-coskews: 0.0533(symmetric) 0.1055(asym.--bow UL) 0.0021(asym.--bow UL)
  # L-coskews: 0.0679(symmetric) 0.0112(asym.--bow LR) 0.1154(asym.--bow LR)
}
Thus, the \emph{L-comoments} (Asquith, 2011) using their sample values measure something fundamental about the bivariate association between the three copulas choosen. The L-coskews for the symmetrical case are about equal and are
\deqn{\tau^{\mathbf{GH}(\Theta_1)}_{3[12]} \approx \tau^{\mathbf{GH}(\Theta_1)}_{3[21]} \rightarrow (0.0533 + 0.0679)/2 = 0.0606\mbox{,}}
whereas the L-coskew for the curvature to the upper left are
\deqn{\tau^{\mathbf{GH}(\Theta_2, 0.6, 0.9)}_{3[12]} = 0.1055\mbox{\ and\ } \tau^{\mathbf{GH}(\Theta_2, 0.6, 0.9)}_{3[12]} = 0.0112\mbox{,}}
whereas the L-coskew for the curvature to the lower right are
\deqn{\tau^{\mathbf{GH}(\Theta_3, 0.9, 0.6)}_{3[12]} = 0.0021\mbox{\ and\ } \tau^{\mathbf{GH}(\Theta_3, 0.6, 0.9)}_{3[12]} = 0.1154\mbox{.}}
Thus, the \eqn{\pi_2} and \eqn{\pi_3} parameters as choosen add about (\eqn{(0.1154+0.1055)/2 - 0.0606 \rightarrow} \eqn{0.1105 - 0.0606 = 0.05}) L-coskew units to the bivariate distribution.
}
\references{
Asquith, W.H., 2011, Distributional analysis with L-moment statistics using the R environment for statistical computing: Createspace Independent Publishing Platform, ISBN 978--146350841--8.

Brahimi, B., Chebana, F., and Necir, A., 2015, Copula representation of bivariate L-moments---A new estimation method for multiparameter two-dimensional copula models: Statistics, v. 49, no. 3, pp. 497--521.

Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.

Zhang, L., and Singh, V.P., 2007, Gumbel--Hougaard copula for trivariate rainfall frequency analysis: Journal Hydrologic Engineering, v. 12, Special issue---Copulas in Hydrology, pp. 409--419.
}
\author{ W.H. Asquith}
\seealso{\code{\link{M}}, \code{\link{GLcop}}, \code{\link{HRcop}}, \code{\link{tEVcop}}, \code{\link{rhobevCOP}}
}
\examples{
Theta <- 2.2 # Lets see if the numerical and analytical tail deps are the same.
del.lamU <- abs(taildepCOP(cop=GHcop, para=Theta)$lambdaU - (2-2^(1/Theta)))
as.logical(del.lamU < 1E-6) # TRUE
\dontrun{
# The simulations match Joe (2014, p. 72) for Gumbel-Hougaard
n <- 600; nsim <- 1000; set.seed(946) # see for reproducibility
SM <- sapply(1:nsim, function(i) { rs <- semicorCOP(cop=GHcop, para=1.35, n=n)
                                 c(rs$botleft.semicor, rs$topright.semicor) })
RhoM     <- round(mean(SM[1,]),        digits=3)
RhoP     <- round(mean(SM[2,]),        digits=3)
SE.RhoM  <- round(sd(SM[1,]),          digits=3)
SE.RhoP  <- round(sd(SM[2,]),          digits=3)
SE.RhoMP <- round(sd(SM[2,] - SM[1,]), digits=3)
# Semi-correlations (sRho) and standard errors (SEs)
message("# sRho[-]=", RhoM, " (SE[-]=", SE.RhoM, ") Joe's=0.132 (SE[-]=0.08)")
message("# sRho[+]=", RhoP, " (SE[+]=", SE.RhoP, ") Joe's=0.415 (SE[+]=0.07)")
message("# SE(sRho[-] - sRho[+])=", SE.RhoMP, " Joe's SE=0.10")
# sRho[-]=0.134 (SE[-]=0.076) Joe's=0.132 (SE[-]=0.08)
# sRho[+]=0.407 (SE[+]=0.074) Joe's=0.415 (SE[+]=0.07)
# SE(sRho[-] - sRho[+])=0.107 Joe's SE=0.10
# Joe (2014, p. 72) reports the values 0.132, 0.415, 0.08, 0.07, 0.10, respectively.}

\dontrun{
file <- "Lcomom_study_of_GHcopPLACKETTcop.txt"
x <- data.frame(tau=NA, trho=NA, srho=NA, PLtheta=NA, PLT2=NA, PLT3=NA, PLT4=NA,
                                          GHtheta=NA, GHT2=NA, GHT3=NA, GHT4=NA )
write.table(x, file=file, row.names=FALSE, quote=FALSE)
n <- 250 # Make a large number for very long CPU run but seems stable
for(tau in seq(0,0.98, by=0.005)) {
   thetag <- GHcop(u=NULL, v=NULL, tau=tau)$para
   trho <- rhoCOP(cop=GHcop, para=thetag)
   GH <- simCOP(n=n, cop=GHcop, para=thetag, points=FALSE, ploton=FALSE)
   srho <- cor(GH$U, GH$V, method="spearman")
   thetap <- PLACKETTpar(rho=trho)
   PL <- simCOP(n=n, cop=PLACKETTcop, para=thetap, points=FALSE, ploton=FALSE)
   GHl <- lmomco::lcomoms2(GH, nmom=4); PLl <- lmomco::lcomoms2(PL, nmom=4)
   x <- data.frame(tau=tau, trho=trho, srho=srho,
                   GHtheta=thetag, PLtheta=thetap,
                   GHT2=mean(c(GHl$T2[1,2], GHl$T2[2,1])),
                   GHT3=mean(c(GHl$T3[1,2], GHl$T3[2,1])),
                   GHT4=mean(c(GHl$T4[1,2], GHl$T4[2,1])),
                   PLT2=mean(c(PLl$T2[1,2], PLl$T2[2,1])),
                   PLT3=mean(c(PLl$T3[1,2], PLl$T3[2,1])),
                   PLT4=mean(c(PLl$T4[1,2], PLl$T4[2,1])) )
   write.table(x, file=file, row.names=FALSE, col.names=FALSE, append=TRUE)
}

# After a processing run with very large "n", then meaningful results exist.
D <- read.table(file, header=TRUE); D <- D[complete.cases(D),]
plot(D$tau, D$GHT3, ylim=c(-0.08,0.08), type="n",
     xlab="KENDALL TAU", ylab="L-COSKEW OR NEGATED L-COKURTOSIS")
points(D$tau,  D$GHT3, col=2);             points(D$tau,  D$PLT3, col=1)
points(D$tau, -D$GHT4, col=4, pch=2);      points(D$tau, -D$PLT4, col=1, pch=2)
LM3 <- lm(D$GHT3~I(D$tau^1)+I(D$tau^2)+I(D$tau^4)-1)
LM4 <- lm(D$GHT4~I(D$tau^1)+I(D$tau^2)+I(D$tau^4)-1)
LM3c <- LM3$coe; LM4c <- LM4$coe
Tau <- seq(0,1, by=.01); abline(0,0, lty=2, col=3)
lines(Tau,   0 + LM3c[1]*Tau^1 + LM3c[2]*Tau^2 + LM3c[3]*Tau^4,  col=4, lwd=3)
lines(Tau, -(0 + LM4c[1]*Tau^1 + LM4c[2]*Tau^2 + LM4c[3]*Tau^4), col=2, lwd=3) #}

\dontrun{
# Let us compare the conditional simulation method of copBasic by numerics and by the
# above analytical solution for the Gumbel-Hougaard copula to two methods implemented
# by package gumbel, a presumed Archimedean technique by package acopula, and an
# Archimedean technique by package copula. Setting seeds by each "method" below does
# not appear diagnostic because of the differences in which the simulations are made.
nsim <- 10000; kn <- "kendall" #  The theoretical KENDALL TAU is (1.5-1)/1.5 = 1/3
# Simulate by conditional simulation using numerical derivative and then inversion
A <- cor(copBasic::simCOP(nsim, cop=GHcop, para=1.5, graphics=FALSE), method=kn)[1,2]
U <- runif(nsim) # GHcop.derCOPinv() comes from earlier in this documentation.
V <- sapply(1:nsim, function(i) { GHcop.derCOPinv(U[i], runif(1), para=1.5) })
# Simulate by conditional simulation using exact analytical solution
B <- cor(U, y=V, method=kn);  rm(U, V)
# Simulate by the "common frailty" technique
C <- cor(gumbel::rgumbel(nsim, 1.5, dim=2, method=1), method=kn)[1,2]
# Simulate by "K function" (Is the K function method, Archimedean?)
D <- cor(gumbel::rgumbel(nsim, 1.5, dim=2, method=2), method=kn)[1,2]
# Simulate by an Archimedean implementation (presumably)
E <- cor(acopula::rCopula(nsim, pars=1.5), method=kn)[1,2]
# Simulate by an Archimedean implementation
G <- cor(copula::rCopula(nsim, copula::gumbelCopula(1.5)), method=kn)[1,2]
K <- round(c(A, B, C, D, E, G), digits=5); rm(A, B, C, D, E, G, kn); tx <- ", "
message("Kendall's Tau: ", K[1], tx, K[2], tx, K[3], tx, K[4], tx, K[5], tx, K[6])
# Kendall's Tau: 0.32909, 0.32474, 0.33060, 0.32805, 0.32874, 0.33986 -- run 1
# Kendall's Tau: 0.33357, 0.32748, 0.33563, 0.32913, 0.32732, 0.32416 -- run 2
# Kendall's Tau: 0.34311, 0.33415, 0.33815, 0.33224, 0.32961, 0.33008 -- run 3
# Kendall's Tau: 0.32830, 0.33573, 0.32756, 0.33401, 0.33567, 0.33182 -- nsim=50000!
# All solutions are near 1/3 and it is unknown without further study which of the
# six methods would result in the least bias and (or) sampling variability.}
}
\keyword{Gumbel--Hougaard copula}
\keyword{copula (formulas)}
\keyword{copula}
\keyword{Joe (2014) Examples and Exercises}
\keyword{Asymmetric Gumbel--Hougaard copula}
\concept{asymmetric Gumbel--Hougaard copula}
\concept{asymmetric Gumbel-Hougaard copula}
\concept{symmetric Gumbel--Hougaard copula}
\concept{symmetric Gumbel-Hougaard copula}
\concept{symmetric Gumbel copula}
\concept{asymmetric Gumbel copula}
\concept{asymmetric logistic copula}
\concept{Gumbel-Hougaard extreme value copula}
\concept{Gumbel--Hougaard extreme value copula}
\keyword{literature errors and inconsistencies}
\keyword{API to the copula package}
\keyword{package copula (comparison to)}
\keyword{copula (extreme value)}
\keyword{extreme value copula}
