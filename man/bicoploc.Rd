\encoding{utf8}
\name{bicoploc}
\alias{bicoploc}
\title{Analog to Line of Organic Correlation by Copula Diagonal}
\description{
\emph{HIGHLY EXPERIMENTAL AND SUBJECT TO OVERHAUL OR REMOVAL}---Compute an analog to the \emph{line of organic correlation} (\emph{reduced major axis}) using the \emph{diagonal} of a copula.

\deqn{\mathrm{Pr}[U \le u, V \le v] = \mathbf{C}(u,v) = f\mbox{.}}

The primary diagonal is defined as
\deqn{\mathbf{\delta}_\mathbf{C}(t) = \mathbf{C}(t,t) = f\mbox{.}}


Two diagnostic plots can be plotted by the arguments available for this function. The plot for \eqn{(U,V)} coordinate nonexceedance probability domain along with the analyses involving the copula diagonal inversion comes first, which is followed by that for \eqn{(X,Y)} coordinate domain along with the well-known line of organic correlation by the method of L-moments and such a \dQuote{line} (could be a curve) by copula diagonal inversion.

This much infrastructure written for flexibility in how a copula would interact for the purpose of estimation with moment preservation. The simple \eqn{u = v} might be sufficient but let us have some flexibility.
}
\usage{
bicoploc(xp, yp=NULL, xout=NA, xpara=NULL, ypara=NULL, dtypex="nor", dtypey="nor",
         ctype=c("weibull", "hazen", "bernstein", "checkerboard"), kumaraswamy=TRUE,
         plotuv=TRUE, plotxy=TRUE, adduv=FALSE, addxy=FALSE, snv=FALSE, limout=TRUE,
         autoleg=TRUE, xleg="topleft", yleg=NULL, rugxy=TRUE, ruglwd=0.5,
         xlim=NULL, ylim=NULL, titleuv="", titlexy="", titlecex=1,
         a=0, ff=pnorm(seq(-5, +5, by=0.1)), locdigits=6,
         paracop=TRUE, verbose=TRUE, x=NULL, y=NULL, ...)
}
\arguments{
  \item{xp}{Numeric vector giving paired data points of \eqn{X}. If this is a matrix or data frame, then the first and second columns are extracted for the \code{xp} and \code{yp} internall;}
  \item{yp}{Optional numeric vector giving paired data points of \eqn{Y} depending on the composition of \code{x};}
  \item{xout}{An optional set of numeric values specifying where interpolation through the diagonal inversion is to take place;}
  \item{xpara}{An \pkg{lmomco} package parameter object for the \eqn{X} variable, which if not provided will trigger an method of L-moment parameter estimation for the distribution \code{dtypex};}
  \item{ypara}{An \pkg{lmomco} package parameter object for the \eqn{Y} variable, which if not provided will trigger an method of L-moment parameter estimation for the distribution \code{dtypey};}
  \item{dtypex}{The \pkg{lmomco} package distribution abbreviations (see \code{lmomco::dist.list()}) for the \eqn{X} variable. If this argument is set \code{NULL} and \code{xpara} is given with an element of \code{type}, then that distribution type is assigned internally to \code{dtypex};}
  \item{dtypey}{The \pkg{lmomco} package distribution abbreviations (see \code{lmomco::dist.list()}) for the \eqn{Y} variable; If this argument is set \code{NULL} and \code{xpara} is given with an element of \code{type}, then that distribution type is assigned internally to \code{dtypex};}
  \item{ctype}{Argument of the same name for the empirical copula for dispatch to \code{\link{EMPIRcop}}. The \code{1/n} form is disabled for \code{bicoploc} operations based on limited experiments. The first letter of the argument's value is extracted, converted to upper case, and used as the plotting character in the two diagnostic plots;}
  \item{kumaraswamy}{A logical to trigger Kumaraswamy distribution smoothing of the copula diagonal inversion from the \emph{empricial copula}. The Kumaraswamy distribution is a distribution having support \eqn{[0,1]} with an explicit quantile function and takes the place of a Beta distribution (see \pkg{lmomco} function \code{quakur()} for more details). The smoothing by Kumaraswamy will provide a continuous real number on the interval, which should insure no flat-lining as one rolls on to or off off the discrete interval provided by the empirical copula for expected sample sizes of the operations anticipated for the \code{bicoploc} function;}
  \item{plotuv}{A logical to trigger plotting of the analyses in the \eqn{(U,V)} coordinate nonexceedance probability domain along with the analyses involving the copula diagonal inversion. If set true, then \code{adduv} is set false internally;}
  \item{plotxy}{A logical to trigger plotting of the analyses in the \eqn{(X,Y)} coordinate domain along with the well-known line of organic correlation by the method of L-moments and such a \dQuote{line}  (could be a curve) by copula diagonal inversion. If set true, then \code{addxy} is set false internally;}
  \item{adduv}{A logical when set true will not call the \code{plot()} function for \eqn{(U,V)} coordinate nonexceedance probability domain but the other graphical operations of lines and points will be called;}
  \item{addxy}{A logical when set true will not call the \code{plot()} function for \eqn{(X,Y)} coordinate domain but the other graphical operations of lines and points will be called;}
  \item{snv}{A logical when set true will plot the \eqn{(U,V)} coordinate nonexceedance probability domain in units of standard normal variates;}
  \item{limout}{A logical when set true will plot the \eqn{(X,Y)} coordinate domain with horizontal and vertical axis limits inflated to the \code{xout} and \eqn{Y} predictions;}
  \item{autoleg}{A logical when set will draw a legend for the plots if the plots are requested;}
  \item{xleg}{The value to become the argument \code{x} in the \code{legend()} call. The default setting is based on general assumption that this \code{bicoploc()} function is to be more commonly used in positive assocation circumstances between \eqn{X} and \eqn{Y} (positive Spearman Rho);}
  \item{yleg}{The value to become the argument \code{y} in the \code{legend()} call;}
  \item{rugxy}{Call \code{rug()} plotting operations on the \eqn{X} and \eqn{Y} values used in the parameter estimation of the parametric marginal distributions using the \code{xp} and \code{yp}, unless either or both have been overridden for the contents in \code{x} and (or) \code{y} arguments;}
  \item{ruglwd}{The line wide passed into \code{rug()}. Because plotting of small and large sample sizes can make it difficult in the smaller samples to see the line, it is judged useful to explicitly have this setting as an declared argument;}
  \item{xlim}{A numeric vector that if precisely of length 2 and no missing values therein, will override the horizontal limits of the \code{plotxy} plot of the \eqn{(X,Y)} domain. Otherwise, the contents of \code{xlim}, if not null, are inserted into a range computation for the limits to apply;}
  \item{ylim}{A numeric vector that if precisely of length 2 and no missing values therein, will override the vertical limits of the \code{plotxy} plot of the \eqn{(X,Y)} domain. Otherwise, the contents of \code{ylim}, if not null, are inserted into a range computation for the limits to apply;}
  \item{titleuv}{An optional title for the \eqn{(U,V)} domain plot;}
  \item{titlexy}{An optional title for the \eqn{(X,Y)} domain plot;}
  \item{titlecex}{The character expansion factor for the titles;}
  \item{a}{Value for the plotting-position formula for \code{lmomco::pp()}, default is \code{a=0}, which specifies \emph{Weibull plotting positions};}
  \item{ff}{The nonexceedance joint probability of of the copula diagonal from which inversion computes the marginal nonexceedance probability values for \eqn{u=v=t} as \eqn{\mathbf{C}(t,t) = f} where \code{ff} is the variable notation for the joint probability \eqn{f};}
  \item{locdigits}{Number of digits for rounding exclusive to the \code{loc} data frame produced in the returned list. The reasoning for this setting is that the expected application in practical circumstances will have discipline knowledge of the rounding depth suitable;}
  \item{paracop}{A logical trigging the use of the parametric asymmetric copula fit by numerical optimization to the \eqn{(U,V)} domain of the data (see \bold{Details});}
  \item{verbose}{Show messages of incremental progress with a incremental counter on the message;}
  \item{x}{Numeric vector of \eqn{X} values to be used in parameter estimation of the marginal parametric distribution if \code{xpara=NULL} and these values are internally replaced with \code{xp} (paired \eqn{X}) if not otherwise specified. This provides the ability to insert an alternative and presumably longer vector of the entire \eqn{X} sample without the restriction of individual values paired to the \eqn{Y}. A feature unique to providing the \code{x} is that missing values can be and are removed on the fly prior to parameter estimation of the marginal distribution. The \code{x} can be specific independent of \code{y} or even at all for either. Absolutely no provision is made that \code{x} can be a matrix or data frame holding the \code{y};}
  \item{y}{Numeric vector of \eqn{Y} values to be used in parameter estimation of the marginal parametric distribution if \code{ypara=NULL} and these values are internally replaced with \code{yp} (paired \eqn{Y}) if not otherwise specified. This provides the ability to insert an alternative and presumably longer vector of the entire \eqn{Y} sample without the restriction of individual values paired to the \eqn{X}. A feature unique to providing the \code{y} is that missing values can be and are removed on the fly prior to parameter estimation of the marginal distribution. The \code{x} can be specific independent of \code{y} or even at all for either; and}
  \item{...}{Additional arguments to pass.}
}
\details{
\emph{ON THE USE OF AN PARAMETRIC ASYMMETRIC COPULA}---
}
\value{
  Lists, vectors, and data frames for the computations and predictions are returned.
  \item{organic}{A list containing a data frame of the predictions for \code{xout} by the conventional LOC (\code{locpair}) (see also \code{locsols$lmrloc}), the estimates by the L-moments of the parameters of the marginal distributions (\code{locpara}), the estimates by copula diagonal with Kumaraswamy smoothing (if requested) (\code{bicoploc}), and the predictions based solely on the empirical copula approximation for the diagonal (\code{bicoploc_emp}). The \code{bicoploc} and \code{bicoploc_emp} are equal to each other if Kumaraswamy was not used. The \code{bicoploc} is intended to be the official output from the \code{bicoploc()} function. The furthest right column is \code{bicoploc_cop} and represents the predicting values using a parametric copula as fit to the \eqn{(U,V)} domain mapped into the \eqn{(X,Y)} domain as explained elsewhere in this documentation or sources;}
  \item{locsols}{A list of solutions to the LOC based (1) (\code{locpair}) on the conventional definition on the paired data (\code{xp} and \code{yp}) with the finiteness check previously described by \code{lmomco::lmrloc()} and (2) (\code{locpara}) the LOC solution \emph{not on the paired data} but extractable from the L-moments of the parameters for the marginal distributions. Certain permutations of available features will either have the two L-moment solutions equal, or just the slopes equal, or differing in both intercept and slope. The \code{lmrloc} list contains both L-moment and product moment estimation of the LOC to adhere precisely to \code{lmomco::lmrloc()} output;}
  \item{xpara}{The parameters of the marginal distribution in \eqn{X} either as given in \code{xpara}, as estimated from \code{xp}, or estimated from \code{x} by the method of L-moments through the \pkg{lmomco} package;}
  \item{ypara}{The parameters of the marginal distribution in \eqn{Y} either as given in \code{ypara}, as estimated from \code{yp}, or estimated from \code{y} by the method of L-moments through the \pkg{lmomco} package;}
  \item{faqs}{A named vector containing some numerical facts about the operations and principally the requisite sample sizes involved are reported here;}
  \item{faqscop}{A named vector containing some numerical facts about the operations involving the fitting of the parametric asymmetric copula (Plackett by default) to the \eqn{(U,V)} domain; and}
  \item{diag}{A data frame containing information on the copula diagonal including the joint probability column \code{jtprob} (the \code{ff} as stand in for \eqn{C(t,t) = f}), the \eqn{u=v=t} in column \code{uv} by the Kumaraswamy smooth (if requested), and the solely empirical copula version in column \code{uv_emp}. If the Kumaraswamy smooth is not used, then \code{uv} and \code{uv_emp} will be equal to each other. The furthest right column is \code{uv_cop} and represents the values using a parametric copula as fit to the \eqn{(U,V)} domain as explained elsewhere in this documentation or sources.}
}
\note{
The use of a copula diagonal inversion for purposes of a line of organic correlation analog within the text-book literature and elsewhere is unknown to the developers (December 2023).

Though \code{bicoploc} has extensive logic for working through the copula diagonal, if we know the parent distribution and we just have the marginal probabilities equal to each other (the diagonal), then we recover the moments of \eqn{Y} simply with \eqn{u = v}:
\preformatted{
  library(lmomco)
  ff    <- c(0.0001, seq(0.001, 0.999, by=0.001), 0.9999)
  xpara <- lmomco::vec2par(c(3, 0.6, -0.4), type="pe3")
  ypara <- lmomco::vec2par(c(3, 0.4, +0.6), type="pe3")
  xx <-   rlmomco(100000,  xpara)
  yy <- approx(qlmomco(ff, xpara), qlmomco(ff, ypara), xout=xx)$y
  lmr2par(yy, type="aep4")$para
  #        mu     sigma     gamma
  # 2.9972742 0.3993914 0.5980866
}
}
\references{
Kruskal, W.H., 1953, On the uniqueness of the line of organic correlation: Biometrics, vol. 9, no. 1, pp. 47--58, \doi{10.2307/3001632}.
}
\author{W.H. Asquith}
\seealso{\code{\link{diagCOPatf}}}
\examples{
# paracop set to FALSE in these examples for speed
set.seed(4); nsim <- 50
X  <- rnorm(nsim, mean=3, sd=0.6)
Y  <- rnorm(nsim, mean=0, sd=0.2)
zz <- bicoploc(X,Y, xout=c(2.5, 3.5, 4), dtypex="nor", dtypey="nor",   paracop=FALSE)
# cor(X,Y, method="spearman") # +0.0785114 POSITIVE

set.seed(1); nsim <- 50
X  <- rnorm(nsim, mean=3, sd=0.6)
Y  <- rnorm(nsim, mean=0, sd=0.2)
zz <- bicoploc(X,Y, xout=c(2.5, 3.5, 4), dtypex="nor", dtypey="nor",   paracop=FALSE)
# cor(X,Y, method="spearman") # -0.1351741 NEGATIVE

set.seed(1); nsim <- 50
X  <-           rnorm(nsim, mean=3, sd=0.6)
Y  <- 0.843*X + rnorm(nsim, mean=0, sd=0.2)
zz <- bicoploc(X,Y, xout=c(2.5, 3.5, 4), dtypex="nor", dtypey="nor",   paracop=FALSE)
# cor(X,Y, method="spearman") # for nsim=1E6 RHO=0.92367

set.seed(1); nsim <- 50
X  <-           lmomco::rlmomco(nsim, lmomco::vec2par(c(3, 0.6, +0.5), type="pe3"))
Y  <- 0.3*X^2 + lmomco::rlmomco(nsim, lmomco::vec2par(c(0, 0.4, 0),    type="pe3"))
zz <- bicoploc(X,Y, xout=c(2.5, 3.5, 4.5), dtypex="nor", dtypey="nor", paracop=FALSE)
# cor(X,Y, method="spearman") # for nsim=1E6 RHO=0.92366

set.seed(1); nsim <- 50
X  <-           lmomco::rlmomco(nsim, lmomco::vec2par(c(3, 0.6, +0.5), type="pe3"))
Y  <- 0.3*X^2 + lmomco::rlmomco(nsim, lmomco::vec2par(c(0, 0.4, 0),    type="pe3"))
zz <- bicoploc(X,Y, xout=c(2.5, 3.5, 4.5), dtypex="gev", dtypey="gev", paracop=FALSE)

\dontrun{
########################################################################################
# Image 800 samples in X and Y and though created as pairs, let us assume only
# 50 are actually paired for purposes of demonstration of specified parameters
# and (or) alternative x and y vectors providing the larger sample.
set.seed(1); nsim <- 800; npair <- 50
X  <-           lmomco::rlmomco(nsim, lmomco::vec2par(c(3, 0.6, +0.5), type="pe3"))
Y  <- 0.3*X^2 + rnorm(nsim, mean=0, sd=0.3)
ix <- sample(seq_len(nsim), npair, replace=FALSE)
Xp <- X[ix]; Yp <- Y[ix]; dtypex <- "gev"; dtypey <- "gev"
xpara <- lmomco::lmr2par(X, type=dtypex); ypara <- lmomco::lmr2par(Y, type=dtypey)

# The next two bicoploc() calls produce identical results except for the density
# of data points along the axes for the rug plots. Ultimately, the same parameter
# estimates for the margins exists for both calls. The plotuv is disabled so that
# the user can tab between the two plotxy plots and see that they are the same.
zz <- bicoploc(Xp,Yp, xout=c(2.5, 3.5, 4.5), ruglwd=0.9, plotuv=FALSE,
               xpara=xpara, ypara=ypara, dtypex=NULL, dtypey=NULL)
mtext("Example of specific xpara and ypara from larger sample", line=0.5)

zz <- bicoploc(Xp,Yp, xout=c(2.5, 3.5, 4.5), ruglwd=0.9, plotuv=FALSE,
               xpara=xpara, ypara=ypara, dtypex=NULL, dtypey=NULL, x=X, y=Y)
mtext("Example of specific xpara and ypara from larger sample", line=0.5) #}

\dontrun{
########################################################################################
set.seed(1); nsim <- 50
UV <- rCOP(nsim, cop=breveCOP, para=list(cop=W, alpha=0, beta=0))
X  <- qnorm(UV[,1], mean=3, sd=0.6)
Y  <- qnorm(UV[,2], mean=2, sd=0.2)
zz <- bicoploc(X,Y, xout=c(1.5, 2.5, 3.5, 4), dtypex="nor", dtypey="nor")

set.seed(1); nsim <- 50
UV <- rCOP(nsim, cop=breveCOP, para=list(cop=W, alpha=0, beta=0.5))
X  <- qnorm(UV[,1], mean=3, sd=0.6)
Y  <- qnorm(UV[,2], mean=2, sd=0.2)
zz <- bicoploc(X,Y, xout=c(1.5, 2.5, 3.5, 4), dtypex="nor", dtypey="nor")

set.seed(1); nsim <- 50
UV <- rCOP(nsim, cop=breveCOP, para=list(cop=M_N5p12b, para=3, alpha=0, beta=0.5))
X  <- qnorm(UV[,1], mean=3, sd=0.6)^2
Y  <- qnorm(UV[,2], mean=2, sd=0.2)
zz <- bicoploc(X,Y, xout=c(1.5, 2.5, 3.5, 4), ylim=c(1,2.5),
                    xleg="bottomleft", dtypex="aep4", dtypey="nor")

# A TRUE COUNTER EXAMPLE? Are the 1-v operations not sufficient depending on
# rotation? Is the secondary diagonal of a copula useful? Is there something wrong
# with the reflection operations as implemented at the end of November 2023?
# Should negatives be turned into positives by reversing Y within the internal logic?
# The solution current at end of November 2023 seems proper.
set.seed(1); nsim <- 500
para <- list(cop1=W, para1=4, cop2=GHcop, para2=c(30,.6, .9), alpha=0, beta=0.1)
UV <- rCOP(nsim, cop=composite2COP, para=para)
X  <- -qexp(UV[,1], rate=10)+0.1 # Or the problem is a reflected exponential but the
Y  <- qnorm(UV[,2], mean=2, sd=0.2)  # exp version in lmomco can not handle
zz <- bicoploc(X,Y, xout=c(-0.05, 0, 0.2), dtypex="exp", dtypey="nor")
# Possibly, try another distribution.
zz <- bicoploc(X,Y, xout=c(-0.3, -0.2, 0), ylim=c(1,4), dtypex="aep4", dtypey="nor") #}

\dontrun{
########################################################################################
set.seed(1); nsim <- 200; npair <- 30
para <- list(cop=PLcop, para=80, alpha=0.3, beta=0.05)
UV <- rCOP(nsim, cop=composite1COP, para=para, resamv01=TRUE)
X  <- lmomco::qlmomco(UV[,1], lmomco::vec2par(c(3, 0.6, +0.4), type="pe3"))
Y  <- lmomco::qlmomco(UV[,2], lmomco::vec2par(c(3, 0.4, +0.0), type="pe3"))
ix <- sample(seq_len(nsim), npair, replace=FALSE)
Xp <- X[ix]; Yp <- Y[ix]; dtypex <- "pe3"; dtypey <- "pe3"
xpara <- lmomco::lmr2par(X, type=dtypex); ypara <- lmomco::lmr2par(Y, type=dtypey)
plot(10^X, 10^Y, log="xy", las=1, pch=21, lwd=0.8, col="black", bg="white",
                 xlab="SOME RISK PHENOMENON IN X-DIRECTION",
                 ylab="SOME RISK PHENOMENON IN Y-DIRECTION")
xout <- c(1.5, 2.5, 3.5, 4)
xlim <- c(1.5, 4.5); ylim <- c(1.8, 5.0)
zz <- bicoploc(Xp,Yp, xout=xout,xpara=xpara,ypara=ypara,  xlim=xlim,ylim=ylim)

zz <- bicoploc(Xp,Yp, xout=xout,xpara=xpara,ypara=ypara,  xlim=xlim,ylim=ylim,x=X,y=Y)
zz <- bicoploc(Xp,Yp, xout=xout,dtypex="pe3",dtypey="pe3",xlim=xlim,ylim=ylim,x=X,y=Y)

zz <- bicoploc(X, Y,  xout=xout,dtypex="pe3",dtypey="pe3",xlim=xlim,ylim=ylim)#}
}
\keyword{bivariate prediction}
\keyword{copula (diagonal)}
\keyword{copula (diagonal inversion)}
\keyword{line of organic correlation}
\concept{organic correlation}
\concept{reduced major axis}
