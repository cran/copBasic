\encoding{utf8}
\name{med.regressCOP}
\alias{med.regressCOP}
\title{Perform Median Regression using a Copula by Numerical Derivative Method for V with respect to U}
\description{
Perform \emph{median regression} (Nelsen, 2006, pp. 217--218) of a copula by inversion of numerical derivatives of the copula (\code{\link{derCOPinv}}). The documentation for \code{\link{qua.regressCOP}} provides mathematical details. The \code{\link{qua.regressCOP.draw}} supports so-called \emph{quantile regression} along various probability levels (see \bold{Examples}).
}
\usage{
med.regressCOP(u=seq(0.01,0.99, by=0.01), cop=NULL, para=NULL, level=NA, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{level}{The level of the prediction interval to compute. For example, \code{level=0.95} will compute the 95-percent prediction interval as will \code{level=0.05} because internally a reflection check is made; and}
  \item{...}{Additional arguments to pass such \code{\link{qua.regressCOP}} and \code{\link{derCOPinv}} that are called in succession.}
}
\value{
  An \R \code{data.frame} of the median regressed probabilities of \eqn{V} and provided \eqn{U} values is returned. Note: if \code{level} is used, then the column ordering of the returned \code{data.frame} changes---please access the columns by the named idiom. The lower- and upper-prediction interval is contained in the columns repectively titled \code{Vlwr} and \code{Vupr} to mimic nomenclature somewhat of function \code{predict.lm()} in \R.
}
\note{
An extended demonstration is needed concerning prediction intervals by median regression and a comparison to well-known linear regression. This also affords and opportunity to have \pkg{copBasic} interact with the \pkg{copula} package to gain access to the \emph{Gaussian copula} and a \emph{maximum pseudo-likelihood} estimation of the parameter.

First, a function \code{NORMcop()} is defined to form the interconnect between the two packages. It is critically important that the user recognize that the so-called \code{copula} object as built by the \pkg{copula} package is treated as the canonical \code{para} argument in \pkg{copBasic} calls herein.
\preformatted{
  "NORMcop" <-  # pCoupla() from package copula is analogous to COP()
  function(u,v, para=NULL, ...) {
    if(length(u) == 1) u <- rep(u, length(v)) # see asCOP() for reasoning of
    if(length(v) == 1) v <- rep(v, length(u)) # this "vectorization" hack
    return(copula::pCopula(matrix(c(u,v), ncol=2), para))
  }
}
The parameter \eqn{\Theta \in [-1, 1]} (\emph{Pearson R}) and \eqn{\rho_\mathbf{C}(\Theta)} (\emph{Spearman Rho}, \code{\link{rhoCOP}}) and \eqn{\tau_\mathbf{C}(\Theta)} (\emph{Kendall Tau}, \code{\link{tauCOP}}) are according to Salvadori \emph{et al.} (2007, p. 255) the values
\deqn{\rho_\mathbf{C}(\Theta) = \frac{2}{\pi}\,\mathrm{arcsin}(\Theta)}
and
\deqn{\tau_\mathbf{C}(\Theta) = \frac{6}{\pi}\,\mathrm{arcsin}(\Theta/2)\mbox{.}}

Second, a bivariate \emph{Gaussian copula} is defined with a parameter \eqn{\Theta = 0.7} (thus \eqn{\rho_\mathbf{C} = 0.6829105}, \code{rhoCOP(NORMcop, norm.cop)}) and then \eqn{n=255} samples simulated from it. These are then cast into standard normal variates to mimic the idea of bivariate data in nonprobability units and facilitation regression comparison.
\preformatted{
  norm.cop <- copula::normalCopula(c(0.7), dim = 2) # define a Gaussian copula
  UVs      <- copula::rCopula(255, norm.cop) # draw 255 samples
  UVs      <- as.data.frame(UVs); X <- qnorm(UVs[,1]); Y <- qnorm(UVs[,2])
}

Third, the \emph{Weibull plotting positions} from the \code{pp()} function of package \pkg{lmomco} are used to estimate the empirical probababilities of the data in \code{UV} that are casted into an \R \code{matrix} because the \pkg{copula} package expects the data as a \code{matrix} for the default parameter estimation. The code is completed by the specification of the fitted \emph{Gaussian copula} in \code{fnorm.cop}.
\preformatted{
  UV <- as.matrix(data.frame(U=lmomco::pp(X, sort=FALSE),
                             V=lmomco::pp(Y, sort=FALSE)))
  para <- copula::fitCopula(copula::normalCopula(dim=2), UV)
  para <- summary(para)$coefficients[1] # maximum pseudo-likelihood est.
  fnorm.cop <- copula::normalCopula(para, dim=2)
}

Fourth, ordinary-least-squares (OLS) linear regression for \eqn{Y\mid X} and \eqn{X\mid Y} is computed, and the results plotted on top of the data points. The 2/3rd-prediction limits are computed by \code{predict.lm()} and also shown.
\preformatted{
  # Classical linear regressions from two perspectives.
  LMyx <- lm(Y~X);       LMxy <- lm(X~Y)
  YonX <- summary(LMyx); XonY <- summary(LMyx)

  QUorV <- seq(-3,3, by=0.05) # vector for graphical operations
  plot(X, Y, col=8, pch=21)
  lines(QUorV, YonX$coefficients[1]+YonX$coefficients[2]*QUorV, col=2, lwd=4)
  tmp <- predict.lm(LMyx, list(X=X), level=2/3, interval="prediction")
  lines(X, tmp[,2], col=2); lines(X, tmp[,3], col="red"  )

  lines(XonY$coefficients[1]+XonY$coefficients[2]*QUorV, QUorV, col=3, lwd=4)
  tmp <- predict.lm(LMxy, list(Y=Y), level=2/3, interval="prediction")
  lines(tmp[,2], Y, col="green"); lines(tmp[,3], Y, col="green")
}

Finally, the demonstration ends with plotting of the median regression for the Gaussian copula and drawing the regression lines. The two median regression lines are nearly coincident with the OLS regression lines as anticipated with a reasonably large sample size albeit maximum pseudo-likelihood was used to estimate the copula parameter. The mean of a uniform distributed variable given say \eqn{U = u} (horizontal axis) is 1/2, which coincides with the median. The median regression lines thus are coincident with the OLS lines even though OLS and real-space (native units of \eqn{X} and \eqn{Y}) were not used for their computation. The 2/3-prediction intervals also are plotted for comparison.
\preformatted{
  UorV    <- c(0.001, seq(.02,0.98, by=.02), 0.999)
  MEDreg  <- med.regressCOP( u=UorV, level=2/3, cop=NORMcop, para=fnorm.cop)
  MEDreg2 <- med.regressCOP2(v=UorV, level=2/3, cop=NORMcop, para=fnorm.cop)
  lines(qnorm(UorV),         qnorm(MEDreg$V),    col="blue",    lty=2)
  lines(qnorm(UorV),         qnorm(MEDreg$Vlwr), col="blue",    lty=2)
  lines(qnorm(UorV),         qnorm(MEDreg$Vupr), col="blue",    lty=2)
  lines(qnorm(MEDreg2$U),    qnorm(UorV),        col="magenta", lty=2)
  lines(qnorm(MEDreg2$Ulwr), qnorm(UorV),        col="magenta", lty=2)
  lines(qnorm(MEDreg2$Uupr), qnorm(UorV),        col="magenta", lty=2)
}

A curious aside (Joe, 2014, p. 164) about the \emph{Gaussian copula} is that \emph{Blomqvist Beta} (\code{\link{blomCOP}}) is equal to \emph{Kendall Tau} (\code{\link{tauCOP}}), which can be checked by
\preformatted{
  blomCOP(cop=NORMcop, para=norm.cop) # 0.4936334
  tauCOP( cop=NORMcop, para=norm.cop) # 0.493633
}
and obviously the \eqn{\beta_\mathbf{C} = \tau_\mathbf{C}} are numerically the same.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{med.regressCOP2}}, \code{\link{qua.regressCOP}}, \code{\link{qua.regressCOP.draw}}
}
\examples{
\dontrun{
# INDEPENDENCE YIELDS STRAIGHT LINES, RED IS THE MEDIAN REGRESSION
FF <- seq(0.1, 0.9, by=0.1)
plot(c(0,1),c(0,1), type="n", lwd=3,
     xlab="U, NONEXCEEDANCE PROBABILITY", ylab="V, NONEXCEEDANCE PROBABILITY")
# Draw the regression of V on U and then U on V (wrtV=TRUE)
qua.regressCOP.draw(f=FF, cop=P, para=NA, ploton=FALSE)
qua.regressCOP.draw(f=FF, cop=P, para=NA, ploton=FALSE, wrtV=TRUE, lty=2)#}

\dontrun{
# NEGATIVE PLACKETT THETA YIELDS CURVES DOWN TO RIGHT, RED IS THE MEDIAN REGRESSION
theta <- 0.5; FF <- seq(0.1, 0.9, by=0.1)
plot(c(0,1),c(0,1), type="n", lwd=3,
     xlab="U, NONEXCEEDANCE PROBABILITY", ylab="V, NONEXCEEDANCE PROBABILITY")
# Draw the regression of V on U and then U on V (wrtV=TRUE)
qua.regressCOP.draw(f=FF, cop=PLACKETTcop, ploton=FALSE, para=theta)
qua.regressCOP.draw(f=FF, cop=PLACKETTcop, ploton=FALSE, para=theta, wrtV=TRUE, lty=2)#}
}
\keyword{median regression}
\keyword{quantile regression}
\concept{quantile regression}
\keyword{Normal copula}
\keyword{Gaussian copula}
\keyword{API to the copula package}
\keyword{package copula (comparison to)}

