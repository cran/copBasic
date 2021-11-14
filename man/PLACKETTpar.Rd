\encoding{utf8}
\name{PLACKETTpar}
\alias{PLACKETTpar}
\alias{PLpar}
\title{ Estimate the Parameter of the Plackett Copula }
\description{
The parameter \eqn{\Theta} of the \emph{Plackett copula} (Nelsen, 2006, pp. 89--92) (\code{\link{PLACKETTcop}} or \code{\link{PLcop}}) is related to the \emph{Spearman Rho} (\eqn{\rho_S \ne 1}, see \code{\link{rhoCOP}})
\deqn{\rho_S(\Theta) = \frac{\Theta + 1}{\Theta - 1} - \frac{2\Theta\log(\Theta)}{(\Theta - 1)^2}\mbox{.}}
Alternatively the parameter can be estimated using a \emph{median-split estimator}, which is best shown as an algorithm. First, compute the two medians:
\preformatted{
  medx <- median(x); medy <- median(y)
}
Second and third, compute the number of occurrences where both values are less than their medians and express that as a probability:
\preformatted{
  k <- length(x[x < medx & y < medy]); m <- k/length(x)
}
Finally, the median-split estimator of  \eqn{\Theta} is computed by
\deqn{\Theta = \frac{4m^2}{(1-2m)^2}\mbox{.}}
Nelsen (2006, p. 92) and Salvadori \emph{et al.} (2007, p. 247) provide further details. The input values \code{x} and \code{y} are \emph{not used} for the median-split estimator if \emph{Spearman's Rho} (see \code{\link{rhoCOP}}) is provided by \code{rho}.
}
\usage{
PLACKETTpar(x, y, rho=NULL, byrho=FALSE, cor=NULL, ...)
      PLpar(x, y, rho=NULL, byrho=FALSE, cor=NULL, ...)
}
\arguments{
  \item{x}{Vector of values for random variable \eqn{X};}
  \item{y}{Vector of values for random variable \eqn{Y};}
  \item{rho}{Spearman Rho and \code{byrho} is set to \code{TRUE} automatically;}
  \item{byrho}{Should Spearman Rho be used instead of the median-split estimator;}
  \item{cor}{A \pkg{copBasic} syntax for \dQuote{the correlation coefficient} suitable for the copula---a synonym for \code{rho}; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  A value for the Plackett copula \eqn{\Theta} is returned.
}
\note{
Evidently there \dQuote{does not appear to be a closed form for \eqn{\tau(\Theta)}} (Fredricks and Nelsen, 2007, p. 2147),  but given \eqn{\rho(\Theta)}, the equivalent \eqn{\tau(\Theta)} can be computed by either the \code{\link{tauCOP}} function or by approximation. One of the Examples sweeps through \eqn{\rho \mapsto [0,1; \Delta\rho{=}\delta]}, fits the Plackett \eqn{\theta(\rho)}, and then solves for Kendall Tau \eqn{\tau(\theta)} using \code{\link{tauCOP}}. A polynomial is then fit between \eqn{\tau} and \eqn{\rho} to provide rapid conversion between \eqn{|\rho|} and \eqn{\tau}
where the residual standard error is 0.0005705, adjusted R-squared is \eqn{\approx 1}, the maximum residual is \eqn{\epsilon < 0.006}. Because of symmetry, it is only necessary to fit positive association and reflect the result by the sign of \eqn{\rho}. This polynomial is from the \code{Examples} is
\preformatted{
  rho <- 0.920698
  "getPLACKETTtau" <- function(rho) {
     taupoly <-   0.6229945*abs(rho)   +   1.1621854*abs(rho)^2 -
                 10.7424188*abs(rho)^3 +  48.9687845*abs(rho)^4 -
                119.0640264*abs(rho)^5 + 160.0438496*abs(rho)^6 -
                111.8403591*abs(rho)^7 +  31.8054602*abs(rho)^8
     return(sign(rho)*taupoly)
  }
  getPLACKETTtau(rho) # 0.7777726
}
The following code might be useful in some applications for the inversion of the polynomial for the \eqn{\rho} as a function of \eqn{\tau}:
\preformatted{
  "fun" <- function(rho, tau=NULL) {tp <- getPLACKETTtau(rho); return(tau-tp)}
  tau <- 0.78
  rho <- uniroot(fun, interval=c(0,1), tau=tau)$root # 0.9220636
}
}
\references{
Fredricks, G.A, and Nelsen, R.B., 2007, On the relationship between Spearman's rho and Kendall's tau for pairs of continuous random variables: Journal of Statistical Planning and Inference, v. 137, pp. 2143--2150.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{PLACKETTcop}}, \code{\link{PLcop}}, \code{\link{PLACKETTsim}}, \code{\link{rhoCOP}}
}
\examples{
\dontrun{
Q1 <- rnorm(1000); Q2 <- Q1+rnorm(1000)
PLpar(Q1,Q2); PLpar(Q1,Q2, byrho=TRUE) # two estimates for same data
PLpar(rho= 0.76) # positive association
PLpar(rho=-0.76) # negative association
tauCOP(cop=PLcop, para=PLpar(rho=-0.15, by.rho=TRUE)) #}

\dontrun{
RHOS <- seq(0,0.990, by=0.002); TAUS <- rep(NA, length(RHOS))
for(i in 1:length(RHOS)) {
   #message("Spearman Rho: ", RHOS[i])
   theta <- PLACKETTpar(rho=RHOS[i], by.rho=TRUE); tau <- NA
   try(tau <- tauCOP(cop=PLACKETTcop, para=theta), silent=TRUE)
   TAUS[i] <- ifelse(is.null(tau), NA, tau)
}
LM <- lm(TAUS~  RHOS    + I(RHOS^2) + I(RHOS^3) + I(RHOS^4) +
              I(RHOS^5) + I(RHOS^6) + I(RHOS^7) + I(RHOS^8) - 1)
plot(RHOS,TAUS, type="l", xlab="abs(Spearman Rho)", ylab="abs(Kendall Tau)")
lines(RHOS,fitted.values(LM), col=3)#}
}
\keyword{Plackett copula}
\keyword{copula (estimation)}

