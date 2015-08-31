\name{PLACKETTpar}
\alias{PLACKETTpar}
\title{ Estimate the Parameter of the Plackett Copula }
\description{
The parameter \eqn{\Theta} of the \emph{Plackett copula} (Nelsen, 2006, pp. 89--92) is related to \emph{Spearman's Rho} (\eqn{\rho_S \ne 1}, see \code{\link{rhoCOP}})
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
}
\arguments{
  \item{x}{Vector of values for random variable \eqn{X};}
  \item{y}{Vector of values for random variable \eqn{Y};}
  \item{rho}{Spearman's Rho and \code{byrho} is set to \code{TRUE} automatically;}
  \item{byrho}{Should Spearman's Rho be used instead of the median-split estimator;}
  \item{cor}{A \pkg{copBasic} syntax for \dQuote{the correlation coefficient} suitable for the copula---a synonym for \code{rho}; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  A value for the Plackett copula \eqn{\Theta} is returned.
}
\note{
Evidently there \dQuote{does not appear to be a closed form for \eqn{\tau(\Theta)}} (Fredricks and Nelsen, 2007, p. 2147),  but given \eqn{\rho(\Theta)}, the equivalent \eqn{\tau(\Theta)} can be computed by either the \code{\link{tauCOP}} function or by approximation. One of the Examples sweeps through \eqn{\rho \mapsto [0,1; \Delta\rho{=}\delta]}, fits the Plackett \eqn{\theta(\rho)}, and then solves for Kendall's Tau \eqn{\tau(\theta)} using \code{\link{tauCOP}}. A polynomial is then fit between \eqn{\tau} and \eqn{\rho} to provide rapid conversion between \eqn{|\rho|} and \eqn{\tau}:
\deqn{\mathrm{sign}(\rho)\tau(\rho) = 0.58509|\rho| + 1.27116|\rho|^2 - 6.50648|\rho|^3 + 15.27065|\rho|^4 -16.05820|\rho|^5 + 6.39068|\rho|^6\mbox{,}}
where the residual standard error is 0.00263, adjusted R-squared is \eqn{> 0.99996}, and ignoring fit at \eqn{\rho = 1}, the maximum residual is \eqn{\epsilon < 0.0063}. Because of symmetry, it is only necessary to fit positive association and reflect the result by the sign of \eqn{\rho}. The maximum \eqn{\tau(|\rho{=}1|)} is \eqn{\tau = 0.95291}. An \R version of the polynomial is
\preformatted{
  rho <- 0.920698
  "getPLACKETTtau" <- function(rho) {
     taupoly <-  0.5850914*abs(rho)   +  1.2711687*abs(rho)^2 -
                 6.5064848*abs(rho)^3 + 15.2706521*abs(rho)^4 -
                16.0582028*abs(rho)^5 +  6.3906825*abs(rho)^6
     taupoly[rho == 1] <- 1
     return(sign(rho)*taupoly)
  }
  getPLACKETTtau(rho) # 0.7800269
}
The following code might be useful in some applications for the inversion of the polynomial for the \eqn{\rho} as a function of \eqn{\tau}:
\preformatted{
  "fun" <- function(rho, tau=NULL) {tp <- getPLACKETTtau(rho); return(tau-tp)}
  tau <- 0.78
  rho <- uniroot(fun, interval=c(0,1), tau=tau)$root # 0.92069823
}
}
\references{
Fredricks, G.A, and Nelsen, R.B., 2007, On the relationship between Spearman's rho and Kendall's tau for pairs of continuous random variables: Journal of Statistical Planning and Inference, v. 137, pp. 2143--2150.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{PLACKETTcop}}, \code{\link{PLACKETTsim}}, \code{\link{rhoCOP}}
}
\examples{
Q1 <- rnorm(1000); Q2 <- Q1+rnorm(1000)
PLACKETTpar(Q1,Q2)
PLACKETTpar(Q1,Q2, byrho=TRUE)
PLACKETTpar(rho= 0.76)
PLACKETTpar(rho=-0.76)
tauCOP(cop=PLACKETTcop, para=PLACKETTpar(rho=-0.15, by.rho=TRUE))
\dontrun{
RHOS <- seq(0,1, by=0.002); TAUS <- rep(NA, length(RHOS))
for(i in 1:length(RHOS)) {
   theta <- PLACKETTpar2(rho=RHOS[i], by.rho=TRUE); tau <- NA
   try(tau <- tauCOP(cop=PLACKETTcop, para=theta), silent=TRUE)
   TAUS[i] <- ifelse(is.null(tau), NA, tau)
}
LM <- lm(TAUS~RHOS + I(RHOS^2) + I(RHOS^3) + I(RHOS^4) + I(RHOS^5) + I(RHOS^6) - 1)
plot(RHOS,TAUS, type="l", xlab="abs(Spearman Rho)", ylab="abs(Kendall Tau)")
fv <- fitted.values(LM); n1 <- length(RHOS) - 5
fv1 <- fv[length(fv) - 1]; rh1  <- RHOS[n1]
fv2 <- fv[length(fv)    ]; rh2  <- RHOS[length(RHOS)]
m <- (1 - fv1)/(1 - rh1);  rhos <- RHOS[RHOS > rh1]
taus <- m*(rhos - rh1) + fv1; fvs <- c(fv[1:(length(fv)-1)], taus)
lines(RHOS,fvs, col=3)
}
}
\keyword{Plackett copula}
\keyword{copula (estimation)}

