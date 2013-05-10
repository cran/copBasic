\name{qua.regressCOP2}
\alias{qua.regressCOP2}
\title{ Perform Quantile Regression using a Copula by Numerical Derivative Method for U with respect to V}
\description{
Perform \dQuote{quantile regression} using a copula by numerical derivatives of the copula. If \eqn{X} and \eqn{Y} are random variables having quantile functions \eqn{x(F)} and \eqn{y(G)} and letting \eqn{x=\tilde{x}(y)} denote a solution to \eqn{\mathrm{Pr}[X \le x\mid Y = y] = F}, where \eqn{F} is a nonexceedance probability. The the curve \eqn{x=\tilde{x}(y)} is the quantile regression curve of \eqn{U} or \eqn{X} with respect to  \eqn{V} or \eqn{Y}, respectively. If \eqn{F=1/2}, then \dQuote{median regression} is performed (\code{\link{med.regressCOP2}}). Using copulas, the quantile regression is expressed as
\deqn{\mathrm{Pr}[X \le x\mid Y = y] = \mathrm{Pr}[U \le F(x) \mid V = F] = \mathrm{Pr}[U \le u\mid V = F] = \frac{\delta \mathbf{C}(u,v)}{\delta v}\mbox{,}}
where \eqn{v = G(y)} and \eqn{u = F(x)}. The general algorithm is
\enumerate{
\item Set \eqn{\delta \mathbf{C}(u,v)/\delta v = F},

\item Solve the regression curve \eqn{u = \tilde{u}(v)} (accomplished by \code{\link{derCOPinv2}}), and

\item Replace \eqn{u} by \eqn{x(u)} and \eqn{v} by \eqn{y(v)}.
}

The last step is optional as step two produces the regression in probability space, which might be desired, and step 3 actually transforms the probability regressions into the quantiles of the respective random variables.
}
\usage{
qua.regressCOP2(F=0.5,
                V=seq(0.01,0.99, by=0.01),
                cop=NULL, para=NULL, ...)
}
\arguments{
  \item{F}{The nonexceedance probability \eqn{F} to perform regression at,}
  \item{V}{A vector of \eqn{v} nonexceedance probabilities,}
  \item{cop}{A copula function,}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula, and}
  \item{...}{Additional arguments to pass.}
}
\value{
  A data frame of the simulated values is returned.
}
\note{
See pp. 217--218 of the reference.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{med.regressCOP2}}, \code{\link{derCOPinv2}}}
\examples{

\dontrun{
# Specify a negatively associated Plackett copula and perform the
# regression using the defaults. Plot the regression---median
# in this case
theta <- 0.10
R <- qua.regressCOP2(cop=PLACKETTcop, para=c(theta))
plot(R$U,R$V, type="l", lwd=3, xlim=c(0,1), ylim=c(0,1))
lines(R$U,(1+(theta-1)*R$U)/(theta+1), col=2)

R <- qua.regressCOP2(F=0.90,cop=PLACKETTcop, para=c(theta))
lines(R$U,R$V, col=2, lwd=2)

R <- qua.regressCOP2(F=0.10,cop=PLACKETTcop, para=c(theta))
lines(R$U,R$V, col=2, lty=2)
}

\dontrun{
# Specify a composite copula as two Placketts with respective
# parameters and then the mixing parameters alpha and beta.
para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=c(0.14),    para2=c(21),
             alpha=0.04,        beta=0.68)
# Initial a plot
plot(c(0,1),c(0,1), type="n", lwd=3,
     xlab="U, NONEXCEEDANCE PROBABILITY",
     ylab="V, NONEXCEEDANCE PROBABILITY")
# Draw the regression of V on U and then U on V (swap=TRUE)
qua.regressCOP.draw(cop=composite2COP, para=para, ploton=FALSE)
qua.regressCOP.draw(cop=composite2COP, para=para, ploton=FALSE, swap=TRUE, lty=2)
}

}
\keyword{distribution}
\keyword{multivariate}
