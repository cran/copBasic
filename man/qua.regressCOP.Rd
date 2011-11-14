\name{qua.regressCOP}
\alias{qua.regressCOP}
\title{ Perform Quantile Regression using a Copula by Numerical Derivative Method for V with respect to U}
\description{
Perform \dQuote{quantile regression} using a copula by numerical derivatives of the copula. If \eqn{X} and \eqn{Y} are random variables having quantile functions \eqn{x(F)} and \eqn{y(G)} and letting \eqn{y=\tilde{y}(x)} denote a solution to \eqn{\mathrm{Pr}[Y \le y\mid X = x] = F}, where \eqn{F} is a nonexceedance probability. Then the curve \eqn{y=\tilde{y}(x)} is the quantile regression curve of \eqn{V} or \eqn{Y} with respect to \eqn{U} or \eqn{X}, respectively. If \eqn{F=1/2}, then \dQuote{median regression} is performed (\code{\link{med.regressCOP}}). Using copulas, the quantile regression is expressed as
\deqn{\mathrm{Pr}[Y \le y\mid X = x] = \mathrm{Pr}[V \le G(y) \mid U = F(x)] = \mathrm{Pr}[V \le v\mid U = v] = \frac{\delta \mathbf{C}(u,v)}{\delta u}\mbox{,}}
where \eqn{v = G(y)} and \eqn{u = F(x)}. The general algorithm is
\enumerate{
\item Set \eqn{\delta \mathbf{C}(u,v)/\delta u = F},

\item Solve the regression curve \eqn{v = \tilde{v}(u)} (accomplished by \code{\link{derCOPinv}}), and

\item Replace \eqn{u} by \eqn{x(u)} and \eqn{v} by \eqn{y(v)}.
}

The last step is optional as step two produces the regression in probability space, which might be desired, and step 3 actually transforms the probability regressions into the quantiles of the respective random variables.
}
\usage{
qua.regressCOP(F=0.5,
               U=seq(0.01,0.99, by=0.01),
               cop=NULL, para=NULL, ...)
}
\arguments{
  \item{F}{The nonexceedance probability \eqn{F} to perform regression at and defaults to median regression \eqn{F=1/2},}
  \item{U}{A vector of \eqn{u} nonexceedance probabilities,}
  \item{cop}{A copula function,}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula, and}
  \item{...}{Additional arguments to pass.}
}
\value{
  A data frame of the regressed probabilities of \eqn{V} and provided \eqn{U} values is returned.
}
\note{
See pp. 217--218 of the reference.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{med.regressCOP}}, \code{\link{derCOPinv}}, \code{\link{qua.regressCOP.draw}}}
\examples{

# Specify a positively associated Plackett copula and perform the
# regression using the defaults. Plot the regression---median
# in this case
theta <- 10
R <- qua.regressCOP(cop=PLACKETTcop, para=c(theta))
plot(R$U,R$V, type="l", lwd=5, xlim=c(0,1), ylim=c(0,1))
# now superimpose the theoretical line for the Plackett
lines(R$U,(1+(theta-1)*R$U)/(theta+1), col=4, lwd=2)
# Continue the example by performing 90th-percentile regression
R <- qua.regressCOP(F=0.90,cop=PLACKETTcop, para=c(theta))
lines(R$U,R$V, col=2, lwd=2)
# and then 10th-percentile regression
R <- qua.regressCOP(F=0.10,cop=PLACKETTcop, para=c(theta))
lines(R$U,R$V, col=3, lty=2)




# Specify a composite copula as two Placketts with respective
# parameters and then the mixing parameters alpha and beta.
para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=c(0.004),    para2=c(5),
             alpha=0.93,        beta=0.57)
# Initial a plot
plot(c(0,1),c(0,1), type="n", lwd=3,
     xlab="U, NONEXCEEDANCE PROBABILITY",
     ylab="V, NONEXCEEDANCE PROBABILITY")
# Draw the regression of V on U and then U on V (swap=TRUE)
qua.regressCOP.draw(cop=composite2COP, para=para, ploton=FALSE)
qua.regressCOP.draw(cop=composite2COP, para=para, swap=TRUE,
lty=2, ploton=FALSE)


\dontrun{
# Specify a composite copula as two Placketts with respective
# parameters and then the mixing parameters alpha and beta.
para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=c(0.04),      para2=c(500),
             alpha=0.63,        beta=0.47)
D <- simCOP(n=3000, cop=composite2COP, para=para, cex=0.5)
qua.regressCOP.draw(cop=composite2COP, para=para, ploton=FALSE)
qua.regressCOP.draw(cop=composite2COP, para=para, swap=TRUE,
                    lty=2, ploton=FALSE)
level.curvesCOP(cop=composite2COP, para=para, ploton=FALSE)

para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
              para1=c(0),      para2=c(500),
              alpha=0.63,        beta=0.47)
D <- simCOP(n=3000, cop=composite2COP, para=para, cex=0.5)
qua.regressCOP.draw(cop=composite2COP, para=para, ploton=FALSE)
qua.regressCOP.draw(cop=composite2COP, para=para,
                    swap=TRUE, lty=2, ploton=FALSE)
level.curvesCOP(cop=composite2COP, para=para, ploton=FALSE)

for(i in seq(1,10)) {
  para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
                para1=10^runif(1,min=-4,max=0),
                para2=10^runif(1,min=0,max=4),
                alpha=runif(1),        beta=runif(1))
  txts <- c("Alpha=",    round(para$alpha, digits=4),
            "; Beta=",   round(para$beta, digits=4),
            "; Theta1=", round(para$para1[1], digits=5),
            "; Theta2=", round(para$para2[1], digits=2))

  D <- simCOP(n=3000, cop=composite2COP, para=para, cex=0.5, col=3)
  mtext(paste(txts,collapse=""))
  qua.regressCOP.draw(F=c(seq(0.05, 0.95, by=0.05)),
                      cop=composite2COP, para=para, ploton=FALSE)
  qua.regressCOP.draw(F=c(seq(0.05, 0.95, by=0.05)),
                      cop=composite2COP, para=para,
                      swap=TRUE, ploton=FALSE)
  level.curvesCOP(cop=composite2COP, para=para, ploton=FALSE)

  Sys.sleep(3)
}

}
}
\keyword{distribution}
\keyword{multivariate}
