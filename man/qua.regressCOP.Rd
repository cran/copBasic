\encoding{utf8}
\name{qua.regressCOP}
\alias{qua.regressCOP}
\title{ Perform Quantile Regression using a Copula by Numerical Derivative Method for V with respect to U}
\description{
Perform \emph{quantile regression} (Nelsen, 2006, pp. 217--218) using a copula by numerical derivatives of the copula (\code{\link{derCOPinv}}). If \eqn{X} and \eqn{Y} are random variables having quantile functions \eqn{x(F)} and \eqn{y(G)} and letting \eqn{y=\tilde{y}(x)} denote a solution to \eqn{\mathrm{Pr}[Y \le y\mid X = x] = F}, where \eqn{F} is a nonexceedance probability. Then the curve \eqn{y=\tilde{y}(x)} is the quantile regression curve of \eqn{V} or \eqn{Y} with respect to \eqn{U} or \eqn{X}, respectively. If \eqn{F=1/2}, then \emph{median regression} is performed (\code{\link{med.regressCOP}}). Using copulas, the quantile regression is expressed as
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
qua.regressCOP(f=0.5, u=seq(0.01,0.99, by=0.01), cop=NULL, para=NULL, ...)
}
\arguments{
  \item{f}{A single value of nonexceedance probability \eqn{F} to perform regression at and defaults to median regression \eqn{F=1/2};}
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  An \R \code{data.frame} of the regressed probabilities of \eqn{V} and provided \eqn{U=u} values is returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{med.regressCOP}}, \code{\link{derCOPinv}}, \code{\link{qua.regressCOP.draw}}}
\examples{
\dontrun{
# Use a positively associated Plackett copula and perform quantile regression
theta <- 10
R <- qua.regressCOP(cop=PLACKETTcop, para=theta) # 50th percentile regression

plot(R$U,R$V, type="l", lwd=6, xlim=c(0,1), ylim=c(0,1), col=8)
lines(R$U,(1+(theta-1)*R$U)/(theta+1), col=4, lwd=1) # theoretical for Plackett, see
#                                                             (Nelsen, 2006, p. 218)
R <- qua.regressCOP(f=0.90, cop=PLACKETTcop, para=theta) # 90th-percentile regression
lines(R$U,R$V, col=2, lwd=2)
R <- qua.regressCOP(f=0.10, cop=PLACKETTcop, para=theta) # 10th-percentile regression
lines(R$U,R$V, col=3, lty=2)
mtext("Quantile Regression V wrt U for Plackett copula")#}

\dontrun{
# Use a composite copula with two Placketts with compositing parameters alpha and beta.
para <- list(cop1=PLACKETTcop, cop2=PLACKETTcop,
             para1=0.04, para2=5, alpha=0.9, beta=0.6)
plot(c(0,1),c(0,1), type="n", lwd=3,
     xlab="U, NONEXCEEDANCE PROBABILITY", ylab="V, NONEXCEEDANCE PROBABILITY")
# Draw the regression of V on U and then U on V (wrtV=TRUE)
qua.regressCOP.draw(cop=composite2COP, para=para, ploton=FALSE)
qua.regressCOP.draw(cop=composite2COP, para=para, wrtV=TRUE, lty=2, ploton=FALSE)
mtext("Composition of Two Plackett Copulas and Quantile Regression")#}

\dontrun{
# Use a composite copula with two Placketts with compositing parameters alpha and beta.
para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=0.34, para2=50, alpha=0.63, beta=0.47)
D <- simCOP(n=3000, cop=composite2COP, para=para, cex=0.5)
qua.regressCOP.draw(cop=composite2COP, para=para, ploton=FALSE)
qua.regressCOP.draw(cop=composite2COP, para=para, wrtV=TRUE, lty=2, ploton=FALSE)
level.curvesCOP(cop=composite2COP, para=para, ploton=FALSE)
mtext("Composition of Two Plackett Copulas, Level Curves, Quantile Regression")

para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop, # Note the singularity
             para1=0, para2=500, alpha=0.63, beta=0.47)
D <- simCOP(n=3000, cop=composite2COP, para=para, cex=0.5)
qua.regressCOP.draw(cop=composite2COP, para=para, ploton=FALSE)
qua.regressCOP.draw(cop=composite2COP, para=para, wrtV=TRUE, lty=2, ploton=FALSE)
level.curvesCOP(cop=composite2COP, para=para, ploton=FALSE)
mtext("Composition of Two Plackett Copulas, Level Curves, Quantile Regression")

pdf("quantile_regression_test.pdf")
for(i in 1:10) {
  para <- list(cop1=PLACKETTcop, cop2=PLACKETTcop, alpha=runif(1), beta=runif(1),
               para1=10^runif(1,min=-4,max=0), para2=10^runif(1,min=0,max=4))
  txts <- c("Alpha=",    round(para$alpha,    digits=4),
            "; Beta=",   round(para$beta,     digits=4),
            "; Theta1=", round(para$para1[1], digits=5),
            "; Theta2=", round(para$para2[1], digits=2))

  D <- simCOP(n=3000, cop=composite2COP, para=para, cex=0.5, col=3)
  mtext(paste(txts, collapse=""))
  qua.regressCOP.draw(f=c(seq(0.05, 0.95, by=0.05)),
                      cop=composite2COP, para=para, ploton=FALSE)
  qua.regressCOP.draw(f=c(seq(0.05, 0.95, by=0.05)),
                      cop=composite2COP, para=para, wrtV=TRUE, ploton=FALSE)
  level.curvesCOP(cop=composite2COP, para=para, ploton=FALSE)
}
dev.off() # done}
}
\keyword{quantile regression}

