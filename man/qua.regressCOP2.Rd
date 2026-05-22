\encoding{utf8}
\name{qua.regressCOP2}
\alias{qua.regressCOP2}
\title{Perform Quantile Regression using a Copula by Numerical Derivative Method for U with respect to V}
\description{
Perform \emph{quantile regression} (Nelsen, 2006, pp. 217--218) using a copula by numerical derivatives of the copula (\code{\link{derCOPinv2}}). If \eqn{X} and \eqn{Y} are random variables having quantile functions \eqn{x(F)} and \eqn{y(G)} and letting \eqn{x=\tilde{x}(y)} denote a solution to \eqn{\mathrm{Pr}[X \le x\mid Y = y] = F}, where \eqn{F} is a nonexceedance probability. Then the curve \eqn{x=\tilde{x}(y)} is the quantile regression curve of \eqn{U} or \eqn{X} with respect to  \eqn{V} or \eqn{Y}, respectively. If \eqn{F=1/2}, then \emph{median regression} is performed (\code{\link{med.regressCOP2}}). Using copulas, the quantile regression is expressed as
\deqn{\mathrm{Pr}[X \le x\mid Y = y] = \mathrm{Pr}[U \le F(x) \mid V = F] = \mathrm{Pr}[U \le u\mid V = F] = \frac{\delta \mathbf{C}(u,v)}{\delta v}\mbox{,}}
where \eqn{v = G(y)} and \eqn{u = F(x)}. The general algorithm is
\enumerate{
\item Set \eqn{\delta \mathbf{C}(u,v)/\delta v = F},

\item Solve the regression curve \eqn{u = \tilde{u}(v)} (provided by \code{\link{derCOPinv2}}), and

\item Replace \eqn{u} by \eqn{x(u)} and \eqn{v} by \eqn{y(v)}.
}

The last step is optional as step two produces the regression in probability space, which might be desired, and step 3 actually transforms the probability regressions into the quantiles of the respective random variables.
}
\usage{
qua.regressCOP2(f=0.5, v=seq(0.01,0.99, by=0.01), cop=NULL, para=NULL, ...)
}
\arguments{
  \item{f}{A single value of nonexceedance probability \eqn{F} to perform regression at and defaults to median regression \eqn{F=1/2};}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  An \R \code{data.frame} of the regressed probabilities of \eqn{U} and \eqn{V=v} is returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{med.regressCOP2}}, \code{\link{derCOPinv2}}}
\examples{
\dontrun{
# Use a positively associated Plackett copula and perform quantile regression
theta <- 0.10
R <- qua.regressCOP2(cop=PLACKETTcop, para=theta) # 50th percentile regression
plot(R$U,R$V, type="l", lwd=6, xlim=c(0,1), ylim=c(0,1), col=8)
lines((1+(theta-1)*R$V)/(theta+1),R$V, col=4, lwd=1) # theoretical for Plackett,
# compare the theoretical form to that in qua.regressCOP---just switch terms around
# because of symmetry
R <- qua.regressCOP2(f=0.90, cop=PLACKETTcop, para=theta) # 90th-percentile regression
lines(R$U,R$V, col=2, lwd=2)
R <- qua.regressCOP2(f=0.10, cop=PLACKETTcop, para=theta) # 10th-percentile regression
lines(R$U,R$V, col=2, lty=2)
mtext("Quantile Regression U wrt V for Plackett copula")#}
}
\keyword{quantile regression}
