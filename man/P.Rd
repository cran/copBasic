\encoding{utf8}
\name{P}
\alias{P}
\title{The Product (Independence) Copula}
\description{
Compute the \emph{product copula} (Nelsen, 2006, p. 12), which is defined as
\deqn{\mathbf{\Pi}(u,v) = uv\mbox{.}}
This is the copula of statistical independence between \eqn{U} and \eqn{V} and is sometimes referred to as the \emph{independence copula}.  The two extreme antithesis copulas are the \emph{\enc{Fréchet}{Frechet}--Hoeffding upper-bound} (\code{\link{M}}) and \emph{\enc{Fréchet}{Frechet}--Hoeffding lower-bound} (\code{\link{W}}) copulas.
}
\usage{
P(u, v, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{M}}, \code{\link{W}}, \code{\link{rhoCOP}}}
\examples{
P(c(0.4, 0, 1), c(0, 0.6, 1))

\dontrun{
n <- 100000 # giant sample size, L-comoments are zero
# PERFECT INDEPENDENCE
UV <- simCOP(n=n, cop=P, graphics=FALSE)
lmomco::lcomoms2(UV, nmom=4)
# The following are Taus_r^{12} and Taus_r^{21}
# L-corr:        0.00265 and  0.00264 ---> ZERO
# L-coskew:     -0.00121 and  0.00359 ---> ZERO
# L-cokurtosis:  0.00123 and  0.00262 ---> ZERO

# MODEST POSITIVE CORRELATION
rho <- 0.6; # Spearman Rho
theta <- PLACKETTpar(rho=rho) # Theta = 5.115658
UV <- simCOP(n=n, cop=PLACKETTcop, para=theta, graphics=FALSE)
lmomco::lcomoms2(UV, nmom=4)
# The following are Taus_r^{12} and Taus_r^{21}
# L-corr        0.50136 and  0.50138 ---> Pearson R == Spearman Rho
# L-coskews    -0.00641 and -0.00347 ---> ZERO
# L-cokurtosis -0.00153 and  0.00046 ---> ZERO }
}
\keyword{copula (formulas)}
\concept{independence copula}
\concept{product copula}
\keyword{copula}
\keyword{Pearson correlation coefficient}
