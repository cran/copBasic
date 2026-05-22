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
\seealso{\code{\link{M}}, \code{\link{W}}, \code{\link{rhoCOP}}, \code{\link{lcomCOP}}, \code{\link{PLcop}} }
\examples{
P(c(0.4, 0, 1), c(0, 0.6, 1))

\dontrun{
  # PERFECT INDEPENDENCE
  n <- 100000    # large sample size, L-comoments are zero
  lcomCOP(cop=P) # zeros (numerical integration)
  UV <- simCOP(n=n, cop=P, graphics=FALSE)
  lmomco::lcomoms2(UV, nmom=4)
  # The following are Taus_r^{12} and Taus_r^{21}
  # L-corr:        0.00265 and  0.00264 ---> ZERO
  # L-coskew:     -0.00121 and  0.00359 ---> ZERO
  # L-cokurtosis:  0.00123 and  0.00262 ---> ZERO

  # CONTRAST TO MODEST POSITIVE CORRELATION
  rho   <- 0.6; # Spearman Rho
  theta <- PLACKETTpar(rho=rho)  # Theta = 5.115658
  lcomCOP(cop=PLcop, para=theta) #   Rho = 0.6 and rest zeros
  UV <- simCOP(n=n, cop=PLcop, para=theta, graphics=FALSE)
  lmomco::lcomoms2(UV, nmom=4)
  # The following are Taus_r^{12} and Taus_r^{21}
  # L-corr       0.60303    and  0.60303    ---> Spearman Rho
  # L-coskews    4.5339e-05 and -0.00124011 ---> ZERO
  # L-cokurtosis 0.00088549 and  0.00129812 ---> ZERO
  # Pearson R == Spearman Rho for UV L-comoments }
}
\keyword{copula (formulas)}
\concept{independence copula}
\concept{product copula}
\keyword{copula}
\keyword{Pearson correlation coefficient}
\keyword{L-comoments}
\keyword{Lcomoments}
\keyword{package lmomco (application of)}
