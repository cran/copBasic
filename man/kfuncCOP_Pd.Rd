\encoding{utf8}
\name{kfuncCOP_Pd}
\alias{kfuncCOP_Pd}
\title{The Kendall (Distribution) Function of d-Dimensional Independence Copula}
\description{
Compute the \emph{Kendall Distribution Function} of a \eqn{d}-Dimensional \emph{independence copula} (\code{\link{P}}) (Joe, 2014, p. 420):

\deqn{F_K(z; \mathbf{P}_d) = z + z\sum_k^{d-1} \bigr(-\mathrm{log}(k)\bigr)^k/k!\mbox{,}}

where \eqn{F_K} is the nonexceedance probability of joint probability \eqn{z} stemming from \eqn{\mathbf{P}_d(u_{1, 2, \cdots d})}.
}
\usage{
kfuncCOP_Pd(z, d=2)
}
\arguments{
  \item{z}{The values for \eqn{z}; and}
  \item{d}{Number of \eqn{d} dimensions.}
}
\value{
  The value(s) for \eqn{F_K(z; \mathbf{P}_d)} is returned.
}
\note{
Though the \pkg{copBasic} is designed for bivariate relations, \emph{vining} workflows could be used to produce \eqn{d}-dimensional relations from which Kendall functions could be empirically computed from \eqn{d}-variate simulations and a user might want an easy mechanism to look at \emph{independence} on Kendall distribution function plots.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{kfuncCOP}}
}
\examples{
# The 0.50 joint probability has a 0.8465736 nonexceedance probability.
kfuncCOP_Pd(0.50, d=3) # 0.8465736

\dontrun{
  f <- seq(0,1, by=0.01)
  plot(f, kfuncCOP(f, cop=P), type="l", lwd=7, col=grey(0.8),
          xlab="Kendall distribution function (joint probability)",
          ylab="Nonexceedance probability")
  lines(f, kfuncCOP_Pd(f, d=2), lwd=1.5, col="black")
  lines(f, kfuncCOP_Pd(f, d=3), col="seagreen3")
  lines(f, kfuncCOP_Pd(f, d=4), col="seagreen4") # }
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{Kendall Function}
