\encoding{utf8}
\name{W}
\alias{W}
\title{The \enc{Fréchet}{Frechet}--Hoeffding Lower-Bound Copula}
\description{
Compute the \emph{\enc{Fréchet}{Frechet}--Hoeffding lower-bound copula} (Nelsen, 2006, p. 11), which is defined as
\deqn{\mathbf{W}(u,v) = \mathrm{max}(u+v-1,0)\mbox{.}}
This is the copula of perfect anti-association (\emph{countermonotonicity}, \emph{perfectly negative dependence}) between \eqn{U} and \eqn{V} and is sometimes referred to as the \emph{countermonotonicity copula}. Its opposite is the \eqn{\mathbf{M}(u,v)} copula (\emph{comonotonicity copula}; \code{\link{M}}), and statistical \emph{independence} is the \eqn{\mathbf{\Pi}(u,v)} copula (\code{\link{P}}).
}
\usage{
W(u, v, ...)
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
\seealso{\code{\link{M}},       \code{\link{P}},
        \code{\link{breveCOP}}, \code{\link{kfuncCOP}}}
\examples{
W(0.41, 0.60) # just barely touching the support, so small, 0.01
W(0.25, 0.45) # no contact with the support, so 0
W(1,    1   ) # total consumption of the support, so 1

\dontrun{
  # This example shows the impact of the "breve" permutation asymmetry addition
  # to perfect negative correlation that though the plot of u,v changes with
  # spread direction with sign of the breve, that the distribution function of
  # the joint distribution still ranges uniformly 1 for breve = 0 down towards
  # independence as breve approaches -1 and + 1. Then, repeat similarily for
  # perfect positive correlation.
  ff <- c(0.001, seq(0.01, 0.99, by=0.01), 0.999)
  bs <- seq(-1, +1, by=0.1)
  plot(c(0,1), c(0,1), type="n", xlab="Joint probability (Kendall function)",
       ylab="Nonexceedance probability of Kendall function")
  for(b in bs) {
    lines(ff, kfuncCOP(ff, cop=breveCOP, para=list(cop=W, breve=b)),
          col=ifelse(b < 0, grey(0.8), "blue"), lwd=ifelse(b < 0, 6, 1))
  }
  # The top line will be W (breve = 0) and therefore its Kendall function is
  # uniform distribution, and the lowest line is independence.
  for(b in bs) {
    lines(ff, kfuncCOP(ff, cop=breveCOP, para=list(cop=M, breve=b)),
          col=ifelse(b < 0, grey(0.8), "seagreen"), lwd=ifelse(b < 0, 6, 1))
  }
  # The bottom line will be M (breve = 0) and therefore its Kendall function is
  # perfect correlation. The top line by M and breve == +/-1 is independence
  lines(ff, kfuncCOP(ff, cop=P), col="red", lwd=3) # draw independence in red
  legend("bottomright", c("Breve in (0,+1] on M copula by breveCOP()",
                          "Breve in (0,+1] on W copula by breveCOP()",
                          "Perfect independence by P() copula",
                          "Breves in [-1, 0] in M or W copulas by breveCOP()"),
         lwd=c(1,1,3,6), col=c("seagreen", "blue", "red", grey(0.8))) # }
}
\keyword{copula (formulas)}
\keyword{copula (limits/bounds)}
\concept{Frechet lower-bound copula}
\concept{Frechet-Hoeffding lower-bound copula}
\concept{Frechet--Hoeffding lower-bound copula}
\concept{Frechet lower bound}
\concept{Frechet-Hoeffding lower-bound}
\concept{Frechet--Hoeffding lower-bound}
\keyword{copula}
\keyword{Kendall Function}
\concept{Kendall distribution function}
\concept{Kendall function}
\concept{Kendall measure}
