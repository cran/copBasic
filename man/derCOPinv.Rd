\encoding{utf8}
\name{derCOPinv}
\alias{derCOPinv}
\title{Numerical Derivative Inverse  of a Copula for V with respect to U}
\description{
Compute the inverse of a numerical partial derivative for \eqn{V} with respect to \eqn{U} of a copula, which is a \emph{conditional quantile function} for nonexceedance probability \eqn{t}, or
\deqn{t = c_u(v) = \mathbf{C}^{(-1)}_{2|1}(v|u) = \frac{\delta \mathbf{C}(u,v)}{\delta u}\mbox{,}}
and solving for \eqn{v}. Nelsen (2006, pp. 13, 40--41) shows that this inverse is quite important for random variable generation using the \emph{conditional distribution method}. This function is not vectorized and will not be so.
}
\usage{
derCOPinv(cop=NULL, u, t, trace=FALSE,
          delu=.Machine$double.eps^0.50, para=NULL, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{u}{A single nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{t}{A single nonexceedance probability level \eqn{t};}
  \item{trace}{A logical controlling a \code{message} on whether the signs on the \code{uniroot} are the same---this is helpful in exploring the numerical derivative limits of a given implementation of a copula.}
  \item{delu}{The \eqn{\Delta u} interval for the derivative;}
  \item{para}{Vector of parameters or other data structures, if needed, to pass to \code{cop}; and}
  \item{...}{Additional arguments to pass to the copula.}
}
\value{
  Value(s) for the derivative inverse are returned.
}
\note{
\emph{AN EDUCATIONAL OPPORTUNITY}---The Farlie-Gumbel-Morgenstern copula \eqn{\mathbf{FGM}(u,v)} \cr (\code{\link{FGMcop}}) (Joe, 2014, p. 213) is
\deqn{\mathbf{FGM}(u,v; \Theta) = uv[1+\Theta(1-u)(1-v)]\mbox{,}}
where \eqn{-1 \le \Theta \le 1} has analytical solutions to the conditional cumulative distribution function (CDF) \eqn{\mathbf{C}_{2|1}(v|u)} as
\deqn{\mathbf{C}_{2|1}(v|u) = v[1 + \Theta(1-v)(1-2u)]\mbox{,}}
and the inverse of the conditional CDF as
\deqn{\mathbf{C}_{2|1}(v|u) = \frac{[1 + \Theta(1-2u)] - \sqrt{[1+\Theta(1-2u)]^2 - 4t(1-2u)}}{2\Theta(1-2u)}\mbox{.}}
These three functions for the copula can be defined in \R by
\preformatted{
  "FGMcop"       <- function(u,v, para=NULL, ...) u*v*(1 + para*(1-u)*(1-v)  )
  "joeFGMder"    <- function(u,v, para=NULL, ...)   v*(1 + para*(1-v)*(1-2*u))
  "joeFGMderinv" <- function(u,t, para=NULL, ...) {
      K <- (1-2*u)
      ((1 + para*K) - sqrt((1 + para*K)^2 - 4*t*K))/(2*para*K)
  }
}
The \eqn{\mathbf{C}^{(-1)}_{2|1}(v|u)} is critical for simulation by the conditional simulation method. Although exclusively for simulation, \pkg{copBasic} uses inversion of the numerical derivative, the \eqn{\mathbf{FGM}} copula has three representations of supposedly the same analytical algorithm for simulation in the literature (Durante, 2007; Johnson, 1987; Nelsen, 2006). An opportunity for comparison is thus available.

The three analytical algorithms for nonexceedance probability \eqn{t} given \eqn{u} by mathematics and code, following Durante (2007, p. 245), are
\deqn{A = \Theta(1-2u) - 1\mbox{,}}
\deqn{B = \sqrt{A^2 - 4t(A+1)}\mbox{, and}}
\deqn{v = 2t/(B-A)\mbox{,}}
and in \R, this \dQuote{Durante algorithm} is
\preformatted{
  "durFGMderinv" <- function(u,t, para=NULL, ...) { # Durante (2007, p. 245)
      A <- para*(1-2*u) - 1; B <- sqrt(A^2 - 4*t*(A+1)); return(2*t/(B - A))
  }
}
and, letting \eqn{K = (2u - 1)}, following Johnson (1987, p. 185)
\deqn{A = K\Theta - 1}
\deqn{B = \sqrt{1 - 2K\Theta + (K\Theta)^2 + 4tK\Theta}}
\deqn{v = 2t/(B - A)}
and in \R, this \dQuote{Johnson algorithm} is
\preformatted{
  "jonFGMderinv" <- function(u,t, para=NULL, ...) { # Johnson (1987, p. 185)
      K <- (2*u - 1)
      A <- K*para - 1; B <- sqrt(1 - 2*K*para + (K*para)^2 + 4*t*K*para)
      2*t/(B - A)
  }
}
and finally following Nelsen (2006, p. 87)
\deqn{A = 1 + \theta(1 - 2u)\mbox{,}}
\deqn{B = \sqrt{A^2 - 4t(A-1)}\mbox{, and}}
\deqn{v = 2t/(B+A)\mbox{,}}
and in \R, this \dQuote{Nelsen algorithm} is
\preformatted{
  "nelFGMderinv" <- function(u,t, para=NULL, ...) { # Nelsen (2006, p. 87)
      A <- 1 + para*(1-2*u); B <- sqrt(A^2 - 4*t*(A-1)); return(2*t/(B + A))
  }
}
With appropriate code now available, two comparisons can be made in the following sections.


\emph{CONDITIONAL DISTRIBUTION FUNCTION}---A comparison of the analytical \eqn{\mathbf{FGM}(u,v)} derivative shows that Joe's equation is congruent with the numerical derivative of \pkg{copBasic}:
\preformatted{
  joeFGMder(0.8, 0.44, para=0.78)             # 0.3246848      (Joe, 2014)
  derCOP(   0.8, 0.44, para=0.78, cop=FGMcop) # 0.3246848      (copBasic )
}
and the result will be used in the computations that follow.

A comparison for \eqn{t = 0.3246848} of the analytical inverse and the numerical optimization of the numerical derivative of \pkg{copBasic} is
\preformatted{
  joeFGMderinv(0.8, 0.3246848, para=0.78)             # 0.5327603
  derCOPinv(   0.8, 0.3246848, para=0.78, cop=FGMcop) # 0.4399934 --> 0.44
}
where obviously, the two results are not in agreement---so something is amiss. Because many examples in this documentation clearly demonstrate numerical reliability, a tentative conclusion is that Joe's listed equation must be in error. Let us check this hypothesis against the three other sources:
\preformatted{
  durFGMderinv(0.8, 0.3246848, para=0.78) # 0.2074546          (Durante, 2007)
  jonFGMderinv(0.8, 0.3246848, para=0.78) # 0.44               (Johnson, 1987)
  nelFGMderinv(0.8, 0.3246848, para=0.78) # 0.44               (Nelsen,  2006)
}
The result from Durante (2007) is different from both Joe (2014) and from \pkg{copBasic}. However, the Johnson (1987) and Nelsen (2006) versions are equivalent and congruent to \pkg{copBasic} with the \emph{distinctly different} numerical methods of \code{derCOPinv}. These incongruent results demonstrate that care is needed when navigating the copula literature and the usefulness of the \pkg{copBasic}-style implementation of copula theory. In words, these computations show that the \eqn{t \approx 32}nd percentile of the \eqn{\mathbf{FGM}} copula given that the 80th percentile in \eqn{U} is about the 44th percentile of \eqn{V}.
}
\references{
Durante, F., 2007, Families of copulas, Appendix C, \emph{in} Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.

Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Johnson, M.E., 1987, Multivariate statistical simulation: New York, John Wiley, 230 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Zhang, L., and Singh, V.P., 2019, Copulas and their applications in water resources engineering: Cambridge University Press, ISBN 978--1--108--47425--2.
}
\author{W.H. Asquith}
\seealso{\code{\link{derCOP}}}
\examples{
u <- runif(1); t <- runif(1)
derCOPinv(u,t, cop=W)   # perfect negative dependence
derCOPinv(u,t, cop=P)   # independence
derCOPinv(u,t, cop=M)   # perfect positive dependence
derCOPinv(u,t, cop=PSP) # a parameterless copula example
\dontrun{
# Simulate 500 values from product (independent) copula
plot(NA,NA, type="n", xlim=c(0,1), ylim=c(0,1), xlab="U", ylab="V")
for(i in 1:500) {
   u <- runif(1); t <- runif(1)
   points(u, derCOPinv(cop=P, u, t), cex=0.5, pch=16) # black dots
}
# Now simulate 500 from the Nelsen 4.2.12 copula.
for(i in 1:500) {
   u <- runif(1); t <- runif(1)
   points(u,derCOPinv(cop=N4212cop,para=9.3,u,t), cex=2, pch=16, col=2) # red dots
} #}

\dontrun{
# Zhang and Singh (2019) exam. 3.23, p. 105
# show the application of the derivative inversion C2|1
# for u=0.6036 and t=0.6036 ---> v = 0.4719
derCOPinv( cop=CLcop, 0.6036, 0.4028, para=0.5) # 0.4719 for C2|1
derCOPinv2(cop=CLcop, 0.6036, 0.4028, para=0.5) # 0.4719 for C1|2
# and C2|1 and C1|2 are equal because the copula has permutation symmetry
isCOP.permsym(cop=CLcop, para=0.5) # TRUE}
}
\keyword{copula (utility)}
\keyword{copula (derivative inverse)}
\keyword{conditional quantile function}
\keyword{copula operator}
\keyword{derivative}
\keyword{copula (conditional quantile function)}
\keyword{Joe (2014) Examples and Exercises}
\keyword{Nelsen (2006) Examples and Exercises}
\keyword{Salvadori et al. (2007) Examples and Exercises}
\keyword{Zhang and Singh (2019) Examples and Exercises}
