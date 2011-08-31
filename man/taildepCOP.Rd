\name{taildepCOP}
\alias{taildepCOP}
\title{ The Upper and Lower Tail Dependency Parameters of a Copula }
\description{
Compute the upper and lower tail dependency parameters (if they exist), respectively, of a copula according to Nelson (2006). The parameters are expressions of conditional probability. The upper tail parameter \eqn{\lambda^U_\mathbf{C}} is defined as the probability that \eqn{Y} is greater than the \eqn{100t}-th percentile defined by quantile function \eqn{y(t)} given that \eqn{X} also is greater than the \eqn{100t}-th percentile defined by quantile function \eqn{x(t)} or
\deqn{\lambda^U_\mathbf{C} = \lim_{t{\rightarrow 1^{-}}} \mathrm{Pr}[Y > y(t)\mid X > x(t)]\mbox{.}}
Similarly, the lower tail dependency parameter \eqn{\lambda^L_\mathbf{C}} is the same with reversed inequalities or
\deqn{\lambda^L_\mathbf{C} = \lim_{t{\rightarrow 0^{+}}} \mathrm{Pr}[Y \le y(t)\mid X \le x(t)]\mbox{.}}


Nelson (2006) notes that \eqn{\lambda^U_\mathbf{C}} and \eqn{\lambda^L_\mathbf{C}} are nonparametric and depend only on the copula of \eqn{X} and \eqn{Y} and shows that each can be computed as follows:
\deqn{\lambda^U_\mathbf{C} = 2 - \lim_{t{\rightarrow 1^{-}}} \frac{1 - \mathbf{C}(t,t)}{1-t}\mbox{\ and}}
\deqn{\lambda^L_\mathbf{C} = \lim_{t{\rightarrow 0^{+}}} \frac{\mathbf{C}(t,t)}{t}\mbox{.}}
}
\usage{
taildepCOP(cop=NULL, para=NULL, plotem=FALSE,
           verbose=FALSE, tol=1e-6, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{plotem}{Plot the parameters by nonexceedance probability with indicated limited values that are the ;}
  \item{verbose}{Show incremental progress;}
  \item{tol}{Tolerance on convergence; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  A list with \eqn{\lambda^U_\mathbf{C}} and \eqn{\lambda^L_\mathbf{C}} is returned.
}
\note{
See pp. 214--215 of the reference. The algorithm implemented in \code{taildepCOP} is based on halves. Starting from the median or \eqn{t=0.5}, the limit is computed by successive halving of the distance to wards 0 and 1 and checking the change in computed value against the tolerance \code{tol} argument. After the change becomes less than the the tolerance, convergence is assumed. Other tests are made for \code{NaN} with the \code{is.nan()} function to aid in breaking the halvings.

Users are encouraged to plot the results and see whether the convergence makes sense. The plot produced when \code{plotem=TRUE} shows the probability \eqn{t} transformed into standard normal deviates by the \code{qnorm()} function so that the distal reaches of each limit are seen. The terminal points of each limit computation are shown by a small dot and the letter \dQuote{L} and \dQuote{U} also plotted at the same point.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{COP}}}
\examples{

# Plot the tail dependencies by nonexceedance probability
# for a positive association Plackett copula
TD <- taildepCOP(cop=PLACKETTcop,para=c(3), plotem=TRUE)
print(TD)


# Plot the tail dependencies by nonexceedance probability
# for the PSP copula
TD <- taildepCOP(cop=PSP, para=c(3), plotem=TRUE)
print(TD)

# How about some composited Plackett-Plackett copulas?
# Each has upper and lower tail dependence parameters equal to zero.
para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=c(0.00395),  para2=c(4.67),
             alpha=0.9392,      beta=0.5699)
taildepCOP(cop=composite2COP, para=para, plotem=TRUE, verbose=TRUE)

para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=c(0.14147),  para2=c(20.96),
             alpha=0.0411,      beta=0.6873)
taildepCOP(cop=composite2COP,para=para)

para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=c(0.10137),  para2=c(4492.87),
             alpha=0.0063,      beta=0.0167)
taildepCOP(cop=composite2COP,para=para)


}
\keyword{multivariate}
