\name{taildepCOP}
\alias{taildepCOP}
\title{ The Lower- and Upper-Tail Dependency Parameters of a Copula }
\description{
Compute the \emph{lower- and upper-tail dependency parameters} (if they exist), respectively, of a copula according to Nelson (2006, p. 214--215). The dependency parameters are expressions of conditional probabilitythat \eqn{Y} is greater than the \eqn{100\times t}-th percentile of its distribution \eqn{G} given that \eqn{X} is greater than the \eqn{100\times t}-th percentile of its distribution \eqn{F} as \eqn{t} approaches unity. Specifics in terms of quantile functions \eqn{G^{(-1)}(t) = y(t)} and \eqn{F^{(-1)}(t) = x(t)} follow.


The \emph{lower-tail dependence parameter} \eqn{\lambda^L_\mathbf{C}} is defined as
\deqn{\lambda^L_\mathbf{C} = \lim_{t{\rightarrow 0^{+}}} \mathrm{Pr}[Y \le y(t)\mid X \le x(t)]\mbox{, and}}
the \emph{upper-tail dependence parameter} \eqn{\lambda^U_\mathbf{C}} with reversed inequalitiesis defined as
\deqn{\lambda^U_\mathbf{C} = \lim_{t{\rightarrow 1^{-}}} \mathrm{Pr}[Y > y(t)\mid X > x(t)]\mbox{.}}


Nelson (2006) also notes that both \eqn{\lambda^L_\mathbf{C}} and \eqn{\lambda^U_\mathbf{C}} are nonparametric and depend only on the copula of \eqn{X} and \eqn{Y} and shows that each can be computed if the above limits exist as follows:
\deqn{\lambda^L_\mathbf{C} = \lim_{t{\rightarrow 0^{+}}} \frac{\mathbf{C}(t,t)}{t} = \delta_\mathbf{C}'(0^{+})\mbox{\ and}}
\deqn{\lambda^U_\mathbf{C} = 2 - \lim_{t{\rightarrow 1^{-}}} \frac{1 - \mathbf{C}(t,t)}{1-t} = 2 - \delta_\mathbf{C}'(1^{-})\mbox{,}}
where \eqn{\delta_\mathbf{C}'(t)} is the derivative of the diagonal of the copula.

If \eqn{\lambda^L_\mathbf{C} \in (0,1]}, then \eqn{\mathbf{C}} has lower-tail dependence but if \eqn{\lambda^L_\mathbf{C} = 0}, then \eqn{\mathbf{C}} has \emph{no} lower-tail dependence. Likewise, if \eqn{\lambda^U_\mathbf{C} \in (0,1]}, then \eqn{\mathbf{C}} has upper-tail dependence but if \eqn{\lambda^U_\mathbf{C} = 0}, then \eqn{\mathbf{C}} has \emph{no} upper-tail dependence.
}
\usage{
taildepCOP(cop=NULL, para=NULL, tol=1e-6, plot=FALSE, verbose=FALSE, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{tol}{A tolerance on convergence;}
  \item{plot}{A logical plotting a diagnostic plot of the diagonal derivatives and label the limits;}
  \item{verbose}{Show incremental progress; and}
  \item{...}{Additional arguments to pass to the copula function.}
}
\value{
  An \R list is returned.
  \item{lambdaL}{The rounded value of \eqn{\lambda^L_\mathbf{C}};}
  \item{lambdaU}{The rounded value of \eqn{\lambda^U_\mathbf{C}};}
  \item{source}{An attribute identifying the computational source: \dQuote{taildepCOP}.}
}
\note{
The algorithm implemented for \code{taildepCOP} is based on halves and uses the copula function (not an analytical or even numeric derivative of the diagonal, \eqn{\delta_\mathbf{C}'(t)}). Starting from the median or \eqn{t = 0.5}, each limit is respectively computed by successive halving of the distance towards \eqn{0^{+}} and \eqn{1^{-}} and checking the change in computed value against the tolerance \code{tol} argument. After the change becomes less than the the \code{tol}erance, convergence is assumed. Other tests are made for \code{NaN} to aid in breaking the successive halvings. The rounding for the numerical results for \eqn{\lambda^U_\mathbf{C}} and \eqn{\lambda^L_\mathbf{C}} is an order of magnitude larger than the tolerance.

Users are encouraged to plot the results and further verify whether the convergence makes sense. The plot produced when \code{plotem=TRUE} shows the probability \eqn{t} transformed into standard normal variates by the \code{qnorm()} function in \R so that the distal reaches of each tail and thus limit are readily seen. The terminal points of each limit computation are shown by a small dot and the letter \dQuote{L} and \dQuote{U} also are plotted at the terminal points.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{COP}}}
\examples{
# Plot the tail dependencies by nonexceedance probability for a
# for a positive association Plackett copula and see that both are zero.
taildepCOP(cop=PLACKETTcop, para=3, plotem=TRUE)
# So Plackett has no tail dependency, as Nelson (2006, p. 215) shows.
\dontrun{
"MOcop" <- function(u,v, para=NULL) { # Marshall-Olkin copula
  alpha <- para[1]; beta <- para[2]; return(min(v*u^(1-alpha), u*v^(1-beta)))
} # The results that follow match those reported by Nelson (2006, p. 215).
taildepCOP(cop=MOcop, para=c(0.4, 0.9)) # LambL = 0, LambU = 0.4 [min(alpha,beta)]
}
\dontrun{
# Plot the tail dependencies by nonexceedance probability
# for the PSP copula, which has lower but no upper tail dependence.
taildepCOP(cop=PSP, para=NULL, plotem=TRUE) # LambL=0.5, LambU=0
# which is readily confirmed by simCOP(1000, cop=PSP)
# Nelson (2006, p. 216) reports that this copula has LambL=1/2 and LambU=0.

# How about some composited Plackett-Plackett copulas?
# Each has upper and lower tail dependence parameters equal to zero.
para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=0.00395,  para2=4.67, alpha=0.9392, beta=0.5699)
taildepCOP(cop=composite2COP, para=para, plot=TRUE, verbose=TRUE)
# This next Plackett-Plackett is interesting because at its core it looks
# like it should be both tail dependent like M() but the shapes of the curves
# are quite different from those of M().
para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=0.101,  para2=4493, alpha=0.0063, beta=0.0167)
taildepCOP(cop=composite2COP, para=para, plot=TRUE)
# This example might show numerical instability for the upper tail
}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}

