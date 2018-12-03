\encoding{utf8}
\name{lcomCOPpv}
\alias{lcomCOPpv}
\title{ Simulating the Sample Distribution(s) of L-correlation, L-coskew, and L-cokurtosis for a Copula }
\description{
\emph{EXPERIMENTAL:} The function provides two themes of sampling distribution characterization by simulation of the first three L-comoment ratios (L-correlation \eqn{\tau_{2[\ldots]}}, L-coskew \eqn{\tau_{3[\ldots]}} and L-cokurtosis \eqn{\tau_{4[\ldots]}}) of a copula. Subsequently, the sampling distribution can be used for inference.

First, semi-optional Monte Carlo integration estimation of the L-comoments of the parent copula are computed. Second, simulations involving the sample size \eqn{n} presumed the size of the actual sample from which the estimates of the sample L-comoments given as arguments. These simulations result in a report of the L-moments (not L-comoments) of the sampling distribution and these then are used to compute p-values for the L-comoment matrices provided by the user as a function argument.
}
\usage{
lcomCOPpv(n, lcom, cop=NULL, para=NULL, repcoe=5E3, type="gno",
                   mcn=1E4, mcrep=10, usemcmu=FALSE, digits=5, ...)
}
\arguments{
  \item{n}{The sample size \eqn{n}. This argument is semi-optional because \eqn{n = 0} can be given to skip corresponding simulations and the \code{ntable} on return will only contain \code{NA}; this feature permits rapid extraction of the \code{Ntable} and thus the \code{lcom} contents are simply not used;}
  \item{lcom}{The sample L-comoments (see below);}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters, if needed, to pass to the copula;}
  \item{repcoe}{The replication coefficient \eqn{\phi} affecting the number of simulations of size \code{n};}
  \item{type}{The distribution type used for modeling the distribution of the sampling values. The generalized normal (see distribution type \code{"gno"} in package \pkg{lmomco}) accommodates some skewness compared to the symmetry of the normal (\code{"nor"}) just in case situations arise in which non-ignorable skewness in the sample distribution exists. The distribution abbreviations of package \pkg{lmomco} are recognized for the \code{type} argument, but in reality the \code{"nor"} and \code{"gno"} should be more than sufficient;}
  \item{mcn}{The sample size \eqn{N} passed to the \code{\link{bilmoms}} function for the Monte Carlo integration. If \eqn{N = 0} then the Monte Carlo integration is not used, otherwise the minimum sample size is internally reset to \eqn{N=4} so that first four L-moments are computable;}
  \item{mcrep}{The number of replications of the Monte Carlo simulation by \code{\link{bilmoms}};}
  \item{usemcmu}{A logical toggling whether the mean value computed from the replicated Monte Carlo integrations is used instead of the mean values for the small sample simulation for the p-value computations;}
  \item{digits}{The number of digits to round numerical entries in the returned tables and can be \code{NA} for no rounding; and}
  \item{...}{Additional arguments to pass to the \code{\link{bilmoms}} function or to the copula.}
}
\details{
The notation \eqn{r[\ldots]} refers to two specific types of L-comoment definitions and a blend between the two. The notation \eqn{r[12]} means that the \eqn{r}th L-comoment for random variables \eqn{\{X^{(1)}, X^{(2)}\}} where \eqn{X^{(2)}} is the sorted variable and \eqn{X^{(1)}} is shuffled by the sorting index. Conversely, the notation \eqn{r[21]} means that the \eqn{r}th L-comoment for random variables \eqn{\{X^{(1)}, X^{(2)}\}} where \eqn{X^{(1)}} is the sorted variable and \eqn{X^{(2)}} is shuffled by the sorting index. The notation \eqn{r[12:21]} means that the average between the \eqn{r[21]} and \eqn{r[21]} is computed, which might prove useful in circumstances of known or expected symmetry of the L-comoments.

Continuing, \eqn{\hat\tau_{2[12]}} is the sample L-correlation, \eqn{\hat\tau_{3[12]}} is the sample L-coskew, and \eqn{\hat\tau_{4[12]}} is the sample L-cokurtosis all with respect to the sorting of the second variable. The computation of these L-comoment matricies can be made by functions such as function \code{lcomoms2()} in the \pkg{lmomco} package. The number of replications for the simulations involving the \eqn{n} sample size is computed by
\deqn{m = \phi/\sqrt{n}\mbox{,}}
where \eqn{\phi} is the \code{repcoe} replication factor or coefficient. If \code{usemcmu} is \code{TRUE} then \code{mcn} \eqn{> 0} else \code{usemcmu} is reset to \code{FALSE}.
}
\value{
  An \R \code{list} is returned.
  \item{text}{A string functioning as a label for the remaining tables;}
  \item{Ntable}{Another \R \code{list} holding tables of the L-moments of the L-comoments derived from Monte Carlo integration for samples of size \eqn{N =} \code{mcn}. The simulations are replicated \code{mcrep} times; and}
  \item{ntable}{Another \R \code{list} holding tables of the L-moments of the L-comoments derived from the small sample simulations for samples of size \eqn{n =} \code{n} as well as the p-values estimated by a generalized normal distribution (see \pkg{lmomco} package documentation) of the L-moments using either the small sample means or the mean of the replicated Monte Carlo integrations as dictated by \code{usemcmu}. In all circumstances, however, the results for the small sample simluations are tabulated in \code{ntable} only the p-value will be reflective the setting of \code{usemcmu}.}
}
\note{
A significance column for the p-values is added to the right side of the returned \code{ntable} and is used to guide the eye in interpretation of results. The significant codes having the following definitions for a two-tailed form:
\preformatted{
  "_" > 0.1;  ".", 0.1;  "*", 0.05;  "**", 0.01;  "***", 0.001
}
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith }
\seealso{\code{\link{COP}}, \code{\link{kullCOP}}, \code{\link{vuongCOP}}}
\examples{
# See Note section of vuongCOP() for an extended discussion of copula inference
\dontrun{
Tau <- 0.6410811; para <- GHcop(tau=Tau)$para # This Tau is from a situation of
# two river tributaries. These three L-comoments with univariate L-moments on the
T2 <- c(1,  0.79908960, 0.79908960, 1) # diagonals are derived from those river
# tributaries and downstream of the junction data.
T3 <- c(0, -0.04999318, 0.07689082, 0)
T4 <- c(0,  0.01773833, 0.04756257, 0) # Is the Ho:GHcop rejectable?
LCOM <- list(T2=matrix(T2, nrow=2), T3=matrix(T3, nrow=2), T4=matrix(T4, nrow=2))
set.seed(30312)
ZZ1 <- lcomCOPpv(75, LCOM, cop=GHcop, para=para, repcoe=2000, usemcmu=FALSE)
print(ZZ1)
set.seed(30312)
ZZ2 <- lcomCOPpv(75, LCOM, cop=GHcop, para=para, repcoe=2000, usemcmu=TRUE)
print(ZZ2)
# The results here suggest that the GHcop is not rejectable.}
}
\keyword{inference}
\keyword{hypothesis testing}
\keyword{copula (characteristics)}
\keyword{copula (inference)}
\keyword{L-comoments}
