\name{joint.curvesCOP2}
\alias{joint.curvesCOP2}
\title{ Compute Coordinates of the Marginal Probabilities given joint AND or OR Probability }
\description{
Compute the coordinates of the bivariate marginal probabilities for variables \eqn{U} and \eqn{V} given selected probabilities levels \eqn{t} for a copula \eqn{\mathbf{C}(u,v)} for \eqn{u} with respect to \eqn{v}. For the case of a \bold{joint and} probability, symbolically the solution is
\deqn{\mathrm{Pr}[U \le v,\ V \le v] = t = \mathbf{C}(u,v)\mbox{,}}
where \eqn{V \mapsto [t_i, t_{j}, t_{j+1}, \cdots, 1; \Delta]} (an irregular sequence of \eqn{v} values from the \eqn{i}th value of \eqn{t_i} provided through to unity) and thus
\deqn{t_i \mapsto \mathbf{C}(u, v=V)\mbox{,}}
and solving for the sequence of \eqn{u}. The index \eqn{j} is to indicate that a separate loop is involved and is distinct from \eqn{i}. The pairings \eqn{\{u(t_i), v(t_i)\}} for each \eqn{t} are packaged as an \R \code{data.frame}. This operation is very similiar to the plotting capabilities in \code{\link{level.curvesCOP2}} for \emph{level curves} (Nelsen, 2006, pp. 12--13) but implemented in the function \code{joint.curvesCOP2} for alternative utility.

For the case of a \bold{joint or} probability, the \emph{dual of a copula (function)} or \eqn{\tilde{\mathbf{C}}(u,v)} from a copula (Nelsen, 2006, pp. 33--34) is used and symbolically the solution is:
\deqn{\mathrm{Pr}[U \le v \mathrm{\ or\ } V \le v] = t = \tilde{\mathbf{C}}(u,v) = u + v - \mathbf{C}(u,v)\mbox{,}}
where \eqn{V \mapsto [0, v_j, v_{j+1}, \cdots, t_i; \Delta]} (an irregular sequence of \eqn{v} values from zero through to the \eqn{i}th value of \eqn{t}) and thus
\deqn{t_i \mapsto \tilde{\mathbf{C}}(u, v=V)\mbox{,}}
and solving for the sequence of \eqn{u}.  The index \eqn{j} is to indicate that a separate loop is involved and is distinct from \eqn{i}. The pairings \eqn{\{u(t_i), v(t_i)\}} for each \eqn{t} are packaged as an \R \code{data.frame}.
}
\usage{
joint.curvesCOP2(cop=NULL, para=NULL, type=c("and", "or"),
                 probs=c(0.5, 0.8, 0.90, 0.96, 0.98, 0.99, 0.995, 0.998),
                 zero2small=TRUE, small=1E-6, divisor=100, delv=0.001, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{type}{What type of joint probability is to be computed;}
  \item{probs}{The joint probabilities for which to compute the coordinates. The default values represent especially useful annual return period equivalents that are useful in hydrologic risk analyses;}
  \item{zero2small}{A logical controlling whether precise zero value for probability are converted to a \code{small} value and precise unity values for probability are converted to the value \code{1 - small}; this logical is useful if transformation from probability space into standard normal variates or \emph{Gumbel reduced variates} (GRV; see function \code{prob2grv()} in package \pkg{lmomco}) is later desired by the user for attendant graphics (see \bold{Examples} section);}
  \item{small}{The value for \emph{small} described for \code{zero2small};}
  \item{divisor}{A divisor on a computation of a \eqn{\Delta} for incrementing through the \eqn{v} domain as part of the coordinate computation (see source code);}
  \item{delv}{A \eqn{\Delta v} for setup of the incrementing through the \eqn{v} domain as part of the coordinate computation (see source code); and}
  \item{...}{Additional arguments to pass to the \code{\link{duCOP}} function of \pkg{copBasic} or \code{uniroot()} function.}
}
\value{
  An \R \code{list} is returned with elements each of the given \code{probs}.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{diagCOPatf}}, \code{\link{duCOP}}, \code{\link{jointCOP}}, \code{\link{joint.curvesCOP}}, \code{\link{level.curvesCOP2}}
}
\examples{
\dontrun{
# Approach the joint curves from both "with respect two" perspectives---results same.
JCvwrtu <- joint.curvesCOP( cop=PSP, prob=0.98)$"0.98"
JCuwrtv <- joint.curvesCOP2(cop=PSP, prob=0.98)$"0.98"; lim <- c(2,5)
plot(qnorm(JCvwrtu$U), qnorm(JCvwrtu$V), type="l", lwd=6, col=8, xlim=lim, ylim=lim,
     xlab="STANDARD NORMAL VARIATE IN U", ylab="STANDARD NORMAL VARIATE IN V")
lines(qnorm(JCuwrtv$U), qnorm(JCuwrtv$V), col=2, lwd=2)
mtext("98th Joint Percentile Level Curve for PSP Copula")#}
}
\keyword{joint probability}
