\encoding{utf8}
\name{joint.curvesCOP}
\alias{joint.curvesCOP}
\title{Compute Coordinates of the Marginal Probabilities given joint AND or OR Probabilities}
\description{
Compute the coordinates of the bivariate marginal probabilities for variables \eqn{U} and \eqn{V} given selected probabilities levels \eqn{t} for a copula \eqn{\mathbf{C}(u,v)} for \eqn{v} with respect to \eqn{u}. For the case of a \bold{joint and} probability, symbolically the solution is
\deqn{\mathrm{Pr}[U \le v,\ V \le v] = t = \mathbf{C}(u,v)\mbox{,}}
where \eqn{U \mapsto [t_i, u_{j}, u_{j+1}, \cdots, 1; \Delta t]} (an irregular sequence of \eqn{u} values from the \eqn{i}th value of \eqn{t_i} provided through to unity) and thus
\deqn{t_i \mapsto \mathbf{C}(u=U, v)\mbox{,}}
and solving for the sequence of \eqn{v}. The index \eqn{j} is to indicate that a separate loop is involved and is distinct from \eqn{i}. The pairings \eqn{\{u(t_i), v(t_i)\}} for each \eqn{t} are packaged as an \R \code{data.frame}. This operation is very similiar to the plotting capabilities in \code{\link{level.curvesCOP}} for \emph{level curves} (Nelsen, 2006, pp. 12--13) but implemented in the function \code{joint.curvesCOP} for alternative utility.

For the case of a \bold{joint or} probability, the \emph{dual of a copula (function)} or \eqn{\tilde{\mathbf{C}}(u,v)} from a copula (Nelsen, 2006, pp. 33--34) is used and symbolically the solution is:
\deqn{\mathrm{Pr}[U \le v \mathrm{\ or\ } V \le v] = t = \tilde{\mathbf{C}}(u,v) = u + v - \mathbf{C}(u,v)\mbox{,}}
where \eqn{U \mapsto [0, u_j, u_{j+1}, \cdots, t_i; \Delta t]} (an irregular sequence of \eqn{u} values from zero through to the \eqn{i}th value of \eqn{t}) and thus
\deqn{t_i \mapsto \tilde{\mathbf{C}}(u=U, v)\mbox{,}}
and solving for the sequence of \eqn{v}.  The index \eqn{j} is to indicate that a separate loop is involved and is distinct from \eqn{i}. The pairings \eqn{\{u(t_i), v(t_i)\}} for each \eqn{t} are packaged as an \R \code{data.frame}.
}
\usage{
joint.curvesCOP(cop=NULL, para=NULL, type=c("and", "or"),
                probs=c(0.5, 0.8, 0.90, 0.96, 0.98, 0.99, 0.995, 0.998),
                zero2small=TRUE, small=1E-6, divisor=100, delu=0.001, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{type}{What type of joint probability is to be computed;}
  \item{probs}{The joint probabilities \eqn{t_i} from which to compute the coordinates. The default values represent especially useful annual return period equivalents that are useful in hydrologic risk analyses;}
  \item{zero2small}{A logical controlling whether exactly zero value for probability are converted to a \code{small} value and exactly unity values for probability are converted to the value \code{1 - small}; this logical is useful if transformation from probability space into standard normal variates or \emph{Gumbel reduced variates} (see function \code{prob2grv()} in package \pkg{lmomco}) is later desired by the user for attendant graphics (see \bold{Examples} section);}
  \item{small}{The value for \emph{small} described for \code{zero2small};}
  \item{divisor}{A divisor on a computation of a \eqn{\Delta t} for incrementing through the irregularly-spaced \eqn{u} domain as part of the coordinate computation (see source code);}
  \item{delu}{A \eqn{\Delta u} for setup of the incrementing through the irregularly-space \eqn{u} domain as part of the coordinate computation (see source code); and}
  \item{...}{Additional arguments to pass to the \code{\link{duCOP}} function of \pkg{copBasic} or \code{uniroot()} function in \R.}
}
\value{
  An \R \code{list} is returned with elements each of the given \code{probs}.
}
\note{
The arguments \code{divisor} and \code{delu} provide flexibility to obtain sufficient smoothness in the coordinate curvatures for a given \eqn{t}. The pairings \eqn{\{u(t_i), v(t_i)\}} for each \eqn{t} packaged as \code{data.fame}s within the returned \code{list} each have their own unique length, and this is the reason that a single \dQuote{master} \code{data.frame} is not returned by this function.

\emph{Extended Example}---The code below shows both types of joint probability being computed using the default \code{probs}. The plotting is made in \emph{Gumbel reduced variates} (GRV; see \code{prob2grv} in package \pkg{lmomco}). This transformation is somewhat suitable for the magnitude variation in and at tail depth of the \code{probs}. Also with transformation is being used, the \code{zero2small} logical is kept at \code{TRUE}, which is appropriate. The \code{zero2small} being set is also useful if standard normal variate transformation (by the \code{qnorm()} function in \R) were used instead.

The \emph{Gumbel--Hougaard} copula (\code{\link{GHcop}}) and a reversed Gumbel--Hougaard copula \code{rGH} are composited together by \code{\link{composite2COP}}. These were chosen so that some asymmetry in the solutions by \code{joint.curvesCOP} could be seen. We begin by specifying symmetrical plotting limits for later use and then creating a function for the reversed Gumbel--Hougaard and setting the parameters and composition weights:
\preformatted{
  grvlim <- lmomco::prob2grv(c(0.25,0.999)) # out to 1,000 years
  "rGHcop" <- function(u,v, ...) { u + v - 1 + GHcop(1-u, 1-v, ...) }
  para <- list(alpha=0.16, beta=0.67, cop1 =GHcop, cop2 =rGHcop,
                                      para1=4.5,   para2=2.2)
  tau    <- tauCOP(    cop=composite2COP, para=para) # Tau    = 0.351219
  nuskew <- nuskewCOP(cop=composite2COP, para=para)  # Nuskew = 0.084262
  UV <- simCOP(n=1000,  cop=composite2COP, para=para, snv=TRUE)
}
The code also computed the \emph{Kendall Tau} (\code{\link{tauCOP}}) and \emph{Nu-Skew} (\code{\link{nuskewCOP}}) and the results shown. The code finishes with a simulation by \code{\link{simCOP}} of the copula composition just for reference.

Next, we compute and plot the joint probability curves. The \code{tol}erance for the \code{uniroot} calls is reset from the \R defaults because slight wooble in the numerical solutions exists otherwise.  The \code{AND} and \code{OR} \code{lists} each provide \code{data.frame}s from which successive graphic calls plot the lines. The second \code{plot()} call is commented out so that both sets of joint probability curves are drawn on the same plot.
\preformatted{
  AND <- joint.curvesCOP(cop=composite2COP, para=para, type="and",
                         divisor=1000, tol=.Machine$double.eps)
  plot(grvlim, grvlim, type="n",
       xlab="GUMBEL REDUCED VARIATE IN U", ylab="GUMBEL REDUCED VARIATE IN V")
  for(t in sort(as.numeric(names(AND)))) {
      UV <- get(as.character(t), AND)
      lines(lmomco::prob2grv(UV$U),         lmomco::prob2grv(       UV$V))
      text( lmomco::prob2grv(median(UV$U)), lmomco::prob2grv(median(UV$V)),
           as.character(round(1/(1-t)), digits=0))
  }

  OR <- joint.curvesCOP(cop=composite2COP, para=para, type="or",
                        divisor=1000, tol=.Machine$double.eps)
  for(t in sort(as.numeric(names(OR)))) {
     UV <- get(as.character(t), OR)
     lines(lmomco::prob2grv(UV$U), lmomco::prob2grv(UV$V), col=2)
     text( lmomco::prob2grv(median(UV$U)), lmomco::prob2grv(median(UV$V)),
          as.character(round(1/(1-t)), digits=0), col=2)
  }
  mtext("Return Periods: black=cooperative risk, red=dual risk")
  abline(0,1, lty=2) # dash line is simply and equal value line
}

\emph{Black Curves}---The black curves represent the nonexceedance \bold{joint and} condition. The black curves are a form of \emph{level curve} (see \code{\link{level.curvesCOP}}), but it seems appropriate to not name them as such because Nelsen's examples and others usually have the level curves on an even step interval of probability such as 10-percent level curves. The complement of nonexceedance \bold{joint and} is the probability level that either or both random variables (say \dQuote{hazards}) \eqn{U} or \eqn{V} causes \dQuote{failure} at the respective return period.

\emph{Red Curves}---The red curves represent the nonexceedance \bold{joint or} (inclusive) condition. The complement of nonexceedance \bold{joint or} (inclusive) is the probability level that both random variables (say \dQuote{hazards}) \eqn{U} or \eqn{V} must simultaneously (or dually) occur for \dQuote{failure} at the respective return period. Note that there is obviously asymmetry in the \bold{joint or} curves.

\emph{Interpretation}---Because the two hazards can \dQuote{cooperate} to cause failure (see \code{\link{coCOP}}) for an equal level of protection (say 500-year event) relative to the complement of nonexceedance \bold{joint or} (inclusive) condition (see \code{surCOP}), the marginal probabilities must be considerably higher. Users are encouraged to review \code{\link{copBasic-package}} and the figure therein.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{diagCOPatf}}, \code{\link{duCOP}}, \code{\link{jointCOP}}, \code{\link{joint.curvesCOP2}}, \code{\link{level.curvesCOP}}
}
\examples{
# See Note section
}
\keyword{joint probability}
