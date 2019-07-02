\encoding{utf8}
\name{isCOP.PQD}
\alias{isCOP.PQD}
\title{ The Positively Quadrant Dependency State of a Copula }
\description{
Numerically determine the global property of the \emph{positively quadrant dependency} (PQD) characteristic of a copula as described by Nelsen (2006, p. 188). The random variables \eqn{X} and \eqn{Y} are PQD if for all \eqn{(x,y)} in \eqn{\mathcal{R}^2} when
\eqn{H(x,y) \ge F(x)G(x)} for all \eqn{(x,y)} in \eqn{\mathcal{R}^2}
and thus by the copula \eqn{\mathbf{C}(u,v) \ge uv} for all \eqn{(u,v)} in \eqn{\mathcal{I}^2}. Alternatively, this means that \eqn{\mathbf{C}(u,v) \ge \mathbf{\Pi}}, and thus it can be said that it is globally \dQuote{greater} than independence (\eqn{uv = \Pi}; \code{\link{P}}).

Nelsen (2006) shows that a copula is PQD when
\deqn{0 \le \beta_\mathbf{C} \mbox{,\ } 0 \le \gamma_\mathbf{C}\mbox{,\ and\ } 0 \le \rho_\mathbf{C} \le 3\tau_\mathbf{C}\mbox{,}}
where \eqn{\beta_\mathbf{C}}, \eqn{\gamma_\mathbf{C}}, \eqn{\rho_\mathbf{C}}, and \eqn{\tau_\mathbf{C}} are various copula measures of association or concordance that are respectively described in \code{\link{blomCOP}}, \code{\link{giniCOP}}, \code{\link{rhoCOP}}, and \code{\link{tauCOP}}.
The concept of negatively quadrant dependency (NQD) is the reverse: \eqn{\mathbf{C}(u,v) \le \mathbf{\Pi}} for all \eqn{(u,v)} in \eqn{\mathcal{I}^2}; so NQD is globally \dQuote{smaller} than independence.

Conceptually, PQD is related to the probability that two random variables are simultaneously small (or simultaneously large) is at least as great as it would be if they were \emph{independent}. The graph of a PQD copula lies on or above the copulatic surface of the \emph{independence copula} \eqn{\mathbf{\Pi}}, and conversely a NQD copula lies on or below \eqn{\mathbf{\Pi}}.

Albeit a \dQuote{global} property of a copula, there can be \dQuote{local} variations in the PQD/NQD state. Points in \eqn{\mathcal{I}^2} where \eqn{\mathbf{C}(u,v) - \mathbf{\Pi} \ge 0} are locally PQD, whereas points in \eqn{\mathcal{I}^2} where \eqn{\mathbf{C}(u,v) - \mathbf{\Pi} \le 0} and locally NQD. Lastly, readers are directed to the last examples in \code{\link{wolfCOP}} because as those examples involve the copulatic difference from independence \eqn{\mathbf{C}(u,v) - \mathbf{\Pi} = \mathbf{C}(u,v) - \mathbf{\Pi}} with 3-D renderings.
}
\usage{
isCOP.PQD(cop=NULL, para=NULL, uv=NULL, empirical=FALSE, verbose=TRUE, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{uv}{An optional \R \code{data.frame} of \eqn{U} and \eqn{V} nonexceedance probabilities \eqn{u} and \eqn{v} for the random variables \eqn{X} and \eqn{Y}. This argument triggers different value return behavior (see \bold{Value});}
  \item{empirical}{A logical that will use sample versions for \emph{Gini Gamma}, \emph{Spearman Rho}, and \emph{Kendall Tau}. This feature is \emph{only} applicable if the copula is empirical and therefore the \code{para} argument is the \code{data.frame} of \eqn{u} and \eqn{v}, which will be passed along to sample version functions instead of copula (see \bold{Note});}
  \item{verbose}{A logical that will report the four concordance measures; and}
  \item{...}{Additional arguments to pass, which are then passed to subordinate functions.}
}
\value{
   If \code{uv=NULL} then a logical for the global property of PQD is returned but if argument \code{uv} is a \code{data.frame}, then an \R \code{list} is returned, and that list holds the global condition in \code{global.PQD} and  local condition assessments in \code{local.PQD} and \code{local.NQD}.
}
\note{
The function \code{isCOP.PQD} will try \code{brute} force computations if subordinate calls to one or more functions fails. The user can use \code{...} to set the \code{delta} argument for \code{\link{giniCOP}}, \code{\link{rhoCOP}}, and (or) \code{\link{tauCOP}}.

This function is not guaranteed to work using a \emph{bivariate empirical copula} such as the following operation: \code{copPQD(cop=EMPIRcop, para=the.data)}. An evidently open problem for \pkg{copBasic} is how to support PQD assessment (either globally or locally) for empirical copulas. The \eqn{\tau_\mathbf{C}} for the bivariate empirical copula example \code{brute=TRUE|FALSE} to unity and \eqn{\gamma_\mathbf{C}} and \eqn{\rho_\mathbf{C}} reach maximum number of subdivisions on the numerical integration and thus fail. If an empirical bivariate copula is \dQuote{Tau'd} to itself, is \eqn{\tau_\mathbf{C} \equiv 1} guaranteed? The \eqn{\tau_\mathbf{C}} computation relies on numerical partial derivatives of the copula, whereas the \eqn{\gamma_\mathbf{C}} and \eqn{\rho_\mathbf{C}} use the copula itself. It seems in the end that use of sample versions of \eqn{\gamma_\mathbf{C}}, \eqn{\rho_\mathbf{C}}, and \eqn{\tau_\mathbf{C}} would be appropriate and leave the \eqn{\beta_\mathbf{C}} as either copula or direct sample computation (see \bold{Examples}).

\emph{SPECIAL DEMONSTRATION 1}---Given the following,
\preformatted{
  para <- list(cop1=PLACKETTcop, cop2=PLACKETTcop, para1=c(14.5),para2=c(1.45),
               alpha=0.51, beta=0.15, kappa=0.45, gamma=0.78)
  D <- simCOP(n=500, cop=composite3COP, para=para, cex=0.5, col=1, pch=16)
}
the two different call types to \code{isCOP.PQD} for an empirical copula are illustrative:
\preformatted{
  global.only <- isCOP.PQD(cop=EMPIRcop, para=D, empirical=TRUE)
}
and
\preformatted{
  PQD.list <- isCOP.PQD(cop=EMPIRcop, para=D, empirical=TRUE, uv=D)
  points(D, col=PQD.list$local.PQD+2, lwd=2) # red (if present) is local NQD
}
which in the former only returns the global PQD and the later returns an \R \code{list} with global (\code{global.PQD}), local (\code{local.PQD} as well as \code{local.NQD}), and the four statistics (\code{beta} \eqn{\beta_\mathbf{C}}, \code{gamma} \eqn{\gamma_\mathbf{C}}, \code{rho} \eqn{\rho_\mathbf{C}}, \code{tau} \eqn{\tau_\mathbf{C}}) used to determine global PQD.


\emph{SPECIAL DEMONSTRATION 1}---Lastly, the \code{ctype=}\code{"bernstein"} argument to the empirical copula can be used. Repeated iterations of the following will show that local quadrant dependency can appear slightly different when the \code{bernstein} argument is present. The simulation sample size is reduced considerably for this second example because of the CPU effort triggered by the \emph{Bernstein extension} (see \code{\link{EMPIRcop}}) having been turned on.
\preformatted{
  para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop, para1=14.5, para2=1.45,
               alpha=0.51, beta=0.15, kappa=0.45, gamma=0.78)
  D <- simCOP(n=50, cop=composite3COP, para=para, cex=0.5, col=1, pch=16)
  PQD.A<- isCOP.PQD(cop=EMPIRcop, para=D, empirical=TRUE, uv=D)
  points(D, col=PQD.A$local.PQD+2, lwd=2) # red (if present) is local NQD
  PQD.B<- isCOP.PQD(cop=EMPIRcop,para=D,empirical=TRUE,uv=D,ctype="bernstein")
  points(D, col=PQD.B$local.PQD+2, lwd=1, pch=3, cex=1.5)
}
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{blomCOP}}, \code{\link{giniCOP}}, \code{\link{rhoCOP}}, \code{\link{tauCOP}}, \code{\link{isCOP.LTD}}, \code{\link{isCOP.RTI}}}
\examples{
\dontrun{
isCOP.PQD(cop=PSP) # TRUE}

\dontrun{
# Example concerning Empirical Bivariate Copula and sample versions for comparison.
set.seed(10); n <- 1000
para <- list(cop1=PLACKETTcop, cop2=PLACKETTcop, para1=0.145,  para2=1.45,
             alpha=0.81, beta=0.8)
D <- simCOP(n=n, cop=composite2COP, para=para, cex=0.5, col=rgb(0,0,0,0.2), pch=16)
#tauCOP(cop=EMPIRcop, para=D)   # ??? but == 1
cor(D$U, D$V, method="kendall") # -0.3224705
blomCOP(cop=EMPIRcop, para=D)   # -0.332
giniCOP(cop=EMPIRcop, para=D)   # -0.3692037
GINI <- sum(abs(rank(D$U)+rank(D$V)-n-1)) - sum(abs(rank(D$U)-rank(D$V)))
print(GINI/as.integer(n^2/2))   # -0.369996
rhoCOP(cop=EMPIRcop, para=D)    # ??? but fails
cor(D$U, D$V, method="spearman")      # -0.456694
lmomco::lcomoms2(D)$T2     #  1.0000000 -0.4568357
                           # -0.4567859  1.0000000}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
