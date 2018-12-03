\encoding{utf8}
\name{isCOP.LTD}
\alias{isCOP.LTD}
\title{ Is a Copula Left-Tail Decreasing }
\description{
Numerically set a logical whether a copula is \emph{left-tail decreasing} (LTD) as described by Nelsen (2006, pp. 192--193) and Salvadori \emph{et al.} (2007, p. 222).  A copula \eqn{\mathbf{C}(u,v)} is left-tail decreasing for \eqn{\mathrm{LTD}(V{\mid}U)} if and only if for any \eqn{v \in [0,1]},
\deqn{\frac{\delta \mathbf{C}(u,v)}{\delta u} \le \frac{\mathbf{C}(u,v)}{u}}
for almost all \eqn{u \in [0,1]}. Similarly, a copula \eqn{\mathbf{C}(u,v)} is left-tail decreasing for \eqn{\mathrm{LTD}(U{\mid}V)} if and only if for any \eqn{u \in [0,1]},
\deqn{\frac{\delta \mathbf{C}(u,v)}{\delta v} \le \frac{\mathbf{C}(u,v)}{v}}
for almost all \eqn{v \in [0,1]} where the later definition is controlled by the \code{wrtV=TRUE} argument.

The LTD concept is associated with the concept of \emph{tail monotonicity} (Nelsen, 2006, p. 191). Specifically, but reference to Nelsen (2006) definitions and geometric interpretations is recommended, \eqn{\mathrm{LTD}(V{\mid}U)} (or \eqn{\mathrm{LTD}(V{\mid}U)}) means that the probability \eqn{P[Y \le y|X \le x]} (or \eqn{P[X \le x|Y \le y]}) is a nonincreasing function of \eqn{x} (or \eqn{y}) for all \eqn{y} (or \eqn{x}).

A positive LTD of either \eqn{\mathrm{LTD}(V{\mid}U)} or \eqn{\mathrm{LTD}(U{\mid}V)} implies positively quadrant dependency (PQD, \code{\link{isCOP.PQD}}) but the condition of PQD does not imply LTD. Finally, the accuracy of the numerical assessment of the returned logical by \code{isCOP.LTD} is dependent on the the \dQuote{smallness} of the \code{delta} argument passed into the function.
}
\usage{
isCOP.LTD(cop=NULL, para=NULL, wrtV=FALSE, delta=0.005, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters, if needed, to pass to the copula;}
  \item{wrtV}{A logical to toggle between with respect to \eqn{v} or \eqn{u} (default);}
  \item{delta}{The increment of \eqn{\{u,v\} \mapsto [0+\Delta\delta, 1-\Delta\delta, \Delta\delta]} set by \code{wrtV}; and}
  \item{...}{Additional arguments to pass to the copula or derivative of a copula function.}
}
\value{
  A logical \code{TRUE} or \code{FALSE} is returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in nature---An approach using copulas: Dordrecht, Netherlands, Springer, Water Science and Technology Library 56, 292 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{isCOP.RTI}}, \code{\link{isCOP.PQD}}}
\examples{
\dontrun{
isCOP.LTD(cop=P, delta=0.01) # independence should be FALSE
# Positive association
isCOP.LTD(cop=PSP)                               # TRUE
# Negative association Plackett
isCOP.LTD(cop=PLACKETTcop, para=0.15)            # FALSE
# Positive association Plackett
isCOP.LTD(cop=PLACKETTcop, para=15)              # TRUE
# Negative association Plackett
isCOP.LTD(cop=PLACKETTcop, wrtv=TRUE, para=0.15) # FALSE
# Positive association Plackett
isCOP.LTD(cop=PLACKETTcop, wrtV=TRUE, para=15)   # TRUE}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}

