\name{coCOP}
\alias{coCOP}
\title{ The Co-Copula }
\description{
Compute the co-copula from a copula 

\deqn{\mathrm{Pr}[U > u\mid V > v] = \mathbf{C}^{\star}(u,v) = 1 - \mathbf{C(1-u,1-v)}}

The co-copula is the expression for the probability that either \eqn{U > u} or \eqn{V > v} when the arguments to \eqn{\mathbf{C}^{\star}(u,v)} are exceedance probabilities, which is obviously unlike a dual of a copula that has \eqn{U \le u} or \eqn{V \le v}.

Alternatively, the probability that either \eqn{U > u} or \eqn{V > v} can be solved using just a copula: \eqn{1 - \mathbf{C}(u,v)} when the arguments to \eqn{\mathbf{C}(u,v)} are nonexceedance probabilities.
}
\usage{
coCOP(cop=NULL, u, v, exceedance=TRUE, ...)
}
\arguments{
  \item{cop}{A copula function,}
  \item{u}{An exceedance probability in X direction,}
  \item{v}{An exceedance probability in Y direction,}
  \item{exceedance}{Are \code{u} and \code{v} really in exceedance probability? If \code{FALSE}, then the complements of the two are made internally and the nonexceedances can thus be passed, and}
  \item{...}{Additional arguments to pass (such as parameters, if needed, for the copula in the form of a list.}
}
\value{
  The value for the dual of a copula is returned.
}
\note{
See pp.~33--34 of the reference. The author (Asquith) finds the use of exceedance probabilities confusing having grown up in a strictly nonexceedance world. This this function and \code{surCOP} have the \code{exceedance} argument to serve as a reminder that co-copula as usually defined uses exceedance probabilities.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269~p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{surCOP}}, \code{\link{duCOP}}, \code{\link{W}}, \code{\link{P}}, \code{\link{M}}, \code{\link{PSP}}}
\examples{
u <- 1 - runif(1) # make exceedance
v <- 1 - runif(1) # make exceedance
cat(c("u and v are",
      round(u,digits=4),"and",
      round(v,digits=4),"exceedance probabilities\n"))

coCOP(cop=W,u,v)
coCOP(cop=P,u,v)
coCOP(cop=M,u,v)
coCOP(cop=PSP,u,v)

# computation using manual manipulation to nonexceedance probability
1 - COP(cop=PSP,(1-u),(1-v))

# computation using manual manipulation to nonexceedance probability
coCOP(cop=PSP,u,v, exceedance=FALSE)

}
\keyword{distribution}
\keyword{multivariate}
