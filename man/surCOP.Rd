\name{surCOP}
\alias{surCOP}
\title{ The Survival Copula }
\description{
Compute the survival copula from a copula 

\deqn{\mathrm{Pr}[U > u, V > v] = \hat{\mathbf{C}}(u,v) = u + v - 1 + \mathbf{C}(1-u,1-v)}

The survival copula is an expression of the probability that both \eqn{U > v} and \eqn{U > v} when the arguments to \eqn{\hat{\mathbf{C}}(u,v)} are exceedance probabilities, which is obviously unlike a copula that has \eqn{U \le u} and \eqn{V \le v}.

Alternatively, the probability that both \eqn{U > u} and \eqn{V > v} can be solved using just a copula: \eqn{1 - u - v + \mathbf{C}(u,v)} when the arguments to \eqn{\mathbf{C}(u,v)} are nonexceedance probabilities.
}
\usage{
surCOP(cop=NULL, u, v, exceedance=TRUE, ...)
}
\arguments{
  \item{cop}{A copula function,}
  \item{u}{An exceedance probability in X direction,}
  \item{v}{An exceedance probability in Y direction,}
  \item{exceedance}{Are \code{u} and \code{v} really in exceedance probability? If \code{FALSE}, then the complements of the two are made internally and the nonexceedances can thus be passed, and}
  \item{...}{Additional arguments to pass (such as parameters, if needed, for the copula in the form of a list.}
}
\value{
  The value for the survival copula is returned.
}
\note{
See pp.~32--34 of the reference. The author (Asquith) finds the use of exceedance probabilities confusing having grown up in a strictly nonexceedance world. This function and \code{coCOP} have the \code{exceedance} argument to serve as a reminder that survival copula as usually defined uses exceedance probabilities.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269~p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{coCOP}}, \code{\link{duCOP}}, \code{\link{W}}, \code{\link{P}}, \code{\link{M}}, \code{\link{PSP}}}
\examples{
u <- 1 - runif(1) # make exceedance
v <- 1 - runif(1) # make exceedance
cat(c("u and v are",
      round(u,digits=4),"and",
      round(v,digits=4),"exceedance probabilities\n"))

surCOP(cop=W,u,v)
surCOP(cop=P,u,v)
surCOP(cop=M,u,v)
surCOP(cop=PSP,u,v)

# computation using manual manipulation to nonexceedance probability
1 - (1-u) - (1-v) + COP(cop=PSP,(1-u),(1-v))

# computation using manual manipulation to nonexceedance probability
surCOP(cop=PSP,u,v, exceedance=FALSE)

}
\keyword{distribution}
\keyword{multivariate}
