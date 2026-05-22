\encoding{utf8}
\name{HAIRPINcop}
\alias{HAIRPINcop}
\title{The Hairpin Copula}
\description{
The \emph{Hairpin copula} (Durante \emph{et al.}, 2014) is

\deqn{\mathbf{C}_{\Theta}(u,v) = \mathbf{HAIRPIN}(u,v) =
\mathrm{min}\bigl[u, v, \mathrm{mean}\bigl(f(u,v) \bigr) \bigr]\mbox{,}}

as vectorized for the pairs \eqn{(u_i, v_i)} for \eqn{i \in 1,2,\cdots}, and \eqn{f(x) = \alpha x^\phi}, \eqn{1 \le \alpha \le \infty}, and \eqn{1 \le \phi \le 2}. The copula, as \eqn{\alpha \rightarrow \infty} or \eqn{\phi \rightarrow 1} limits, to the \emph{comonotonicity coupla} (\eqn{\mathbf{M}(u,v)}; \code{\link{M}}).
}
\usage{
HAIRPINcop(u, v, para=c(1, 2, 1), ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (three element) of two parameters and the reflection: \eqn{\alpha} and \eqn{\phi} of the coupla then the reflection number \eqn{n \in 1, 2, 3, 4} following the reflection pattern of \code{\link{COP}}; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned.
}
\references{
Durante, F., \enc{Fernández-Sánchez}{Fernandez-Sanchez}, J., and Trutschnig, W., 2014, Multivariate copulas with hairpin support: Journal of Multivariate Analysis, v. 130, pp. 323--334, \doi{10.1016/j.jmva.2014.06.009}.}
\author{W.H. Asquith}
\seealso{\code{\link{M}}
}
\examples{
\dontrun{
  # Produce a Celtic Knot look through simulation -----------------------------
  nsim <- 1.5E4
  plot(c(0, 1), c(0, 1), type="n", bty="n",
       xaxt="n", yaxt="n", xaxs="r", yaxs="r", xlab="", ylab="")
  sapply(1:4, function(n) {
                  UV <- simCOP(nsim, cop=HAIRPINcop, para=c(1.12, 1.8, n),
                 seed=1, ploton=FALSE, pch=16, cex=0.6, col=n); return(n) })
  sapply(1:4, function(n) {
                  UV <- simCOP(nsim, cop=HAIRPINcop, para=c(1.12, 1.3, n),
                 seed=1, ploton=FALSE, pch=16, cex=0.6, col=n); return(n) }) # }
}
\keyword{Hairpin copula}
\keyword{copula (formulas)}
\keyword{copula}
