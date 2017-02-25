\name{diagCOP}
\alias{diagCOP}
\title{ The Diagonals of a Copula }
\description{
Compute the \emph{primary diagonal} or alternatively the \emph{secondary diagonal}  (Nelsen, 2006, pp. 12 and 16) of copula \eqn{\mathbf{C}(u,v)}.
The primary diagonal is defined as
\deqn{\mathbf{\delta}_\mathbf{C}(t) = \mathbf{C}(t,t)\mbox{,}}
and the secondary diagonal is defined as
\deqn{\mathbf{\delta}^{\star}_\mathbf{C}(t) = \mathbf{C}(t,1-t)\mbox{.}}
Plotting is provided by this function because the diagonals are such important visual attributes of a copula. This function computes whole diagonals. If individual values are desired, then users are asked to use function calls along the diagonal such as \code{COP(0.25,0.25, cop=P)} for the primary diagonal and \code{COP(0.25,1-0.25, cop=P)} for the secondary diagonal, where for both examples the \emph{independence copula} (\eqn{uv = \mathbf{\Pi}}; \code{\link{P}}) was chosen for purposes of clarification.

The \eqn{\mathbf{\delta}_\mathbf{C}(t)} is related to order statistics of the multivariate sample (here bivariate) (Durante and Sempi, 2015, p. 68). The probability for the maxima is
\eqn{\mathrm{Pr}[\mathrm{max}(u, v) \le t] = \mathbf{C}(t,t) = \mathbf{\delta}_\mathbf{C}(t) \mbox{\ and }}
the probability for the minima is
\eqn{\mathrm{Pr}[\mathrm{min}(u, v) \le t] = 2t - \mathbf{\delta}_\mathbf{C}(t)\mbox{.}}
}
\usage{
diagCOP(cop=NULL, para=NULL, secondary=FALSE,
        ploton=TRUE, lines=TRUE, delt=0.005, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters, if needed, to pass to the copula;}
  \item{secondary}{A logical to toggle the secondary diagonal;}
  \item{ploton}{A logical to toggle on the plot;}
  \item{lines}{Draw the lines of diagonal to the current device;}
  \item{delt}{The increment of the diagonal curve to plot, defaults to 0.5-percent intervals, which should be small enough to resolve fine curvature for many copulas in practice; and}
  \item{...}{Additional arguments to pass to the \code{plot()} and \code{lines()} functions in \R.}
}
\value{
  An \R \code{list} of the \eqn{t} values, \eqn{\mathbf{\delta}_\mathbf{C}(t,t)} (primary) or \eqn{\mathbf{\delta}^{\star}_\mathbf{C}(t,1-t)} (secondary diagonal), along with a tag as to which diagonal is returned.
}
\references{
Durante, F., and Sempi, C., 2015, Principles of copula theory: Boca Raton, CRC Press, 315 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{diagCOPatf}}, \code{\link{COP}}, \code{\link{sectionCOP}}}
\examples{
\dontrun{
# The primary diagonal of the W, P, M, and PSP copulas on the same plot
D <- diagCOP(cop=W,   lwd=2)
D <- diagCOP(cop=P,   lty=2, ploton=FALSE)
D <- diagCOP(cop=M,   col=2, ploton=FALSE)
D <- diagCOP(cop=PSP, col=3, ploton=FALSE)
mtext("PRIMARY DIAGONAL OF SIMPLE COPULAS") # four primary diagonals }

\dontrun{
# The secondary diagonal of the W, P, M, and PSP copulas on the same plot
D <- diagCOP(cop=W,   lwd=2, secondary=TRUE)
D <- diagCOP(cop=P,   lty=2, secondary=TRUE, ploton=FALSE)
D <- diagCOP(cop=M,   col=2, secondary=TRUE, ploton=FALSE)
D <- diagCOP(cop=PSP, col=3, secondary=TRUE, ploton=FALSE)
mtext("SECONDARY DIAGONAL OF SIMPLE COPULAS") # four secondary diagonals }
}
\keyword{visualization}
\keyword{diagnostics}
\keyword{copula (characteristics)}
