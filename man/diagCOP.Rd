\name{diagCOP}
\alias{diagCOP}
\title{ The Diagonals of a Copula }
\description{
Compute the primary or secondary diagonals of copula \eqn{\mathbf{C}(u,v)}.
The primary diagonal is
\deqn{\mathbf{\delta}_\mathbf{C}(t) = \mathbf{C}(t,t)\mbox{, and}}
the secondary diagonal is
\deqn{\mathbf{\delta}_\mathbf{C}(t) = \mathbf{C}(t,1-t)\mbox{.}}
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
  \item{delt}{The increment of the level curves to plot, defaults to 5-percent intervals; and}
  \item{...}{Additional arguments to pass to the \code{plot()} and \code{lines()} functions.}
}
\value{
  A \code{list} of the \eqn{\mathbf{\delta}_\mathbf{C}(t,t)} (primary diagonal) or \eqn{\mathbf{\delta}_\mathbf{C}(t,1-t)} (secondary diagonal) for the \code{t} values are returned.
}
\note{
See pp. 12 and 16 of the reference.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{sectionCOP}}}
\examples{
# Compute and show the primary diagonal of the W, P, M, and PSP
# copulas on the same plot
D <- diagCOP(cop=W,   lwd=2)
D <- diagCOP(cop=P,   lty=2, ploton=FALSE)
D <- diagCOP(cop=M,   col=2, ploton=FALSE)
D <- diagCOP(cop=PSP, col=3, ploton=FALSE)
mtext("PRIMARY DIAGONAL OF SIMPLE COPULAS")

# Compute and show the secondary diagonal of the W, P, M, and PSP
# copulas on the same plot
D <- diagCOP(cop=W,   lwd=2, secondary=TRUE)
D <- diagCOP(cop=P,   lty=2, secondary=TRUE, ploton=FALSE)
D <- diagCOP(cop=M,   col=2, secondary=TRUE, ploton=FALSE)
D <- diagCOP(cop=PSP, col=3, secondary=TRUE, ploton=FALSE)
mtext("SECONDARY DIAGONAL OF SIMPLE COPULAS")

}
\keyword{distribution}
\keyword{multivariate}
