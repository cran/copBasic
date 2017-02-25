\name{gridCOP}
\alias{gridCOP}
\title{ Compute a Copula on a Grid }
\description{
Compute a grid of copula values. This function has the primary intention of supporting 3-D renderings or 2-D images of the \emph{copulatic surface}. Users should be aware of the convention of the placement of the plotting origin and the various plotting mechanisms available to them in \R. By convention copulatic surfaces start in lower left corner for \eqn{u = v = 0}, but matrix conventions (or at least how some functions plot matrices) start with the origin in the upper left.
}
\usage{
gridCOP(cop=NULL, para=NULL, delta=0.05, transpose=TRUE, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{delta}{The \eqn{\Delta u = \Delta v} of the grid edges;}
  \item{transpose}{A logical to transpose the returned grid. This is needed if functions such as \code{image()} in \R are to be used for visualization (see last example in \bold{Examples} with \code{\link{composite2COP}}); and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The values for \eqn{\mathbf{C}(u,v)} are returned as a grid as an \R \code{matrix}.
}
\author{ W.H. Asquith}
\seealso{\code{\link{EMPIRcopdf}}
}
\examples{
\dontrun{
the.grid <- gridCOP(cop=PSP)
the.grid[1,1] <- 0 # replace the NaN
image(the.grid) # ramps to the upper right }

\dontrun{
# See this composite copula also used in densityCOPplot() documentation.
para <- list(alpha=0.15, beta=0.90, kappa=0.06, gamma=0.96,
             cop1=GHcop, cop2=PLACKETTcop, para1=5.5, para2=0.07)
GR <- gridCOP(cop=composite2COP, para=para, delta=0.005)
image(GR, col=terrain.colors(20)) # asymmetric, high curvature in top half }
}
\keyword{copula utility}
\keyword{copulatic surface}
