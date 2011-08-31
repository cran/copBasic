\name{level.curvesCOP}
\alias{level.curvesCOP}
\title{ Compute and Plot Level Curves of a Copula V with respect to U }
\description{
Compute and plot level curves of a copula for \eqn{V} with respect to \eqn{U}. The level curves at \eqn{t} are defined for \eqn{U \in [0,1]} by
\deqn{t = \mathbf{C}(u=U,v)\mbox{,}}
and solving for \eqn{v}.
}
\usage{
level.curvesCOP(cop=NULL, para=NULL, ploton=TRUE, lines=TRUE,
                plotMW=FALSE, ramp=TRUE, delu=0.001, delt=0.10,
                getlevel=NULL, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{ploton}{A logical to toggle on the plot;}
  \item{lines}{A logical to toggle calls to the \code{lines} function to draw the lines;}
  \item{plotMW}{A logical to toggle to use \code{abline} to plot cross lines for the \code{\link{M}} and \code{\link{W}} copulas;}
  \item{ramp}{A logical to toggle whether the level curves are ramped in thickness according to the probability of the line;}
  \item{delu}{The increment for \eqn{U \in [0,1]};}
  \item{delt}{The increment of the level curves to plot, defaults to 10-percent intervals;}
  \item{getlevel}{If defined and level exists upon stepping through using \code{delt}, then the level curve at the \code{getlevel} is returned in a \code{list} data structure; and}
  \item{...}{Additional arguments to pass to the \code{lines} function.}
}
\value{
  No values are returned. This function is used for its side effects.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{COP}},
         \code{\link{COPinv}},
         \code{\link{level.curvesCOP}}
}
\examples{
level.curvesCOP(cop=M, para=NULL, delt=.02) # Upper bounds copula


D <- level.curvesCOP(cop=P, getlevel=0.56)
str(D) # empty
D <- level.curvesCOP(cop=P, getlevel=0.5)
str(D) # contains stuff
}
\keyword{misc}
