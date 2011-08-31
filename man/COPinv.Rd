\name{COPinv}
\alias{COPinv}
\title{ The Inverse of a Copula for V with respect to U }
\description{
Compute the inverse of a copula for \eqn{V} with respect to \eqn{U} or
\deqn{t = \mathbf{C}(u=U,v)\mbox{,}}
and solving for \eqn{v}.
}
\usage{
COPinv(cop=NULL, u, t, para=NULL, ...)
}
\arguments{
  \item{cop}{A copula function,}
  \item{u}{A nonexceedance probability in \eqn{X} direction,}
  \item{t}{A nonexceedance probability in \eqn{t} direction,}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula, and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for \eqn{v} is returned.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{COP}},
         \code{\link{COPinv2}},
         \code{\link{level.curvesCOP}}
}
\examples{

level.curvesCOP(cop=P, para=NULL, delt=.02) # Product copula

level.curvesCOP(cop=PSP, para=NULL) # PSP copula


# Produce the level curves for the Plackett copula for
# a theta parameter < 0 (positive association)
para  <- c(0.0001)
level.curvesCOP(cop=PLACKETTcop, para=para)


# Produce the level curves for the Plackett copula for
# a theta parameter > 0 (positive association)
para  <- c(10)
level.curvesCOP(cop=PLACKETTcop, para=para)

# Now create a composite copula
para <- list(cop1=PLACKETTcop,  cop2=PSP,
             para1=c(0.4),    para2=NULL,
             alpha=0.6,        beta=0.09)
level.curvesCOP(cop=composite2COP, para=para, delt=.1)


}
\keyword{distribution}
\keyword{multivariate}
