\encoding{utf8}
\name{EMPIRgrid_lkup}
\alias{EMPIRgrid_lkup}
\title{Empirical Copula by Bilinear Interpolation of Gridded Bivariate Empirical Copula}
\description{
Compute joint probability through a gridded \emph{empirical copula} \eqn{\mathbf{C}_n(u,v)} using bilinear interpolation (vectorized, no \code{for} looping). The function is designed to act like \code{\link{EMPIRcop}}, but in lieu of consuming the bivariate data in \code{para} argument, the \code{EMPIRgrid_lkup} operates off a precomputed gridded list representation of the empirical copula computed either from \code{\link{EMPIRgrid}} (legacy) or \code{\link{EMPIRgrid_fast}}. Given a gridded empirical copula denoted as \eqn{\ddot{\mathbf{C}}_n} and bounding cells of a rectangle \eqn{11} (lower left), \eqn{12} (lower right), \eqn{21} (upper left), and \eqn{22}, the \eqn{\mathbf{C}_n(u,v)} can be estimated by bilinear interpolation in a weighted mean form as:

\deqn{\mathbf{C}_n(u,v) \approx w_{11}\times\ddot{\mathbf{C}}_{11} +
                                w_{12}\times\ddot{\mathbf{C}}_{12} +
                                w_{21}\times\ddot{\mathbf{C}}_{21} +
                                w_{22}\times\ddot{\mathbf{C}}_{22}\text{,}}
for which the weights are
\deqn{w_{11} = \frac{(u_2 - u  )(v_2 - v  )}{(u_2 - u_1)(v_2 - v_1)}\text{,\ }
      w_{12} = \frac{(u_2 - u  )(v   - v_1)}{(u_2 - u_1)(v_2 - v_1)}\text{,}}
\deqn{w_{21} = \frac{(u   - u_1)(v_2 - v  )}{(u_2 - u_1)(v_2 - v_1)}\text{,\ and\ }
      w_{22} = \frac{(u   - u_1)(v   - v_1)}{(u_2 - u_1)(v_2 - v_1)}\text{.}}
}
\usage{
EMPIRgrid_lkup(u, v, para=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A gridded empirical data structure from \code{\link{EMPIRgrid}} and \code{\link{EMPIRgrid_fast}}; and}
  \item{...}{Additional arguments to pass to \code{\link{EMPIRgrid_fast}} if \code{para} is null.}
}
\value{
  Value(s) for the copula are returned.
}
\author{W.H. Asquith}
\seealso{\code{\link{EMPIRgrid}}, \code{\link{EMPIRgrid_fast}}}
\examples{
\dontrun{
  n <- 2000
  para  <- list(cop=GHcop, para=c(2.3,1.5), breve=0.5)
  uv    <- simCOP(n, cop=breveCOP, para=para, graphics=TRUE)
  system.time(gridn <- EMPIRgrid(     uv, ctype="1/n", deluv=1/2000))
  system.time(gridf <- EMPIRgrid_fast(uv, ctype="1/n"))
  # gridf should be shown to be over 100 times faster.
  copn  <- EMPIRgrid_lkup(uv[,1], uv[,2], para=gridn)
  copf  <- EMPIRgrid_lkup(uv[,1], uv[,2], para=gridf)
  plot(copn, copf); abline(0,1) # equal value line }

\dontrun{
  n <- 21 # Make a small sample demonstration and exhaustive list of
  # comparisons in empirical copula computation styles.
  para  <- list(cop=GHcop, para=c(2.3,1.5), breve=0.5)
  uv    <- simCOP(n, cop=breveCOP, para=para,    graphics=TRUE )
  gridn <- EMPIRgrid_fast(uv, ctype="1/n",       gridonly=FALSE)
  gridw <- EMPIRgrid_fast(uv, ctype="bernstein", gridonly=FALSE)
  copw  <- EMPIRcop(uv[,1], uv[,2], para=uv, ctype="weibull"     )
  copn  <- EMPIRcop(uv[,1], uv[,2], para=uv, ctype="1/n"         )
  coph  <- EMPIRcop(uv[,1], uv[,2], para=uv, ctype="hazen"       )
  copb  <- EMPIRcop(uv[,1], uv[,2], para=uv, ctype="bernstein"   )
  copc  <- EMPIRcop(uv[,1], uv[,2], para=uv, ctype="checkerboard")
  copgn <- EMPIRgrid_lkup(uv[,1], uv[,2], para=gridn)
  copgb <- EMPIRgrid_lkup(uv[,1], uv[,2], para=gridw)

  plot(  copgn, copw, pch=24, col="seagreen4", bg="seagreen2")
  points(copgn, copn, pch=24, col="red4"     , bg="red1"     )
  points(copgn, coph, pch=24, col="blue4"    , bg="blue1"    )
  points(copgn, copb, pch=24, col="orchid4"  , bg="orchid1"  )
  points(copgn, copc, pch=24, col="salmon4"  , bg="salmon1"  )

  points(copgb, copw, pch=25, col="seagreen", bg="white")
  points(copgb, copn, pch=25, col="red"     , bg="white")
  points(copgb, coph, pch=25, col="blue"    , bg="white")
  points(copgb, copb, pch=25, col="orchid1" , bg="white")
  points(copgb, copc, pch=25, col="salmon"  , bg="white")

  abline(0,1, lty=2) # equal value line }
}
\keyword{empirical copula}
\keyword{empirical copula (utility)}
\keyword{copulatic surface}
\keyword{gridded empirical copula}
