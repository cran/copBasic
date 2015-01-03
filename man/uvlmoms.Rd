\encoding{utf8}
\name{uvlmoms}
\alias{uvlmoms}
\alias{uvskewness}
\title{ Bivariate Skewness after Joe (2015) or the Univariate L-moments of Combined U and V }
\description{
Joe (2015, pp. 65--66) suggests two quantile-based measures of \emph{bivariate skewness} defined for uniform random variables \eqn{u} and \eqn{v} combined as either \eqn{\psi_{u+v-1} = u + v - 1} or \eqn{\psi_{u-v} = u -v} for which the \eqn{\mathrm{E}[u] = \mathrm{E}[v] = 0}. The bivariate skewness is the quantity \eqn{\eta}:
\deqn{\eta(p; \psi) = \frac{x(1-p) - 2x(\frac{1}{2}) + x(p)}{x(1-p) - x(p)} \mbox{,}}
where \eqn{0 < p < \frac{1}{2}}, \eqn{x(F)} is the quantile function for nonexceedance probability \eqn{F} for either the quantities \eqn{X = \psi_{u+v-1}} or \eqn{X = \psi_{u-v}} using either the empirical quantile function or a fitted distribution. Structurally the above definition for \eqn{\eta} based on quantiles is oft shown in comparative literature concerning L-moments. But why stop there? Why not compute the L-moments themselves to arbitrary order for \eqn{\eta} by either definition (the \code{uvlmoms} variation)? Why not fit a distribution to the computed L-moments for estimation of \eqn{x(F)}? Or simply compute \dQuote{skewness} according to the definition above (the \code{uvskewness} variation).
}
\usage{
uvlmoms(u,v=NULL,    umv=TRUE, p=NA,   type="gno", getlmoms=TRUE,  ...)

uvskewness(u,v=NULL, umv=TRUE, p=0.05, type=6,     getlmoms=FALSE, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction and if \code{NULL} then \code{u} is treated as a two column \R \code{data.frame};}
  \item{umv}{A logical controlling the computation of \eqn{\psi}: \eqn{\psi = u + v - 1} (\code{umv = TRUE}) or \eqn{\psi = u - v} (\code{umv = FALSE});}
  \item{p}{A suggested \eqn{p} value is \code{p = 0.05}. If \code{is.na(NA)}, then \code{getlmoms} is set to \code{TRUE} (see below);}
  \item{type}{The \code{type} argument is mutable. Either it is an integer between 1 and 9 selecting one of the nine quantile algorithms described for the \code{quantile} function in \R. The default 6 uses the Weibull plotting positions and differs from \R's default of 7. Otherwise \code{type} must be a valid distribution abbreviation for the \pkg{lmomco} package as in the abbreviation list \code{dist.list} function of that package. The \code{gno} shown is for the Generalized Normal distribution;}
  \item{getlmoms}{A logical triggering whether the L-moments of either \eqn{\psi_{u+v-1}} or \eqn{\psi_{u - v}} are returned instead computing the above definition of \dQuote{skewness;} and}
  \item{...}{Additional arguments to pass to the \pkg{lmomco} function \code{lmoms}, such as the number of L-moments \code{nmoms}.}
}
\value{
  An \R \code{list} of the univariate L-moments of \eqn{\eta} is returned (see documentation for \code{lmoms} in the \pkg{lmomco} package). Or the skewness of \eqn{\eta} can be either (1) based on the empirical distribution based on plotting positions by the \code{quantile} function of \R using the \code{type} as described, or (2) based on the fitted quantile function for the parameters of a distribution for the \pkg{lmomco} package.
}
\references{
Joe, H., 2015, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{COP}}
}
\examples{
UV <- simCOP(n=100, cop=GHcop, para=1.5, graphics=FALSE)
# lmr <- uvlmoms(UV) # commented out just for speed
uvskewness(UV, p=0.10)
uvskewness(UV, p=0.10, type="gno")
\dontrun{
ps <- seq(0.01,0.49,by=0.01)
ETA <- sapply(1:length(ps), function(i) { uvskewness(UV, p=ps[i], type=5, uvm1=FALSE) } )
plot(ps, ETA, type="l", xlab="P FACTOR", ylab="BIVARIATE SKEWNESS")
}
}
\keyword{bivariate asymmetry (measures)}
\keyword{bivariate skewness}
\keyword{L-moments of combined U and V}

