\name{convexCOP}
\alias{convexCOP}
\title{ Convex Combination of an Arbitrary Number of Copulas }
\description{
The \emph{convex composition of \eqn{N} number of copulas} (Salvadori \emph{et al.}, p. 132, 2007) provides for complexity extension between coupla families. Let \eqn{\mathbf{C}_{i}} be a copula with respective vector of parameters \eqn{\Theta_i}, then the convex combination of these copulas is

\deqn{\mathbf{C}^{\times}_{\omega}(u,v) = \sum_{i=1}^N \omega_i \mathbf{C}_{i}(u, v; \Theta_i)\mbox{,}}

where \eqn{\sum_{i=1}^N \omega = 1} for \eqn{N} number of copulas. The weights \eqn{\omega} are treated as \eqn{1/N} if the \code{weights} element is absent in the \R \code{list} argument \code{para}.
}
\usage{
convexCOP(u,v, para, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A special parameter \code{list} (see \bold{Note}); and}
  \item{...}{Additional arguments to pass to the copula.}
}
\value{
  Value(s) for the convex combination copula is returned.
}
\note{
The following descriptions list in detail the structure and content of the \code{para} argument but please reference the \bold{Examples} to see the \code{i} notation:
\describe{
  \item{\code{copi}}{--- The \eqn{i}th copula;}
  \item{\code{parai}}{--- Vector of parameters \eqn{\Theta_i}; and}
  \item{\code{weights}}{--- Optional vector of weights whose sum will be rescaled to unity, default is \eqn{1/N} for each weight.}
}
}
\references{
Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{convex2COP}} }
\examples{
# The copulas and parameters are named by sequence number appended to cop and para.
para1 <- list(cop1=GHcop, cop2=PLcop, para1=8, para2=.03, weights=c(.8,.2))
para2 <- list(cop1=GHcop, cop2=PLcop, para1=8, para2=.03, alpha=0.8)
H <- convexCOP( 0.6,0.4, para=para1)
G <- convex2COP(0.6,0.4, para=para2)
if( abs(H-G) <= 1e-6 )  message("They are equal.")

\dontrun{
# A convex combination of three copulas. A GHcop with strong positive association and
# a Plackett with strong negative association, and independence. The weights favor the
# GHcop but a little outlier and expansive spread is superimposed on the core trend.
para <- list(cop1=GHcop, cop2=PLcop, cop3=P,
             para1=8, para2=.03, para3=NA, weights=c(40,7,10))
UV <- simCOP(1000, cop=convexCOP, para=para, lwd=0.8) #}
}
\keyword{copula composition}
\keyword{copula composition (convex combination)}

