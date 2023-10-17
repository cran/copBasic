\encoding{utf8}
\name{convex2COP}
\alias{convex2COP}
\title{Convex Combination of Two Copulas}
\description{
The \emph{convex composition of two copulas} (Joe, 2014, p. 155) provides for some simple complexity extension between copula families. Let \eqn{\mathbf{A}} and \eqn{\mathbf{B}} be copulas with respective vectors of parameters \eqn{\Theta_\mathbf{A}} and \eqn{\Theta_\mathbf{B}}, then the convex combination of these copulas is

\deqn{\mathbf{C}^{\times}_{\alpha}(u,v) = \alpha \mathbf{A}(u, v; \Theta_\mathbf{A}) - (1-\alpha)\mathbf{B}(u,v; \Theta_\mathbf{B})\mbox{,}}

where \eqn{0 \le \alpha \le 1}. The generalization of this function for \eqn{N} number of copulas is provided by \code{\link{convexCOP}}.
}
\usage{
convex2COP(u,v, para, ...)
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
The following descriptions list in detail the structure and content of the \code{para} argument:
\describe{
  \item{\code{alpha}}{--- The \eqn{\alpha} compositing parameter;}
  \item{\code{cop1}}{--- Function of the first copula \eqn{\mathbf{A}};}
  \item{\code{cop2}}{--- Function of the second copula  \eqn{\mathbf{B}};}
  \item{\code{para1}}{--- Vector of parameters \eqn{\Theta_\mathbf{A}} for  \eqn{\mathbf{A}}; and}
  \item{\code{para2}}{--- Vector of parameters \eqn{\Theta_\mathbf{B}} for  \eqn{\mathbf{B}}.}
}
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{COP}}, \code{\link{breveCOP}}, \code{\link{convexCOP}}, \code{\link{composite1COP}}, \code{\link{composite2COP}}, \code{\link{composite3COP}}, \code{\link{FRECHETcop}}, \cr \code{\link{glueCOP}}}
\examples{
para <- list(alpha=0.24, cop1=FRECHETcop, para1=c(0.4, 0.56),
                         cop2=PSP,        para2=NA)
convex2COP(0.87,0.35, para=para) # 0.3188711
\dontrun{
# Suppose we have a target Kendall Tau of 1/3 and a Gumbel-Hougaard copula seems
# attractive but the GH has just too much upper tail dependency for comfort. We
# think from data analysis that an upper tail dependency that is weaker and near
# 2/10 is the better. Let us convex mix in a Plackett copula and optimize.
TargetTau <- tauCOP(cop=GHcop, para=1.5) # 1/3 (Kendall Tau)
taildepCOP(   cop=GHcop, para=1.5, plot = TRUE)$lambdaU  # 0.4126
TargetUpperTailDep <- 2/10

# **Serious CPU time pending for this example**
par <- c(-.10, 4.65) # Initial guess but the first parameter is in standard
# normal for optim() to keep us in the [0,1] domain when converted to probability.
# The guesses of -0.10 (standard deviation) for the convex parameter and 4.65 for
# the Plackett are based on a much longer search times as setup for this problem.
# The simplex for optim() is going to be close to the solution on startup.
"afunc" <- function(par) {
   para <- list(alpha=pnorm(par[1]), cop1=GHcop,       para1=1.5,
                                     cop2=PLACKETTcop, para2=par[2])
   tau  <- tauCOP(cop=convex2COP, para=para)
   taildep <- taildepCOP(cop=convex2COP, para=para, plot = FALSE)$lambdaU
   err <- sqrt((TargetTau - tau)^2 + (TargetUpperTailDep - taildep)^2)
   print(c(pnorm(par[1]), par[2], tau, taildep, err))
   return(err)
}
mysolution <- optim(par, afunc, control=list(abstol=1E-4))

para <- list(alpha=.4846902, cop1=GHcop,       para1=1.5,
                             cop2=PLACKETTcop, para2=4.711464)
UV <- simCOP(n=2500, cop=convex2COP, para=para, snv=TRUE) #}
}
\keyword{copula composition}
\keyword{copula composition (convex combination)}

