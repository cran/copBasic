\encoding{utf8}
\name{isCOP.radsym}
\alias{isCOP.radsym}
\title{Is a Copula Radially Symmetric}
\description{
Numerically set a logical whether a copula is \emph{radially symmetric} (Nelsen, 2006, p. 37) [\emph{reflection symmetric}, Joe (2014, p. 64)]. A copula \eqn{\mathbf{C}(u,v)} is radially symmetric if and only if for any \eqn{\{u,v\} \in [0,1]} either of the following hold
 \deqn{\mathbf{C}(u,v) = u + v - 1 + \mathbf{C}(1-u, 1-v)}
 or
 \deqn{u + v - 1 + \mathbf{C}(1-u, 1-v) - \mathbf{C}(u,v) \equiv 0\mbox{.}}

Thus, if the equality of the copula \eqn{\mathbf{C}(u,v) = \hat{\mathbf{C}}(u,v)} (the \emph{survival copula}), then radial symmetry exists: \code{\link{COP}} \eqn{=} \code{\link{surCOP}} or \eqn{\mathbf{C}(u,v) = \hat{\mathbf{C}}(1-u,1-v)}. The computation is (can be) CPU intensive.
}
\usage{
isCOP.radsym(cop=NULL, para=NULL, delta=0.005, tol=1e-4, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters, if needed, to pass to the copula;}
  \item{delta}{The increments of  \eqn{\{u,v\} \mapsto [0+\Delta\delta, 1-\Delta\delta, \Delta\delta]};}
  \item{tol}{A tolerance on the check for symmetry, default 1 part in 10,000, which is the test for the \eqn{\equiv 0} (zero equivalence, see source code); and}
  \item{...}{Additional arguments to pass to the copula or derivative of a copula function.}
}
\value{
  A logical \code{TRUE} or \code{FALSE} is returned.
}
\note{
An open research question possibly exists: \emph{Is a radially symmetric copula characterized by the L-comoments for orders} \eqn{r{\ge}3} \emph{as having values of zero}? The author asks this question partly out of intuition stemming from numerical experiments (some not show here) suggesting this condition, and review of copula literature does not seem to directly address this question. Let us consider the two symmetrical copulas: the parameterless \eqn{\mathbf{PSP}(u,v)} (see \code{\link{PSP}}) and the single parameter \eqn{\mathbf{PL}(u,v; \Theta)} (see \code{\link{PLACKETTcop}}) with the \eqn{\Theta_\mathbf{PL} = 4.708664} (see \code{\link{rhoCOP}}). The two copulas have different radial symmetries as shown below.
\preformatted{
  plackpar <- PLACKETTpar(rho=rhoCOP(cop=PSP)) # Spearman Rho = 0.4784176
  isCOP.radsym(cop=PSP)                        # FALSE
  isCOP.radsym(cop=PLACKETTcop, para=plackpar) # TRUE
}
Now, let us compute the L-comoments from the \pkg{lmomco} \R package for \eqn{n=10{,}000} simulations from each copula. The L-correlations are each about 0.48, which agree with the given \eqn{\rho_\mathbf{C}}.
\preformatted{
  set.seed(639)
  UVa <- simCOP(n=10000, cop=PSP,         para=NA,       graphics=FALSE)
  set.seed(639)
  UVb <- simCOP(n=10000, cop=PLACKETTcop, para=plackpar, graphics=FALSE)
  lmomco::lcomoms2(UVa, nmom=4)$T3[2,1] # Only show L-coskew of V wrt U.
  lmomco::lcomoms2(UVb, nmom=4)$T3[2,1] # Only show L-coskew of V wrt U.
}
The L-coskew for the \eqn{\mathbf{PSP}} is about \eqn{-0.129} and that for the \eqn{\mathbf{PL}} copula is about zero (\eqn{<0.0029}). L-cokurtosis provides a similar result if \code{T3} is changed to \code{T4}. The \eqn{\mathbf{PSP}} L-cokurtosis is about \eqn{0.041}, whereas the \eqn{\mathbf{PL}} L-cokurtosis is about \eqn{<0.0037}.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{isCOP.permsym}}}
\examples{
# Radially symmetry is computationally intensive and relies on a gridded [0,1]x[0,1]
# space and laborious check on equality. Thus these examples are commented out for
# R --timings check. Note that the proof of radial symmetry absent of algebraic
# manipulation or verification is difficult and subject to the fineness of the grid
# to find a nonequality from which to immediately conclude FALSE.
\dontrun{
isCOP.radsym(cop=P) # TRUE

para <- list(cop1=PLACKETTcop, cop2=M, para1=c(.3), para2=NA, alpha=0.8, beta=0.5)
isCOP.radsym(composite2COP, para=para) # FALSE
}
\dontrun{
gh <- simCOP(n=34, cop=GHcop, para=theta, ploton=FALSE, points=FALSE) * 150
# Pretend gh is real data, the * 150 is to clearly get into an arbitrary unit system.

# The sort=FALSE is critical in the following two calls
fakeU <- lmomco::pp(gh[,1], sort=FALSE) # Weibull plotting position i/(n+1)
fakeV <- lmomco::pp(gh[,2], sort=FALSE) # Weibull plotting position i/(n+1)
uv <- data.frame(U=fakeU, V=fakeV); # our U-statistics

set.seed(120); theta <- 2
gh <- simCOP(n=34, cop=GHcop, para=theta, ploton=FALSE, points=FALSE) * 150
# Pretend psp is real data, the * 150 is to clearly get into an arbitrary unit system.

# The sort=FALSE is critical in the following two calls
fakeU <- lmomco::pp(gh[,1], sort=FALSE) # Weibull plotting position i/(n+1)
fakeV <- lmomco::pp(gh[,2], sort=FALSE) # Weibull plotting position i/(n+1)
uv <- data.frame(U=fakeU, V=fakeV); # our U-statistics

isCOP.radsym(cop=EMPIRcop, para=uv) # FALSE
isCOP.LTD(cop=EMPIRcop,    para=uv) # TRUE
isCOP.RTI(cop=EMPIRcop,    para=uv) # FALSE
isCOP.PQD(cop=EMPIRcop,    para=uv,
                    empirical=TRUE) # TRUE
  # Blomqvist's Beta = 0.2941
  #     Gini's Gamma = 0.5606
  #   Spearman's Rho = 0.6584
  #    Kendall's Tau = 0.5045

isCOP.radsym(cop=GHcop, para=theta) # FALSE
isCOP.LTD(cop=GHcop,    para=theta) # TRUE
isCOP.RTI(cop=GHcop,    para=theta) # TRUE
isCOP.PQD(cop=GHcop,    para=theta) # TRUE
  # Blomqvist's Beta = 0.5009
  #     Gini's Gamma = 0.5591
  #   Spearman's Rho = 0.6822
  #    Kendall's Tau = 0.5000

# Notice that isCOP.RTI is not the same for empirical and theoretical.
# This shows the difficulty in tail dependence parameter estimation for
# small samples (see Salvadori et al., 2007 p. 175).}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{copula (symmetry)}
