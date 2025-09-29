\encoding{utf8}
\name{FRcop}
\alias{FRcop}
\title{The Frank Copula}
\description{
The \emph{Frank copula} (Joe, 2014, p. 165) is

\deqn{\mathbf{C}_{\Theta}(u,v) = \mathbf{FR}(u,v) =
-\frac{1}{\Theta}\mathrm{log}\biggl[\frac{1 - \mathrm{e}^{-\Theta} - \bigl(1 - \mathrm{e}^{-\Theta u}\bigr) \bigl(1 - \mathrm{e}^{-\Theta v}\bigr)}{1 - \mathrm{e}^{-\Theta}}\biggr]\mbox{,}
}

where \eqn{\Theta \in [-\infty, +\infty], \Theta \ne 0}. The copula, as \eqn{\Theta \rightarrow -\infty} limits, to the \emph{countermonotonicity coupla} (\eqn{\mathbf{W}(u,v)}; \code{\link{W}}), as \eqn{\Theta \rightarrow 0^{\pm}} limits to the \emph{independence copula} (\eqn{\mathbf{\Pi}(u,v)}; \code{\link{P}}), and as \eqn{\Theta \rightarrow +\infty}, limits to the \emph{comonotonicity copula} (\eqn{\mathbf{M}(u,v)};  \code{\link{M}}). The parameter \eqn{\Theta} is readily computed from a \emph{Kendall Tau} (\code{\link{tauCOP}}) by numerical methods as  \eqn{\tau_{\mathbf{C}}(\Theta) = 1 + 4\Theta^{-1}[D_1(\Theta) - 1]}
or from a \emph{Spearman Rho} (\code{\link{rhoCOP}}) as
\eqn{\rho_{\mathbf{C}}(\Theta) = 1 + 4\Theta^{-1}[D_2(\Theta) - D_1(\Theta)]} for \emph{Debye function} as
\deqn{
  D_k(x, k) = k x^{-k} \int_0^x t^k \bigl(\mathrm{e}^{t} - 1\bigr)^{-1}\, \mathrm{d}t\mbox{.}
}
}
\usage{
FRcop(u, v, para=NULL, rhotau=NULL, userhotau_chk=TRUE,
            cortype=c("kendall", "spearman", "tau", "rho"), ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (single element) of parameters---the \eqn{\Theta} parameter of the copula;}
  \item{rhotau}{Optional Kendall Tau or Spearman Rho and parameter \code{para} is returned depending on the setting of \code{cortype}. The \code{u} and \code{v} can be used for estimation of the parameter as computed through the setting of \code{cortype};}
  \item{cortype}{A character string controlling, if the parameter is not given, to use a Kendall Tau or Spearman Rho for estimation of the parameter. The name of this argument is reflective of an internal call to \code{stats::cor()} to the correlation (association) setting for Kendall Tau or Spearman Rho;}
  \item{userhotau_chk}{A logical to trigger computation of Kendall Tau for the given parameter and used as a secondary check on numerical limits of the copula implementation for the package; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned. Otherwise if \code{tau} is given, then the \eqn{\Theta} is computed and a \code{list} having
  \item{para}{The parameter \eqn{\Theta}, and}
  \item{tau}{Kendall Tau.}
and if \code{para=NULL} and \code{tau=NULL}, then the values within \code{u} and \code{v} are used to compute Kendall Tau and then compute the parameter, and these are returned in the aforementioned list. Or if \code{rho} is given, then the \eqn{\Theta} is computed and a similar \code{list} is returned having similar structure but with Spearman Rho instead.
}
\note{
Whether because of default directions of derivative in \code{\link{derCOP}} for partial derivative of the copula or other numerical challenges, the implementation uses the negative parameter whether positive or not and rotates the copula as needed for complete operation of \code{\link{simCOP}}. Several default limiting conditions are in the sources before conversion to independence, perfect positive dependence, or perfect negative dependence.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{M}}, \code{\link{P}}, \code{\link{W}}
}
\examples{
UV  <- simCOP(n=1000, cop=FRcop, para=20, seed=1)
print(FRcop(UV[,1], UV[,2], cortype="kendall" )$tau) # 0.8072993
print(FRcop(UV[,1], UV[,2], cortype="spearman")$rho) # 0.9536648

\dontrun{
  # Joe (2014) example follows by extending the functionality of copBasic
  # into a 3-dimensional C-vine copula using Frank and Gumbel copulas and
  # Kendall Taus and Kendall Partial Taus. The example is expanded to show
  # how advanced features of copBasic can be incorporated for asymmetry
  # (if needed) and reflections (rotations) of copula:
  nsim <- 5000 # number of simulations but not too large for long CPU
  d <- 3       # three dimensions in Joe (2014, algorithm 15) implementation
  GHcop_para <- GHcop(tau   =0.5                   )$para # theta = 2
  FRcop_para <- FRcop(rhotau=0.7, cortype="kendall")$para # theta = 11.41155
  # Substantial notation complexity by structuring the C-vine by matrices for
  # copula families and their parameters then dial in asymmetry by "breves"
  # (see copBasic::breveCOP) and then reflection (rotation ability in the
  # copulas themselves). With zero breves, we have no asymmetry (permutation)
  # and we are going with the basic copula formula, so Reflects are all 1.
  Cops     <- matrix(c(NA,    "GHcop",     "FRcop",
                       NA,         NA,         "M",
                       NA,         NA,         NA),  nrow=d, ncol=d, byrow=TRUE)
  Thetas   <- matrix(c(NA, GHcop_para,  FRcop_para,
                       NA,         NA,          NA,
                       NA,         NA,          NA), nrow=d, ncol=d, byrow=TRUE)
  Breves   <- matrix(c(NA,          0,           0,
                       NA,         NA,           0,
                       NA,         NA,          NA), nrow=d, ncol=d, byrow=TRUE)
  Reflects <- matrix(c(NA,          1,           1,
                       NA,         NA,           1,
                       NA,         NA,          NA), nrow=d, ncol=d, byrow=TRUE)
  # see copBasic::breveCOP(), copBasic::GHcop(), copBasic::M()
  # see also copBasic::COP() for how reflection work within the package
  set.seed(1); U <- NULL # seed and vector of the U_{(1,2,3)} probabilities
  for(i in 1:nsim) {
    w <- runif(d); u <- rep(NA, d); u[1] <- w[1]
    # looks messy but just a way dump a host of "parameters" including family
    # itself down into copBasic logic
    para <- list(cop=breveCOP, para=list(cop=eval(parse(text=Cops[1,2])),
            para=Thetas[1,2], breve=Breves[1,2], reflect=Reflects[1,2]))
    u[2] <- derCOPinv(u[1], t=w[2], cop=COP, para=para) # conditional quantiles
    for(j in 3:d) {
      q <- w[j]
      for(l in (j-1):1) { # Joe (2014, algorithm 16)
        para <- list(cop=breveCOP, para=list(cop=eval(parse(text=Cops[l,j])),
                para=Thetas[l,j], breve=Breves[l,j], reflect=Reflects[l,j]))
        q <- derCOPinv(w[l], t=q, cop=COP, para=para)   # conditional quantiles
      }
      u[j] <- q
    }
    U <- rbind(U, matrix(u, ncol=d, byrow=TRUE))
  }
  U <- as.data.frame( U )
  names( U ) <- paste0("U", seq_len(d))

  # Kendall Partial Tau; Joe (2014, p. 8.66, Theorem 8.66)
  "pc3dCOP" <- function(taujk, tauhj, tauhk) { # close attention to h subscript!
    etajk <- sin( pi*taujk / 2) # Expansion to trig by Joe (2014, theorem 8.19),
    etahj <- sin( pi*tauhj / 2) # Joe says this definition "might be"
    etahk <- sin( pi*tauhk / 2) # better than partial tau on scores.
    (2/pi) * asin( (etajk - etahj*etahk) / sqrt( (1-etahj^2) * (1-etahk^2) ) )
  }
  # Joe (2014, pp. 404-405)
  # Kendall partial tau but needs pairwise Kendall taus to compute partial tau.
  "PairwiseTaus" <- function(U) {
    ktau <- matrix(NA, nrow=d, ncol=d)
    for(j in 1:d) { for(l in 1:d) {
        if(j <  l) next
        if(j == l) { ktau[l,j] <- 1; next } # 1s on diagonal as required.
        ktau[l,j] <- ktau[j,l] <- cor(U[,l], U[,j], method="kendall") # symmetrical
    } }
    return(ktau)
  }
  PairwiseTaus  <- PairwiseTaus(U) # [1,2] and [1,3] are 0.5 and 0.7 matching
  # requirement and more importantly for Joe (2014, table 8.3, p. 405). Now, [2,3] is
  # about 0.783 which again matches Joe's results of 0.782.
  Tau23given1pc <- pc3dCOP(PairwiseTaus[2,3], PairwiseTaus[2,1], PairwiseTaus[3,1])
  # Joe (2014, table 8.3, p. 405) reports tau^{pc}_{jk;h} as 0.848 by giant
  # simulation and we get about 0.852199 for some modest nsim and set.seed(1). }
}
\keyword{Frank copula}
\keyword{copula (formulas)}
\keyword{copula}
\concept{Frank copula}
\keyword{Joe (2014) Examples and Exercises}
\keyword{Debye function}
\keyword{C-vine copula}
\keyword{Vine Copula}
\keyword{copula (comprehensive)}
\concept{comprehensive copula}
\concept{C-vine}
\concept{C-vines}
