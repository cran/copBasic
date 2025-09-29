\encoding{utf8}
\name{isCOP.radsym}
\alias{isCOP.radsym}
\title{Is a Copula Radially Symmetric}
\description{
Numerically set a logical whether a copula is \emph{radially symmetric} (Nelsen, 2006, p. 37) [\emph{reflection symmetric}, Joe (2014, p. 64)] (refer also to \emph{permutation symmetric}, \code{\link{isCOP.permsym}}). A copula \eqn{\mathbf{C}(u,v)} is radially symmetric if and only if for any \eqn{\{u,v\} \in [0,1]} either of the following hold
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
\bold{L-comoments and Radial and Permutation Symmetry} --- A natural question exists: \emph{Is a radially symmetric copula characterized by the L-comoments for orders} \eqn{r{\ge}3} \emph{as having values of zero}? Apparently, yes, and review of copula literature does not seem to directly address this question. Let us consider the two symmetrical copulas: the parameterless \eqn{\mathbf{PSP}(u,v)} (see \code{\link{PSP}}) and the single parameter \eqn{\mathbf{PL}(u,v; \Theta)} (see \code{\link{PLcop}}) with the \eqn{\Theta_\mathbf{PL} = 4.708664} (\code{\link{rhoCOP}}). The two copulas have different radial symmetries as shown below.
\preformatted{
  J <- PLACKETTpar(rho=rhoCOP(cop=PSP)) # Spearman Rho = 0.4784176
  isCOP.radsym(     cop=PSP)            # FALSE
  isCOP.radsym(     cop=PLcop, para=J)  # TRUE
  UV <- simCOP(300, cop=PSP,   para=J, ploton=TRUE,  pch= 1, col="blue", seed=1)
  UV <- simCOP(300, cop=PLcop, para=J, ploton=FALSE, pch=16, col="red" , seed=1)
}
Now, let us compute the L-comoments (\eqn{\tau_k^{[12]|[21]}}) (\code{\link{lcomCOP}}) from each copula in the code that follows. The L-correlations (Spearman Rho) are each about 0.478, which agree with the given \eqn{\rho_\mathbf{C}}. The L-coskews (T3_PSP[12,21]) for the \eqn{\mathbf{PSP}} are about \eqn{-0.129} with those for the \eqn{\mathbf{PL}} copula at zero. The \eqn{\mathbf{PSP}} L-cokurtoses (T4_PSP[12,21]) are about \eqn{0.047}, whereas those for the \eqn{\mathbf{PL}} are zero. The [12], [21] equalities result from \emph{permutation symmetry} (\code{\link{isCOP.permsym}}), but the nonzero L-cokurtoses for the \eqn{\mathbf{PSP}} result from radial asymmetry.
\preformatted{
  lcmPSP <- lcomCOP(cop=PSP)
  lcmPLA <- lcomCOP(cop=PLcop, para=J)
  lcmPSP <-  lapply(lcmPSP, function(i) round(i, digits=5))
  lcmPLA <-  lapply(lcmPLA, function(i) round(i, digits=5))
  message( # L-correlations (T2), L-coskews (T3), L-cokurtoses (T4)
   "T2[12]_PSP == T2_PSP[21] =", lcmPSP$lcomUV[2], "=", lcmPSP$lcomVU[2], "\n",
   "T2[12]_PLA == T2_PLA[21] =", lcmPLA$lcomUV[2], "=", lcmPLA$lcomVU[2], "\n",
   "T3[12]_PSP == T3_PSP[21] =", lcmPSP$lcomUV[3], "=", lcmPSP$lcomVU[3], "\n",
   "T3[12]_PLA == T3_PLA[21] =", lcmPLA$lcomUV[3], "=", lcmPLA$lcomVU[3], "\n",
   "T4[12]_PSP == T4_PSP[21] =", lcmPSP$lcomUV[4], "=", lcmPSP$lcomVU[4], "\n",
   "T4[12]_PLA == T4_PLA[21] =", lcmPLA$lcomUV[4], "=", lcmPLA$lcomVU[4], "\n")
}

Now, \code{isCOP.radsym()} is false for all rotations (reflections) of the \eqn{\mathbf{PSP}}, which requires that in magnitude that the L-coskews are equal but with reflections 3 and 4 rotating the coupla to a \eqn{\rho_\mathbf{C} = \tau_2^{[12]} = \tau_2^{[21]} = -0.478}. The L-cokurtoses are all equal with a change in sign with reflections 3 and 4. The \eqn{\mathbf{PSP}} inherently has a bilateral symmetry about any diagonal, which makes the \eqn{r{\ge}3} L-comoments as mutual equals in magnitude.
\preformatted{
  lcmPSP1 <- lcomCOP(cop=COP, para=list(cop=PSP, reflect=1)) # default
  lcmPSP2 <- lcomCOP(cop=COP, para=list(cop=PSP, reflect=2)) # 180 degrees
  lcmPSP3 <- lcomCOP(cop=COP, para=list(cop=PSP, reflect=3)) # -90 degrees
  lcmPSP4 <- lcomCOP(cop=COP, para=list(cop=PSP, reflect=4)) # +90 degrees
  lcmPSP1 <-  lapply(lcmPSP1, function(i) round(i, digits=5))
  lcmPSP2 <-  lapply(lcmPSP2, function(i) round(i, digits=5))
  lcmPSP3 <-  lapply(lcmPSP3, function(i) round(i, digits=5))
  lcmPSP4 <-  lapply(lcmPSP4, function(i) round(i, digits=5))
  message( #  L-coskews (T3), reflections 1~, 2~, 3~, 4~
   "1~T3[12] == T3[21] = ", lcmPSP1$lcomUV[3], " = ", lcmPSP1$lcomVU[3], "\n",
   "2~T3[12] == T3[21] = ", lcmPSP2$lcomUV[3], " = ", lcmPSP2$lcomVU[3], "\n",
   "3~T3[12] != T3[21] = ", lcmPSP3$lcomUV[3], " = ", lcmPSP3$lcomVU[3], "\n",
   "4~T3[12] != T3[21] = ", lcmPSP4$lcomUV[3], " = ", lcmPSP4$lcomVU[3], "\n")
  # 1*T3[12] == T3[21] = -0.12949 = -0.12949
  # 2*T3[12] == T3[21] =  0.12949 =  0.12949
  # 3*T3[12] != T3[21] =  0.12949 = -0.12949
  # 4*T3[12] != T3[21] = -0.12949 =  0.12949
  message( # L-cokurtoses (T4), reflections 1~, 2~, 3~, 4~
   "1~T4[12] == T3[21] =", lcmPSP1$lcomUV[4], " = ", lcmPSP1$lcomVU[4], "\n",
   "2~T4[12] == T3[21] =", lcmPSP2$lcomUV[4], " = ", lcmPSP2$lcomVU[4], "\n",
   "3~T4[12] == T3[21] =", lcmPSP3$lcomUV[4], " = ", lcmPSP3$lcomVU[4], "\n",
   "4~T4[12] == T3[21] =", lcmPSP4$lcomUV[4], " = ", lcmPSP4$lcomVU[4], "\n")
  # 1~T4[12] == T3[21] =  0.04677 =  0.04677
  # 2~T4[12] == T3[21] =  0.04677 =  0.04677
  # 3~T4[12] == T3[21] = -0.04677 = -0.04677
  # 4~T4[12] == T3[21] = -0.04677 = -0.04677
}
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{isCOP.permsym}}, \code{\link{lcomCOP}}
}
\examples{
# Radially symmetry is computationally intensive and relies on a gridded [0,1]x[0,1]
# space and laborious check on equality. Thus these examples are commented out for the
# R --timings check. Note, proof of radial symmetry absent of algebraic manipulation
# or verification is difficult and subject to the grid fineness to find a nonequality
# from which to immediately conclude FALSE. L-comoments could be used for simultaneous
# radial and permutation symmetry assessment and even beyond those by detection of
# no bilateral symmetry from any perspective (refer to the Note section above).
\dontrun{
  isCOP.radsym(cop=P) # TRUE

  para <- list(cop1=PLcop, cop2=M, para1=c(.03), para2=NA, alpha=0.8, beta=0.5)
  isCOP.radsym(      composite2COP, para=para) # FALSE
  isCOP.permsym(     composite2COP, para=para) # FALSE
  lcm <- lcomCOP(cop=composite2COP, para=para)
  lcm <- lapply(lcm, function(i) round(i, digits=5))
  message("    SpearmanRho, Lcoskew,  Lcokurt\n",
          "Tk[12] ", paste(lcm$lcomUV[2:4], collapse=", "), "\n",
          "Tk[21] ", paste(lcm$lcomVU[2:4], collapse=", "))
  #     SpearmanRho, Lcoskew,  Lcokurt  # Magitudes for L-comoment k>=3
  # Tk[12] -0.25236, 0.22828,  0.01796  # are not equal because there is
  # Tk[21] -0.25236, 0.17612, -0.05165  # no bilateral symmetry.}

\dontrun{
  set.seed(120); theta <- 2
  gh <- simCOP(n=34, cop=GHcop, para=theta, ploton=FALSE, points=FALSE) * 150
  # Pretend gh is real data, the * 150 is to clearly get into an arbitrary unit system.

  # The sort=FALSE is critical in the following two calls
  fakeU <- lmomco::pp(gh[,1], sort=FALSE) # Weibull plotting position i/(n+1)
  fakeV <- lmomco::pp(gh[,2], sort=FALSE) # Weibull plotting position i/(n+1)
  uv <- data.frame(U=fakeU, V=fakeV); # our U-statistics

  isCOP.radsym(cop=EMPIRcop, para=uv) # FALSE
  isCOP.LTD(cop=EMPIRcop,    para=uv) # TRUE
  isCOP.RTI(cop=EMPIRcop,    para=uv) # FALSE
  isCOP.PQD(cop=EMPIRcop,    para=uv, # TRUE
            empirical=TRUE) # need to set this true to avoid integrate()
   # Blomqvist's Beta = 0.2941
   #     Gini's Gamma = 0.5606
   #   Spearman's Rho = 0.6584
   #    Kendall's Tau = 0.5045

  isCOP.radsym(cop=GHcop, para=theta) # FALSE
  isCOP.LTD(   cop=GHcop, para=theta) # TRUE
  isCOP.RTI(   cop=GHcop, para=theta) # TRUE
  isCOP.PQD(   cop=GHcop, para=theta) # TRUE
    # Blomqvist's Beta = 0.5009
    #     Gini's Gamma = 0.5591
    #   Spearman's Rho = 0.6822
    #    Kendall's Tau = 0.5000

  # Notice that isCOP.RTI is not the same for empirical and theoretical.
  # This shows the difficulty in tail dependence parameter estimation for
  # small samples (refer also to Salvadori et al., 2007 p. 175).}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{copula (symmetry)}
\keyword{L-comoments}
\keyword{Lcomoments}
