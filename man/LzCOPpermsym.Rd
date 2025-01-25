\encoding{utf8}
\name{LzCOPpermsym}
\alias{LzCOPpermsym}
\title{Maximum Asymmetry Measure (or Vector) of a Copula by Exchangability}
\description{
Compute a measure of maximum exchangable asymmetry of a copula \eqn{\mathbf{C}_\Theta} using \emph{exchangability} (\emph{permutation symmetry}) according to De Baets and De Meyer (2017) by
\deqn{\mu_{\infty\mathbf{C}}^{\mathrm{permsym}} = \mu_\infty^{\mathrm{permsym}} = 3 \times \mathrm{max}\bigl(\,|\,\mathbf{C}_\Theta(u,v) -
                       \mathbf{C}_\Theta(v,u)\,|\,\bigr)}
for \eqn{(u,v) \in \mathcal{I}^2}. De Baets and De Meyer comment that among many asymmetric metrics with copulas that \eqn{\mu_\infty^{\mathrm{permsym}}} is \dQuote{by far the most interesting} (De Baets and De Meyer, 2017, p. 36). The \eqn{3} multiplier in the definition ensures that \eqn{\mu_\infty^{\mathrm{permsym}} \in [0, 1]}. Those authors also conclude that exchangability of random variables, in general, is not a desired property in statistical models, and they use the \eqn{\mu_\infty} notation in lieu of \eqn{L_\infty^{\mathrm{permsym}}} (see documentation related to \code{\link{LpCOPpermsym}}). The term \dQuote{Permutation-Mu} is used for this measure in \code{\link{copBasic-package}} and in similar contexts.
}
\usage{
LzCOPpermsym(cop=NULL, para=NULL, n=5E4,
             type=c("halton", "sobol", "torus", "runif"),
             as.abs=TRUE, as.vec=FALSE, as.mat=FALSE, plot=FALSE, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters, if and as needed, to pass to the copula;}
  \item{n}{The simulation size. The default seems sufficient for many practical applications but is suboptimal because the maximum operator in the definition is expected to potentially underestimate the true maximum. When a vector is returned, the default simulation size appears sufficient for many parameter estimation schemes;}
  \item{type}{The type of random number generator on \eqn{\mathcal{I}^2} for computing the maximum (apparent) (see argument \code{n}) or a vector of signed differences (see \bold{Details});}
  \item{as.abs}{A logical controlling whether the absolute value operation in the \eqn{\mu_\infty^{\mathrm{permsym}}} definition is used. This feature permits flexibility retaining the sign of asymmetry;}
  \item{as.vec}{A logical to disable the maximum operation but instead return the a vector of signed differences in the exchanged variables. If this argument is set true, then \code{as.abs} will be set false. The return of a vector of signed differences (still multiplied by 3) could be useful in parameter estimation schemes with a similar vector from an \emph{empirical copula} (\code{\link{EMPIRcop}}) (see \bold{Details});}
  \item{as.mat}{A logical to disable the maximum operation (like \code{as.vec}), but instead return a matrix of the \eqn{\mathcal{I}^2} values with third column as the vector of signed differences. If this argument is set true, then \code{as.abs} will be set false;}
  \item{plot}{A logical to create a plot of the \eqn{\mathcal{I}^2} domain used in the simulation with a plot title showing the \code{type} argument setting;}
  \item{...}{Additional arguments to pass to support flexible implementation.}
}
\details{
\emph{EFFECT OF RANDOM NUMBER GENERATION}---Package \pkg{randtoolbox} provides for random number generation on forms different than simply simulating \emph{uniform independent random variables} for \eqn{\mathcal{I}^2}. The \emph{Halton}, \emph{Sobol}, and \emph{Torus} types are implemented. The \code{plot} argument is useful for the user to see the differences in how these generators canvas the \eqn{\mathcal{I}^2} domain.

The default is Halton, which visually appears to better canvas \eqn{\mathcal{I}^2} without the clumping that simple uniform random variables does and without the larger gaps of Sobol or Torus. Testing indicates that Halton might generally require the smallest simulation size of the others with simple uniform random variables potentially being the worst and hence such is not the default. Exceptions surely exist depending on the style of the asymmetry. Nevertheless, Halton, Sobol, and Torus produce more consistent estimation behavior with each having a monotone approach towards the true maximum than simple uniform random variables.

The following example is a useful illustration of an asymmetrical \emph{Clayton copula} (\eqn{\mathbf{CL}(u,v; \Theta)}, \code{\link{CLcop}}) by \emph{composition of a single copula} (\code{\link{composite1COP}}) with the theorical \eqn{\mu_\infty^{\mathrm{permsym}}} maxima computed by large sample simulation. A user might explore the effect of the random number generation by changing the \code{type} variable.
\preformatted{
  type <- "halton"
  para <- list(cop=CLcop, para=20, alpha=0.3, beta=0.1) # asymmetrical Clayton
  ti <- LzCOPpermsym(cop=composite1COP, para=para, n=2E6, type=type) # large
  ns <- as.integer( 10^seq(1, 4, by=0.05) ) # sequence of simulation sizes
  mi <- sapply(ns, function(n) { # produce vector of maxima for simulation size
               LzCOPpermsym(cop=composite1COP, para=para, n=n, type=type) })
  ylim <- range(c(0.06, mi, ti)) # vertical limits to ensure visibility
  plot(ns, mi, log="x", pch=21, bg=grey(0.9), ylim=ylim, main=type,
       xlab="Simulation size", ylab="Maximum asymmetry measure")
  abline(h=ti, lwd=3, col="seagreen") # large sample size estimate in green
}

\emph{COPULA PARAMETER ESTIMATION}---Parameter estimation using \emph{signed permutation asymmetry vector} can readily be accomplished. In the self-contained example below, we will assume a parent of \emph{Gumbel--Hougaard} (\eqn{\mathbf{GH}(u,v; \Theta)}, \code{\link{GHcop}}) extended to asymmetry by using three parameters (\eqn{\Theta = (10, 0.8, 0.6)}. Imagine that we unfortunately have a very small sample size (\eqn{n = 100}) as \dQuote{hundred years of data.} The small sample size facilitates the use of the \emph{checkboard empirical copula} (\code{\link{EMPIRcop}}); the sample size is small enough that the checkerboard helps smooth through ties. The simulation size for \code{LzCOPpermsym} is set \dQuote{large} as presumed by the existing default.

\preformatted{
  para <- c(10, 0.8, 0.6)                       # parameters of the parent
  nsam <- 100; seed <- 2; nsim <- 5000          # note a change from default
  as.vec <- TRUE       # set to FALSE to use just Permutation-Mu
  rhoP <- rhoCOP(cop=GHcop, para=para)          # parent Spearman Rho
  UVsS <- simCOP(cop=GHcop, para=para, n=nsam, seed=seed) # simulate a sample
  rhoS <- rhoCOP(as.sample=TRUE,     para=UVsS) # sample Spearman Rho
  infS <- LzCOPpermsym(cop=EMPIRcop, para=UVsS, n=nsim, type="halton",
                       as.vec=as.vec, ctype="checkerboard")
  # empirical copula used and returning signed asymmetry vector
  # transformation and re-transformation, GHcop paras >1; [0,1]; and [0,1]
  tparf <- function(par) c(exp(par[1]) + 1,  pnorm( par[2] ), pnorm( par[3] ))
  rparf <- function(par) c(log(par[1]  - 1), qnorm( par[2] ), qnorm( par[3] ))

  ofunc <- function(par, norho=FALSE) { # objective function
    mypara <- tparf(par)
    rhoT   <- rhoCOP(cop=GHcop, para=mypara)    # simulated Spearman Rho
    infT   <- LzCOPpermsym(cop=GHcop, para=mypara, n=nsim, type="halton",
                           as.vec=as.vec)
    err    <-    mean( (infT - infS)^2 )        # mean squared errors
    ifelse(norho, err, (rhoT - rhoS)^2 + err)   # with Spearman Rho or not
  }

  init.par <- rparf(c(2, 0.5, 0.5)); rt <- NULL # initial parameter guess
  try( rt <- optim(init.par, ofunc, norho=FALSE) ) # 3D optimization
  if(is.null(rt)) stop("fatal, optim() returned NULL")
  # construct GHcop parameters from optimization with re-transformation
  sara <- tparf(rt$par)
  rhoT <- rhoCOP(cop=GHcop, para=sara)          # theoretical Spearman Rho
  UVsT <- simCOP(cop=GHcop, para=sara, n=nsam, seed=seed,  # same seed sim by
                 cex=0.3, pch=16, col="red", ploton=FALSE) # est. parameters

  mara <- mleCOP(UVsS, cop=GHcop, init.para=init.par, parafn=tparf)$para

  level.curvesCOP(cop=GHcop, para=para)
  level.curvesCOP(cop=GHcop, para=sara, ploton=FALSE, col="red" ) # perm diffs
  level.curvesCOP(cop=GHcop, para=mara, ploton=FALSE, col="blue") #   mleCOP()
}

Comparison of level curves between the known parent, the parameter estimation using function \code{LzCOPpermsym}, and the \emph{maximum likelihood} by \code{\link{mleCOP}} shows that signed asymmetry differences can be used for parameter estimation. One could use the maximum as in the definition, but for purposes of high-dimensional optimization, using the vector might be better to prevent local minima (less optimal solutions) being found if the \eqn{\mu_\infty^{\mathrm{permsym}}} was used. Because vectors of differences between empirical copula and the fitted copula are involved, measures of fit using such differences are expected to be more favorable to optimization than using \code{LzCOPpermsym} than say maximum likelihood. The measures of fit AIC (\code{\link{aicCOP}}), BIC (\code{\link{bicCOP}}), and RMSE (\code{\link{rmseCOP}}), for example, are often, smaller for the \code{sara} fitted parameters than for the \code{mara} fitted (maximum likelihood). Finally, setting \code{as.vec <- FALSE}, re-running, and thus using \eqn{\mu_\infty^{\mathrm{permsym}}}, will likely show parameter estimates, visible through the level curves, that are much less favorable.

\emph{RELATION TO ANOTHER DISTANCES}---The documentation for \code{\link{LpCOPpermsym}} lists a supremum definition \eqn{L_\infty^{\mathrm{permsym}}}, which is like \eqn{\mu_\infty^{\mathrm{permsym}}} but lacks the multiplier of 3. However, that documentation mentions a ratio of 1/3 being as upper bounds and hence the De Baets and De Meyer (2017) reasoning for the 3 multiplier to rescale \eqn{\mu_\infty^{\mathrm{permsym}} \in (0,1)}. The simple interrelations between the two functions are explored in the following example:

\preformatted{
  para <- c(30, 0.2, 0.95); n <- 5E4
  p <- 1
  mean(abs(LzCOPpermsym(cop=GHcop, para=para, n=n,
                          as.vec=TRUE)/3)^p)^(1/p)   # 0.01867929
           LpCOPpermsym(cop=GHcop, para=para, p=p)   # 0.01867940
  p <- 3
  mean(abs(LzCOPpermsym(cop=GHcop, para=para, n=n,
                          as.vec=TRUE)/3)^p)^(1/p)   # 0.02649376
           LpCOPpermsym(cop=GHcop, para=para, p=p)   # 0.02649317
}

The critical note is that \code{\link{LpCOPpermsym}} is the integral of the absolute differences in permuted differences across \eqn{\mathcal{I}^2}. Hence, it is an expectation. The \code{LzCOPpermsym} is difference because of the maximum of the differences. The computations in the example above show how the same information can be extracted from the two functions. De Baets and De Meyer (2017) do not make reference to raising and then rooting by the power \eqn{p} as shown. The examples here provide credence to the default setting of \code{n} (simulation size) for several significant figures of similarity.
}
\value{
  A scalar value for the measure is returned or other as dictated by arguments.
}
\references{
De Baets, B., and De Meyer, H., 2017, Chapter 3---A look at copulas in a curved mirror: New York, Springer, ISBN 978--3--319--64221--5, pp. 33--47.
}
\author{W.H. Asquith}
\seealso{\code{\link{LpCOPpermsym}}, \code{\link{isCOP.permsym}}}
\examples{
LzCOPpermsym(cop=PSP)                            # 0, permutation symmetric
LzCOPpermsym(cop=GHcop, para=c(10, 0.9, 0.3))    # 0.17722861

# See also results of
# isCOP.permsym(cop=PSP)                         # TRUE
# isCOP.permsym(cop=GHcop, para=c(10, 0.9, 0.3)) # FALSE

\dontrun{
  sapply(1:4, function(r) { # Four rotations of a Galambos copula
    Lz <- LzCOPpermsym(cop=COP, para=list(cop=GLcop, para=2, reflect=r))
    UV <- simCOP(1000, cop=COP, para=list(cop=GLcop, para=2, reflect=r))
    mtext(paste0("Reflection ", r, " : Permutation-Mu =", Lz)); Lz })
  # [1] 0.00000000 0.00000000 0.07430326 0.07430326 }
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{copula (symmetry)}
\keyword{copula (fitting)}
\keyword{Theory Examples (reflection/rotation)}
