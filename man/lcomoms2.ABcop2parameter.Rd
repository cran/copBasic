\name{lcomoms2.ABcop2parameter}
\alias{lcomoms2.ABcop2parameter}
\title{ Convert L-comoments to Parameters of Alpha-Beta Compositions of Two One-Parameter Copulas }
\description{
\emph{EXPERIMENTAL}---This function converts the \emph{L-comoments} of a bivariate sample to the four parameters of a composition of two one-parameter copulas. Critical inputs are of course the first three dimensionless L-comoments: \emph{L-correlation}, \emph{L-coskew}, and \emph{L-cokurtosis}. The most complex input is the \code{solutionenvir}, which is an \code{environment} containing arbitrarily long, but individual tables, of L-comoment and parameter pairings. These pairings could be computed from the examples in \code{\link{simcompositeCOP}}.

The individual tables are prescanned for potentially acceptable solutions and the absolute additive error of both L-comoments for a given order is controlled by the \code{tNeps} arguments. The default values seem acceptable. The purpose of the prescanning is to reduce the computation space from perhaps millions of solutions to a few orders of magnitude. The computation of the solution error can be further controlled by \eqn{X} or \eqn{u} with respect to \eqn{Y} or \eqn{v} using the \code{comptNerrXY} arguments, but experiments thus far indicate that the defaults are likely the most desired. A solution \dQuote{matching} the L-correlation is always sought; thus there is no \code{uset2err} argument. The arguments \code{uset3err} and \code{uset4err} provide some level of granular control on addition error minimization; the defaults seek to \dQuote{match} L-coskew and ignore L-cokurtosis. The \code{setreturn} controls which rank of computed solution is returned; users might want to manually inspect a few of the most favorable solutions, which can be done by the \code{setreturn} or inspection of the returned object from the \code{lcomoms2.cop2parameter} function. The examples are detailed and self-contained to the \pkg{copBasic} package; curious users are asked to test these.
}
\usage{
lcomoms2.ABcop2parameter(solutionenvir=NULL,
                         T2.12=NULL, T2.21=NULL,
                         T3.12=NULL, T3.21=NULL,
                         T4.12=NULL, T4.21=NULL,
                         t2eps=0.1, t3eps=0.1, t4eps=0.1,
                         compt2erruv=TRUE, compt2errvu=TRUE,
                         compt3erruv=TRUE, compt3errvu=TRUE,
                         compt4erruv=TRUE, compt4errvu=TRUE,
                         uset3err=TRUE, uset4err=FALSE,
                         setreturn=1, maxtokeep=1e5)
}
\arguments{
  \item{solutionenvir}{The environment containing solutions;}
  \item{T2.12}{L-correlation \eqn{\tau_2^{[12]}};}
  \item{T2.21}{L-correlation \eqn{\tau_2^{[21]}};}
  \item{T3.12}{L-coskew \eqn{\tau_3^{[12]}};}
  \item{T3.21}{L-coskew \eqn{\tau_3^{[21]}};}
  \item{T4.12}{L-cokurtosis \eqn{\tau_4^{[12]}};}
  \item{T4.21}{L-cokurtosis \eqn{\tau_4^{[21]}};}
  \item{t2eps}{An error term in which to pick a potential solution as close enough on preliminary processing for \eqn{\tau_2^{[1 \leftrightarrow 2]}};}
  \item{t3eps}{An error term in which to pick a potential solution as close enough on preliminary processing for \eqn{\tau_3^{[1 \leftrightarrow 2]}};}
  \item{t4eps}{An error term in which to pick a potential solution as close enough on preliminary processing for \eqn{\tau_4^{[1 \leftrightarrow 2]}};}
  \item{compt2erruv}{Compute an L-correlation error using the 1 with respect to 2 (or \eqn{u} wrt \eqn{v});}
  \item{compt2errvu}{Compute an L-correlation error using the 2 with respect to 1 (or \eqn{v} wrt \eqn{u});}
  \item{compt3erruv}{Compute an L-coskew error using the 1 with respect to 2 (or \eqn{u} wrt \eqn{v});}
  \item{compt3errvu}{Compute an L-coskew error using the 2 with respect to 1 (or \eqn{v} wrt \eqn{u});}
  \item{compt4erruv}{Compute an L-cokurtosis error using the 1 with respect to 2 (or \eqn{u} wrt \eqn{v});}
  \item{compt4errvu}{Compute an L-cokurtosis error using the 2 with respect to 1 (or \eqn{v} wrt \eqn{u});}
  \item{uset3err}{Use the L-coskew error in the determination of the solution. The L-correlation error is always used;}
  \item{uset4err}{Use the L-cokurtosis error in the determination of the solution. The L-correlation error is always used;}
  \item{setreturn}{Set (index) number of the solution to return. The default of 1 returns the preferred solutions based on the controls for the minimization; and}
  \item{maxtokeep}{The value presets the number of rows in the solution matrix. This matrix is filled with potential solutions as the various subfiles of the \code{solutionenvir} are scanned. The matrix is trimmed of \code{NA}s and error trapping is in place for too small values of \code{maxtokeep}. The default value appears appropriate for the feeding of massively large simulated parameter spaces.}
}
\value{
  An \R \code{data.frame} is returned.
}
\references{
Asquith, W.H., 2011, Distributional analysis with L-moment statistics using the R environment for statistical computing: Createspace Independent Publishing Platform, ISBN 978--146350841--8.

Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{ W.H. Asquith }
\seealso{\code{\link{simCOP}}, \code{\link{simcompositeCOP}}, \code{\link{composite2COP}}}
\examples{
\dontrun{
# Build an initial parameter to L-comoment mapping table.
  mainpara <- list(cop1=PLACKETTcop, cop2=PLACKETTcop,
                   para1gen=function() { return(10^runif(1, min=-5, max=0)) },
                   para2gen=function() { return(10^runif(1, min=0,  max=5)) })
  nsim <- 1E4
  n <- 1000 # really use vastly larger sample size
  PlackettPlackettNP <- simcompositeCOP(n=sample.size.for.estimation,
        nsim=nsim, parent=mainpara)
  save(PlackettPlackettNP, file="PlackettPlackettNP.RData", compress="xz")

# Plackett-Plackett composited copula from the copBasic package
# Then create an environment to house the "table."
PlackettPlackett <- new.env()
assign("NeedToCreateForDemo", PlackettPlackettNP, envir=PlackettPlackett)
# Now that the table is assigned into the environment, the parameter
# estimation function can be used. In reality, a much much larger
# solution set is needed, but this effort is experimental.

# Now grab the closest Plackett-Plackett solution having the following six
# arbitrary L-comoments. Then simulate 1000 values and plot them to show
# the underlying bivariate distribution.
PPcop <- lcomoms2.ABcop2parameter(solutionenvir=PlackettPlackett,
                                  T2.12=-0.5059, T2.21=-0.5110,
                                  T3.12= 0.1500, T3.21= 0.1700,
                                  T4.12=-0.0500, T4.21= 0.0329,
                                  uset3err=TRUE, uset4err=TRUE)
# A user in encouraged to inspect the contents of PPcop to "assess" the
# solution by a method of L-comoments, we will now proceed with showing the
# copula via a simulation of the fitted version.
para <- list(cop1=PLACKETTcop, cop2=PLACKETTcop, alpha=PPcop$alpha, beta=PPcop$beta,
             para1=PPcop$Cop1Thetas, para2=PPcop$Cop2Thetas)

D <- simCOP(n=5000, cop=composite2COP, para=para, col=rgb(0,0,0,0.1), pch=16)
# The sample L-comoments of the fitted Plackett-Plackett may be found by
lmomco::lcomoms2(D, nmom=4) # from the lmomco package, and six sample values shown
T2.12 <- -0.5151547; T2.21 <- -0.5139863
T3.12 <-  0.1502336; T3.21 <-  0.1721355
T4.12 <- -0.0326277; T4.21 <-  0.0233979
PPcop <- lcomoms2.ABcop2parameter(solutionenvir=PlackettPlackett,
                                  T2.12=T2.12, T2.21=T2.21,
                                  T3.12=T3.12, T3.21=T3.21,
                                  T4.12=T4.12, T4.21=T4.21, uset4err=TRUE)
para <- list(cop1=PLACKETTcop, cop2=PLACKETTcop, alpha=PPcop$alpha, beta=PPcop$beta,
             para1=PPcop$Cop1Thetas, para2=PPcop$Cop2Thetas)
D <- simCOP(n=5000, cop=composite2COP, para=para, col=rgb(0,0,0,0.1), pch=16)
level.curvesCOP(cop=composite2COP, para=para, delt=.1, ploton=FALSE)
qua.regressCOP.draw(cop=composite2COP, para=para,
                    ploton=FALSE, f=seq(0.05, 0.95, by=0.05))
qua.regressCOP.draw(cop=composite2COP, para=para, wrtV=TRUE,
                    ploton=FALSE, f=seq(0.05, 0.95, by=0.05), col=c(3,2))
diag <- diagCOP(cop=composite2COP, para=para, ploton=FALSE, lwd=4)

image(gridCOP(cop=composite2COP, para=para), col=terrain.colors(20))
# One can inspect alternative solutions like this
# S <- PPcop$solutions$solutions[,1:16]
# B <- S[abs(S$t2.12res) < 0.02 & abs(S$t2.21res) < 0.02 &
#        abs(S$t3.12res) < 0.02 & abs(S$t3.21res) < 0.02, ]
#print(B) }
}
\keyword{L-comoments}
\keyword{copula composition}
