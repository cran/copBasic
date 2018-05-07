\encoding{utf8}
\name{densityCOPplot}
\alias{densityCOPplot}
\title{ Contour Density Plot of a Copula }
\description{
Generate a \emph{contour density plot} after the advocation of Joe (2014, pp. 9--15). Such graphics are plots of \emph{scaled copula densities} (\eqn{c^\star(u,v)}, bivariate herein) that are copula densities scaled to the standard normal distribution \eqn{\sim} N(0,1) margins. Joe (2014) repeatedly emphasizes such plots in contrast to the uniform distribution \eqn{\sim} U(0,1) margins. Nelsen (2006) does not discuss such scaling but seemingly Nelsen's objectives for his book were different.

The density of copula \eqn{\mathbf{C}(u,v)} is numerically estimated by
\deqn{c(u,v) = [\mathbf{C}(u_2,v_2) - \mathbf{C}(u_2,v_1) - \mathbf{C}(u_1,v_2) + \mathbf{C}(u_1,v_1)]/[\Delta(uv)\times\Delta(uv)]\mbox{,}}
where \eqn{c(u,v) \ge 0} (see Nelsen, 2006, p. 10; \code{\link{densityCOP}}). Given a numerically estimated quantity \eqn{c^\star(u,v) = c(u,v)\times\phi(\Phi^{(-1)}(u))\times\phi(\Phi^{(-1)}(v))} for copula density \eqn{c(u,v)}, a grid of the \eqn{c^\star(u,v)} values can be contoured by the \code{contour()} function in \R.  The density function of the N(0,1) is \eqn{\phi(z)} for standard normal variate \eqn{z} and the quantile function of the N(0,1) is \eqn{\Phi^{(-1)}(t)} for nonexceedance probability \eqn{t}.

A grid (matrix) of \eqn{c(u,v)} or \eqn{c^\star(u,v)} is defined for sequence of \eqn{u} and \eqn{v} probabilities for which each sequence has equal steps that are equal to \eqn{\Delta(uv)}. This function has as focus on plotting of the contour lines of \eqn{c^\star(u,v)} but the \R \code{matrix} of either \eqn{c(u,v)} or \eqn{c^\star(u,v)} can be requested on return. For either matrix, the \code{colnames()} and \code{rownames()} (the \R functions) are set equal to the sequence of \eqn{u} and \eqn{v}, respectively. Neither the column or row names are set to the standard normal variates for the matrix of \eqn{c^\star(u,v)}, the names remain in terms of nonexceedance probability.

For plotting and other uses of normal scores of data, Joe (2014, p. 245) advocates that one should use the plotting position formula \eqn{u_i = (i-1/2)/n} (\emph{Hazen plotting position}) for normal scores \eqn{z_i = \Phi^{-1}(u_i)} in preference to \eqn{i/(n+1)} (\emph{Weibull plotting position}) because \eqn{n^{-1}\sum_{i=1}^{n} z^2_i} is closer to unity. Other places of Joe's advocation for the Hazen plotting positions are available (Joe, 2014, pp. 9, 17, 245, 247--248).
}
\usage{
densityCOPplot(cop=NULL, para=NULL, deluv=0.002, origins=TRUE,
               getmatrix=c("none", "cdenzz", "cden"), n=0,
               ploton=TRUE, contour.col=1, contour.lwd=1.5, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{deluv}{The change in the sequences \eqn{\{u, v\} \mapsto \delta, \ldots, 1-\delta; \delta = \Delta(uv)} probabilities;}
  \item{origins}{A logical to plot the origin lines;}
  \item{getmatrix}{A trigger on whether the density matrix is to be returned. The option \code{cdenzz} returns the density scaled by the two standard normal densities (\eqn{c^\star(u,v)}), whereas the option \code{cden} returns simply the copula density (\eqn{c(u,v)});}
  \item{ploton}{A logical to toggle on the plot;}
  \item{contour.col}{The color of the contour lines, which corresponds to the \code{col} argument of the \code{contour} function in \R;}
  \item{contour.lwd}{The width of the contour lines, which corresponds to the \code{lwd} argument of the \code{contour} function in \R;}
  \item{n}{An optional sample size for which simulation of this many values from the copula will be made by \code{\link{simCOP}} and drawn; and}
  \item{...}{Additional arguments to pass to the copula function and to the \code{contour} function in \R (\emph{e.g.} to turn off labeling of contours add \code{drawlabels=FALSE}).}
}
\value{
  This is a high-level function used for its side effects; an \R \code{matrix} can be triggered, however, as a returned value.
}
\note{
Joe (2014, p. 244) says \dQuote{if [density] plots show multimodality, then multivariate techniques of mixture models, cluster analysis[,] and nonparametric functional data analysis might be more appropriate} than relatively straightforward parametric copula models.
}
\references{
Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{simCOP}}, \code{\link{densityCOP}}}
\examples{
\dontrun{
# Joe (2014, p. 5) names rMTCJ = reflected Mardia-Takahasi-Cook-Johnson copula
"rMTCJ" <- function(u,v,para, ...) {
   u + v - 1 + ((1-u)^(-para) + (1-v)^(-para) - 1)^(-1/para)
} # Survival Copula ("reflected" in Joe's terms)
densityCOPplot(cop=rMTCJ, para=1.0760, n=9000)
# The density plot matches that shown by Joe (2014, p. 11, fig. 1.2, lower left plot)
# for a Spearman's Rho equaling 0.5. Let us compute then Rho:
rhoCOP(cop=rMTCJ, para=1.076075) # 0.4999958

# Now let us get really wild with a composition with TWO modes!
# This example also proves that the grid orientation is consistent with respect
# to the orientation of the simulations.
para <- list(alpha=0.15, beta=0.90, kappa=0.06, gamma=0.96,
             cop1=GHcop, cop2=PLACKETTcop, para1=5.5, para2=0.07)
densityCOPplot(cop=composite2COP, para=para, n=9000)

# Now let us hack back to a contour density plot with U(0,1) and not N(0,1) margins
# just so show that capability exists, but emphasis of densityCOPplot is clearly
# on Joe's advocation so it does not have a trigger to use U(0,1) margins.
set.seed(12)
H <- densityCOPplot(cop=PLACKETTcop, para=41.25, getmatrix="cden", n=1000)
set.seed(12)
UV <- simCOP(cop=PLACKETTcop, para=41.25, n=1000, col=8)
U <- as.numeric(colnames(H)); V <- as.numeric(rownames(H))
contour(x=U, y=V, z=t(H), lwd=1.5, cex=2, add=TRUE, col=2) # }

\dontrun{
UV <- rCOP(400,  cop=PSP, pch=16, col=8, n=400)
CL <- mleCOP(UV, cop=CLcop,   interval=c(1,20))
JO <- mleCOP(UV, cop=JOcopB5, interval=c(.1,20))
PL <- mleCOP(UV, cop=PLcop,   interval=c(.1,20))

AICs <- c(CL$AIC, JO$AIC, PL$AIC)
names(AICs) <- c("Clayton", "Joe(B5)", "Plackett")
print(round(AICs, digits=2))
# Clayton    Joe(B5)   Plackett 
#  -167.15   -36.91  -131.35
# So we conclude Clayton must be the best fit of the three.

plot(qnorm(UV[,1]), qnorm(UV[,2]), pch=16, col=8,
     xlim=c(-3,3), ylim=c(-3,3), cex=0.5,
     xlab="Standard normal variate of V",
     ylab="Standard normal variate of U")
densityCOPplot(cop=PSP, contour.col=grey(0.5), lty=2,
               contour.lwd=3.5, ploton=FALSE)
densityCOPplot(cop=CLcop,     para=CL$para,
               contour.col=2, ploton=FALSE)
densityCOPplot(cop=JOcopB5,   para=JO$para,
               contour.col=3, ploton=FALSE)
densityCOPplot(cop=PLcop,     para=PL$para,
               contour.col=4, ploton=FALSE) # }
}
\keyword{visualization}
\keyword{copula (density)}
\keyword{Joe (2014) Examples and Exercises}
