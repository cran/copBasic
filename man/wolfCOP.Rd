\name{wolfCOP}
\alias{wolfCOP}
\title{ The Schweizer and Wolff's Sigma of a Copula }
\description{
Compute the measure of association known as \emph{Schweizer and Wolff's Sigma} \eqn{\sigma_\mathbf{C}} of a copula according to Nelsen (2006, p. 209) by
\deqn{\sigma_\mathbf{C} = 12\int\!\!\int_{\mathcal{I}^2} |\mathbf{C}(u,v) - uv|\,\mathrm{d}u\mathrm{d}v\mbox{.}}
It is obvious that this measure of association is similar to the following form of Spearman's Rho
\deqn{\rho_\mathbf{C} = 12\int\!\!\int_{\mathcal{I}^2} [\mathbf{C}(u,v) - uv]\,\mathrm{d}u\mathrm{d}v\mbox{,\ and}}
if a copula is positively quadrant dependent (PQD, see \code{\link{isCOP.PQD}}) then \eqn{\sigma_\mathbf{C} = \rho_\mathbf{C}} and conversely if a copula is negatively quadrant dependent (NQD) then \eqn{\sigma_\mathbf{C} = -\rho_\mathbf{C}}. However, a feature that makes \eqn{\sigma_\mathbf{C}} especially attractive is that random variables \eqn{X} and \eqn{Y} that are not PQD or NQD---that is copulas that are neither larger nor smaller than \eqn{\mathbf{\Pi}}---is that \dQuote{\eqn{\sigma_\mathbf{C}} is often a better measure of [dependency] than \eqn{\rho_\mathbf{C}}} (Nelsen, 2006, p. 209).
}
\usage{
wolfCOP(cop=NULL, para=NULL, brute=FALSE, delta=0.002, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{brute}{Should brute force be used instead of two nested \code{integrate()} functions in \R to perform the double integration;}
  \item{delta}{The \eqn{\mathrm{d}u} and \eqn{\mathrm{d}v} for the brute force (\code{brute=TRUE}) integration; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for \eqn{\sigma_\mathbf{C}} is returned.
}
\references{
Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{hoefCOP}}, \code{\link{rhoCOP}}
}
\examples{
wolfCOP(cop=PSP)

# EXTENDED EXAMPLE --- with ## Not run: to be added by user for more graphics.
para <- list(cop1=PLACKETTcop, cop2=PLACKETTcop, para1=0.145, para2=21.9,
             alpha=0.81,       beta=0.22)
D <- simCOP(n=250, cop=composite2COP, para=para,
            cex=0.5, col=rgb(0,0,0,0.2), pch=16)
# Globally PQD but there is a significant local NQD part of IxI space that is
PQD <- isCOP.PQD(cop=composite2COP, para=para, uv=D)
points(D, col=PQD$local.PQD+2, lwd=2)
# clearly NQD, so this copula interacts through the P copula. Hence by the logic
# of Nelsen (2006), then Schweizer-Wolff Sigma should be larger than Spearman Rho.
wolfCOP(cop=composite2COP, para=para) # 0.08373378
 rhoCOP(cop=composite2COP, para=para) # 0.02845131
hoefCOP(cop=composite2COP, para=para) # 0.08563823
# which is clearly present in these simulated data. In fact the output above also shows
# Sigma to be larger than Kendall Tau, Gini Gamma, and Blomqvist Beta thus Sigma has
# captured the fact that although the symbols plot near randomly on the figure, the
# the symbol coloring for PQD and NQD clearly shows local dependency differences.
\dontrun{
the.grid <- EMPIRgrid(para=D)
the.persp <- persp(the.grid$empcop, theta=-25, phi=20, shade=TRUE,
                   xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")
empcop <- EMPIRcopdf(para=D) # data.frame of all points
points(trans3d(empcop$u, empcop$v, empcop$empcop, the.persp),  cex=0.7,
       col=rgb(0,1-sqrt(empcop$empcop),1,sqrt(empcop$empcop)), pch=16)
points(trans3d(empcop$u, empcop$v, empcop$empcop, the.persp),
       col=PQD$local.PQD+1, pch=1)

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE), respect = TRUE)
PQD.NQD.cop <- gridCOP(cop=composite2COP, para=para)
Pi <- gridCOP(cop=P)
RHO <- PQD.NQD.cop - Pi; SIG <- abs(RHO)
the.persp <- persp(PQD.NQD.cop, theta=-25, phi=20, shade=TRUE, cex=0.5,
               xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")
mtext("The Copula that has local PQD and NQD", cex=0.5)
the.persp <- persp(Pi, theta=-25, phi=20, shade=TRUE, cex=0.5,
               xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")
mtext("Independence (Pi)", cex=0.5)
the.persp <- persp(RHO, theta=-25, phi=20, shade=TRUE, cex=0.5,
               xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")
mtext("Copula delta: Integrand of Spearman's Rho", cex=0.5)
the.persp <- persp(SIG, theta=-25, phi=20, shade=TRUE, cex=0.5,
               xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")
mtext("abs(Copula delta): Integrand of Schweizer-Wolff's Sigma", cex=0.5)
}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}

