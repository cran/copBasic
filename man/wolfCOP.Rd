\encoding{utf8}
\name{wolfCOP}
\alias{wolfCOP}
\title{ The Schweizer and Wolff Sigma of a Copula }
\description{
Compute the measure of association known as \emph{Schweizer--Wolff Sigma} \eqn{\sigma_\mathbf{C}} of a copula according to Nelsen (2006, p. 209) by

\deqn{\sigma_\mathbf{C} = 12\int\!\!\int_{\mathcal{I}^2} |\mathbf{C}(u,v) - uv|\,\mathrm{d}u\mathrm{d}v\mbox{.}}

It is obvious that this measure of association is similar to the following form of \emph{Spearman's Rho} of a copula:

\deqn{\rho_\mathbf{C} = 12\int\!\!\int_{\mathcal{I}^2} [\mathbf{C}(u,v) - uv]\,\mathrm{d}u\mathrm{d}v\mbox{.}}

If a copula is \emph{positively quadrant dependent} (PQD, see \code{\link{isCOP.PQD}}) then \eqn{\sigma_\mathbf{C} = \rho_\mathbf{C}} and conversely if a copula is \emph{negatively quadrant dependent} (NQD) then \eqn{\sigma_\mathbf{C} = -\rho_\mathbf{C}}. However, a feature that makes \eqn{\sigma_\mathbf{C}} especially attractive is that random variables \eqn{X} and \eqn{Y} that are not PQD or NQD---that is copulas that are neither larger nor smaller than \eqn{\mathbf{\Pi}}---is that \dQuote{\eqn{\sigma_\mathbf{C}} is often a better measure of [dependency] than \eqn{\rho_\mathbf{C}}} (Nelsen, 2006, p. 209).
}
\usage{
wolfCOP(cop=NULL, para=NULL, as.sample=FALSE, brute=FALSE, delta=0.002, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{as.sample}{A logical controlling whether an optional \R \code{data.frame} in \code{para} is used to compute the \eqn{\hat{\sigma}_\mathbf{C}} (see \bold{Note}). If set to \code{-1}, then the message concerning CPU effort will be surpressed;}
  \item{brute}{Should brute force be used instead of two nested \code{integrate()} functions in \R to perform the double integration;}
  \item{delta}{The \eqn{\mathrm{d}u} and \eqn{\mathrm{d}v} for the brute force (\code{brute=TRUE}) integration; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value for \eqn{\sigma_\mathbf{C}} is returned.
}
\note{
A natural estimator for \eqn{\sigma_\mathbf{C}} is through the \emph{empirical copula} (\enc{Póczos}{Poczos} \emph{et al.}, 2015) and can be computed as
\deqn{\hat{\sigma}_\mathbf{C} = \frac{12}{n^2 - 1}
        \sum_{i=1}^n\sum_{j=1}^n \bigg|\hat{\mathbf{C}}_n\biggl(\frac{i}{n}, \frac{j}{n}\biggr) -
                 \frac{i}{n}\times\frac{j}{n}\bigg|\mbox{,}}
where \eqn{\hat{\mathbf{C}}_n} is the simplest empirical copula of
\deqn{\hat{\mathbf{C}}_n\biggl(\frac{i}{n}, \frac{j}{n}\biggr) =
	\frac{1}{n}\{\# \mathrm{\ of\ } (U_k \le U_i, V_k \le V_j)\}}

An extended example is informative. First declare a composite of two different Plackett copulas (\code{\link{PLACKETTcop}}) and simulate a few hundred values:
\preformatted{
   para <- list(cop1=PLACKETTcop, cop2=PLACKETTcop, para1=0.145, para2=21.9,
                alpha=0.81,       beta=0.22)
   D <- simCOP(n=300, cop=composite2COP, para=para,
               cex=0.5, col=rgb(0,0,0,0.2), pch=16)
}
Second, show that this copula is globally PQD (\code{\link{isCOP.PQD}}), but there is a significant local NQD part of \eqn{\mathcal{I}^2} space that clearly is NQD.
\preformatted{
  PQD <- isCOP.PQD(cop=composite2COP, para=para, uv=D)
  message(PQD$global.PQD) # TRUE
  points(D, col=PQD$local.PQD+2, lwd=2)
}
This composite copula intersects through the \code{\link{P}} copula. Hence by the logic of Nelsen (2006), then Schweizer--Wolff Sigma should be larger than Spearman Rho (\code{\link{rhoCOP}}) as shown below
\preformatted{
  wolfCOP(cop=composite2COP, para=para) # 0.08373378 (theoretical)
   rhoCOP(cop=composite2COP, para=para) # 0.02845131 (theoretical)
  hoefCOP(cop=composite2COP, para=para) # 0.08563823 (theoretical)
}
In fact, the output above also shows Schweizer--Wolff Sigma to be larger than Kendall Tau (\code{\link{tauCOP}}), Gini Gamma (\code{\link{giniCOP}}), and Blomqvist Beta (\code{\link{blomCOP}}) thus Schweizer--Wolff Sigma has captured the fact that although the symbols plot near randomly on the figure, the symbol coloring for PQD and NQD clearly shows local dependency differences. The sample version is triggered by
\preformatted{
  wolfCOP(para=D, as.sample=TRUE) # 0.09278467 (sample)
}
}
\references{
\enc{Póczos}{Poczos}, \enc{Barnabás}{Barnabas}, Krishner, Sergey, \enc{Pál}{Pal}, \enc{Szepesvári}{Szepesvari}, Csaba, and Schneider, Jeff, 2015, Robust nonparametric copula based dependence estimators: Accessed on August 11, 2015 at \url{https://www.cs.cmu.edu/~bapoczos/articles/poczos11nipscopula.pdf}.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{blomCOP}}, \code{\link{footCOP}}, \code{\link{giniCOP}},
         \code{\link{hoefCOP}}, \code{\link{rhoCOP}}, \code{\link{tauCOP}},
         \code{\link{joeskewCOP}}, \code{\link{uvlmoms}}
}
\examples{
\dontrun{
wolfCOP(cop=PSP) # 0.4784176}

\dontrun{
n <- 1000; UV <- simCOP(n=n, cop=N4212cop, para=7.53, graphics=FALSE)
wolfCOP(cop=N4212cop, para=7.53) # 0.9884666 (theoretical)
wolfCOP(para=UV, as.sample=TRUE) # 0.9884174 (sample)}

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
mtext("Copula delta: Integrand of Spearman Rho", cex=0.5)
the.persp <- persp(SIG, theta=-25, phi=20, shade=TRUE, cex=0.5,
               xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")
mtext("abs(Copula delta): Integrand of Schweizer-Wolff Sigma", cex=0.5)# }
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}

