\encoding{utf8}
\name{wolfCOP}
\alias{wolfCOP}
\title{Schweizer and Wolff Sigma of a Copula}
\description{
Compute the measure of association known as \emph{Schweizer--Wolff Sigma} \eqn{\sigma_\mathbf{C}} of a copula according to Nelsen (2006, p. 209) by

\deqn{\sigma_\mathbf{C} = 12\int\!\!\int_{\mathcal{I}^2} \bigl|\mathbf{C}(u,v) - uv\bigr|\,\mathrm{d}u\mathrm{d}v\mbox{,}}

which is \eqn{0 \le \sigma_\mathbf{C} \le 1}. It is obvious that this measure of association, without the positive sign restriction, is similar to the following form of \emph{Spearman Rho} (\code{\link{rhoCOP}}) of a copula:

\deqn{\rho_\mathbf{C} = 12\int\!\!\int_{\mathcal{I}^2} \bigl[\mathbf{C}(u,v) - uv\bigr]\,\mathrm{d}u\mathrm{d}v\mbox{.}}

If a copula is \emph{positively quadrant dependent} (PQD, see \code{\link{isCOP.PQD}}), then \eqn{\sigma_\mathbf{C} = \rho_\mathbf{C}}; conversely if a copula is \emph{negatively quadrant dependent} (NQD), then \eqn{\sigma_\mathbf{C} = -\rho_\mathbf{C}}. However, a feature making \eqn{\sigma_\mathbf{C}} especially attractive is that for random variables \eqn{X} and \eqn{Y}, which are not PQD or NQD---copulas that are neither larger nor smaller than \eqn{\mathbf{\Pi}}---is that \dQuote{\eqn{\sigma_\mathbf{C}} is often a better measure of [dependency] than \eqn{\rho_\mathbf{C}}} (Nelsen, 2006, p. 209).

\enc{Úbeda-Flores}{Ubeda-Flores} (2022) study the maximum difference between \eqn{\sigma_\mathbf{C} - |\rho_\mathbf{C}| \approx 0.60496} for a special class of singular copula (see \bold{Examples}), but states that it \dQuote{remains an open problem to check if, for any copula \eqn{\mathbf{C}_\Theta},} that the maximum listed here is the maximum difference possible.

Li \emph{et al.} (2019) compare \eqn{\sigma_\mathbf{C}} (they label it \eqn{\delta^\mathrm{E}_i}, \dQuote{the extended delta index}) to alternative measures of dependency with emphasis on information content. Those authors also show that dependence can also be measured by

\deqn{\delta_i = \frac{1}{2}\int\!\!\int_{\mathcal{I}^2} \bigl|c(u,v) - 1\bigr|\,\mathrm{d}u\mathrm{d}v\mbox{,}}

where \eqn{c(u,v)} is a \emph{copula density} (\code{\link{densityCOP}}). Li \emph{et al.} (2019, p. 45) \dQuote{measure how much the model output \eqn{Y} and the input \eqn{X_i} are dependent,} and \dQuote{importance of the inputs globally and
quantitatively with model free and moment independent properties.}

By definition for two random variables \eqn{U} and \eqn{V} with cumulative distribution functions of \eqn{F(u)} and \eqn{F(v)}, the variables are \emph{independent}, if and only if, for all subsets (conditions) selecting values \eqn{u} and \eqn{y} of \eqn{U \le u} and \eqn{V \le v}, when for all \eqn{u} and \eqn{v}, the combined cumulative distribution function is

\deqn{H(x, y) = F(x)G(x) = \mathbf{C}(u,v) = uv\mbox{.}}

Note, structural similarity between \eqn{\sigma_\mathbf{C}} and the definition for \code{\link{LzCOPpermsym}}. It seems to follow, by intuition, if therein, the copula on the left were to be replaced with the \emph{permutation symmetric} (\emph{exchangeable symmetric}) copula \code{\link{isCOP.permsym}}, such as \eqn{\mathbf{\Pi} = uv = vu}, then the copula on the right, now being, without loss of generality, \eqn{\mathbf{C}(u,v)} or \eqn{\mathbf{C}(v,u)}, then \eqn{\sigma_\mathbf{C}} must respond partly to dependence existing in the presence of \emph{permutation asymmetry}.
}
\usage{
wolfCOP(cop=NULL, para=NULL, as.sample=FALSE, brute=FALSE, delta=0.002,
        nlarge=500, usefastgrid=TRUE, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{as.sample}{A logical controlling whether an optional \R \code{data.frame} in \code{para} is used to compute the \eqn{\hat{\sigma}_\mathbf{C}} (see \bold{Details}). If set to \code{-1}, then the message concerning CPU effort will be suppressed. This feature of controlling the message is not applicable if the fast computation by \code{usefastgrid} is set and sample size is larger than \code{nlarge} as the double summation in the \enc{Pózos}{Poczos} \emph{et al.} (2015) is bypassed in favor of \code{\link{EMPIRgrid_fast}};}
  \item{brute}{Should brute force be used instead of two nested \code{integrate()} functions in \R to perform the double integration;}
  \item{delta}{The \eqn{\mathrm{d}u} and \eqn{\mathrm{d}v} for the brute force (\code{brute=TRUE}) integration;}
  \item{nlarge}{A large sample size for which to consider the fast-grid computation if \code{usefastgrid} is true;}
  \item{usefastgrid}{A logical controlling whether the fast-grid computation is acceptable to the user; and}
  \item{...}{Additional arguments to pass to the copula itself, to \code{\link{EMPIRgrid_fast}}, or the \code{integrate()} function.}
}
\value{
The value for \eqn{\sigma_\mathbf{C}} is returned, and the sample \eqn{\hat\sigma_\mathbf{C}} computation versions have \code{names()} attached to verify to the user which subsystem was used given the \code{nlarge} and \code{usefastgrid} arguments.
}
\details{
A natural estimator for \eqn{\sigma_\mathbf{C}} is through the \emph{empirical copula} (\enc{Póczos}{Poczos} \emph{et al.}, 2015) and can be computed as
\deqn{\hat{\sigma}_\mathbf{C} = \frac{12}{n^2 - 1}
        \sum_{i=1}^n\sum_{j=1}^n \bigg|\hat{\mathbf{C}}_n\biggl(\frac{i}{n}, \frac{j}{n}\biggr) -
                 \frac{i}{n}\times\frac{j}{n}\bigg|\mbox{,}}
where \eqn{\hat{\mathbf{C}}_n} is the simplest empirical copula of
\deqn{\hat{\mathbf{C}}_n\biggl(\frac{i}{n}, \frac{j}{n}\biggr) =
	\frac{1}{n}\{\# \mathrm{\ of\ } (U_k \le U_i, V_k \le V_j)\}}

The sample version of \eqn{\hat\sigma_\mathbf{C}} is triggered by \code{wolfCOP(para=D, as.sample=TRUE)} for the double summation if \code{usefastgrid} is false and or \code{usefastgrid} is true and the same size is smaller than \code{nlarge}. A Monte Carlo integration version potentially suitable to very large sample sizes or difficult to integrate copula families is located at \code{\link{wolfCOPsamc}}, but the function itself is likely of limited general use because of the implementation of \code{\link{EMPIRgrid_fast}} inside \code{wolfCOP}. It is upon the fast estimator listed above that the extensive simulations leading to \code{\link{wolfCOPtest}} are based. In general, there are several asymptotically equivalent definitions of the empirical copula and some of which are implemented in \code{\link{EMPIRcop}}.

Consideration of speed performance between the \code{as.sample=TRUE} for the sample estimator argued by \enc{Póczos}{Poczos} \emph{et al.}, 2015) (refer to section \bold{Note}), the \code{\link{EMPIRgrid_fast}} is measurably faster by about \eqn{n \ge 500}. The following code shows numerical equivalence between these estimators, but use of other \emph{empirical copula} will have divergence unless samples are large:
\preformatted{
  "MYwolfCOPgrid" <- function(uv, ctype="1/n", ...) {
    ef <- EMPIRgrid_fast(para=uv, gridonly=FALSE, ctype=ctype)
    pf <- ef$empcop; n <- nrow(uv)
    gu <- as.numeric( rownames(ef$empcop) ) # gv <- gu (can compute w/o gv)
    for(i in seq_len(nrow(pf))) pf[i,] <- gu * gu[i]
    return(12 * sum(abs(ef$empcop - pf)) / (n^2 - 1))
  }

  ns <- as.integer( 10^seq(1, 3.3, by=0.1) ) # some sampel sizes
  WG <- WS <- vector(mode="numeric", length(ns))
  for(i in seq_len(length(ns))) {
    uv    <- simCOP(n=ns[i], cop=P, graphics=FALSE)
    WG[i] <- wolfCOP( para=uv, as.sample=TRUE, usefastgrid=FALSE)
    WS[i] <- MYwolfCOPgrid(uv)
  }

  opts <- par(xpd=NA, no.readonly=FALSE)
  df <- data.frame(n=ns, wolf_grid=WG, wolf_sample=WS)
  plot(df$n, df$wolf_sample, type="n", xlim=c(10, 10000), ylim=c(0, 0.60),
       xaxs="i", yaxs="i", log="x", las=1,
       xlab="Sample size of Independence copula",
       ylab="Schweizer-Wolff Sigma")
  points(df$n, df$wolf_sample, pch=21, bg="white")
  points(df$n, df$wolf_grid, pch=16, cex=0.6)
  par(opts) # }
}
\note{
An extended example is informative. First, declare a composite of two different Plackett copulas (\code{\link{PLcop}}) and simulate a few hundred values. The parameter settings are such that the data will attain a look of independence.:
\preformatted{
   para <- list(cop1=PLcop, para1=0.145, alpha=0.81,
                cop2=PLcop, para2=21.90,  beta=0.22)
   D <- simCOP(n=300, cop=composite2COP, para=para,
               cex=0.5, col=grey(0, 0.5), pch=16, seed=15)
}

Second, show that this copula is globally PQD (\code{\link{isCOP.PQD}}), but there is a substantial local NQD part of \eqn{\mathcal{I}^2} space that clearly is NQD.
\preformatted{
  cols <- c("red3", "green4")
  PQD <- isCOP.PQD(cop=composite2COP, para=para, uv=D)
  message(PQD$global.PQD) # TRUE
  points(D, col=cols[1+as.numeric(PQD$local.PQD)], lwd=2) # TRUE are green
}

This composited copula intersects, that is, passes through, the \code{\link{P}} copula. By the logic of Nelsen (2006), then the \eqn{\sigma_\mathbf{C}} should be larger than \eqn{\rho_\mathbf{C}} as shown below
\preformatted{
  wolfCOP(cop=composite2COP, para=para) # 0.08373378 (theoretical)
   rhoCOP(cop=composite2COP, para=para) # 0.02845131 (theoretical)
  hoefCOP(cop=composite2COP, para=para) # 0.08563823 (theoretical)
}

In fact, output above also shows Schweizer--Wolff Sigma to be larger than \emph{Blomqvist Beta} (\code{\link{blomCOP}}), \emph{Gini Gamma} (\code{\link{giniCOP}}), and \emph{Kendall Tau} (\code{\link{tauCOP}}). Schweizer--Wolff Sigma captures the fact that although the symbols seems to plot randomly on the figure, symbol coloring for PQD and NQD clearly shows local dependency differences.
}
\references{
Li, L. Liu, Y., and Lu, Z., 2019, A new dependence measure for importance analysis---Application to an environmental model: Applied Mathematical Modelling, v. 74, pp. 43--61, \doi{10.1016/j.apm.2019.04.046}.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

\enc{Póczos}{Poczos}, B., Krishner, S., \enc{Pál}{Pal}, D., \enc{Szepesvári}{Szepesvari}, C., and Schneider, J., 2015, Robust nonparametric copula based dependence estimators, accessed on August 11, 2015, at \url{https://www.cs.cmu.edu/~bapoczos/articles/poczos11nipscopula.pdf}.

Schweizer, B., and Wolff, E.F., 1981, On nonparametric measures of dependence for random variables: The Annuals of Statistics, v. 9, no. 4, pp. 879--855, \url{https://www.jstor.org/stable/2240856}.

\enc{Úbeda-Flores}{Ubeda-Flores}, M., 2022, A note on the maximum difference between Schweizer and Wolff's Sigma and the absolute value of Spearman's Rho: Transactions on Fuzzy Sets and Systems, v. 1, no. 2, pp. 1--5, \doi{10.30495/tfss.2022.1956899.1024}.
}
\author{W.H. Asquith}
\seealso{\code{\link{wolfCOPsamc}}, \code{\link{wolfCOPtest}}\cr
         \code{\link{blomCOP}}, \code{\link{footCOP}}, \code{\link{giniCOP}},
         \code{\link{hoefCOP}}, \code{\link{rhoCOP}}, \code{\link{tauCOP}},
         \code{\link{joeskewCOP}}, \code{\link{uvlmoms}}, \code{\link{ORDSUMcop}}
}
\examples{
wolfCOP(cop=PSP) # 0.4784176

\dontrun{
  # Maximum Difference between Sigma and Absolute Spearman Rho
  k <- 1/2 + sqrt(3)/6 # Ubeda-Flores (2022) or 1/2 - sqrt(3)/6
  para <- list(cop=c(W, W), para=NULL, part=c(0, k, 1))
  rho <- rhoCOP( ORDSUMcop, para=para)
  wol <- wolfCOP(ORDSUMcop, para=para)
  message("Wolf - |Rho| = ", wol - abs(rho))
  # 0.604967382.... that is approx. 0.60496 given by Ubeda-Flores (2022).

  ks <- seq(0.01, 0.99, by=0.002); rhos <- wols <- NULL
  for(k in ks) {
    message(k, "-", appendLF=FALSE)
    para <- list(cop=c(W, W), para=NULL, part=c(0, k, 1))
    rhos <- c(rhos, abs( rhoCOP( cop=ORDSUMcop, para=para, subdivisions=200) ))
    wols <- c(wols,      wolfCOP(cop=ORDSUMcop, para=para, subdivisions=200)  )
  }
  message("done")
  plot( ks, rhos, type="l", lty=2, main="Ubeda-Flores (2022) Example 2.3",
       xlab="Theta of W,W shuffle", ylab="Value")
  lines(ks, wols, lty=1); lines(ks, wols - rhos, lwd=3)
  legend(0.3, 0.9, c("|Spearman Rho|", "Schweizer and Wolff Sigma",
                     "Schweizer and Wolff Sigma minus |Spearman Rho|"),
         bty="n", lty=c(2,1,1), lwd=c(1,1,2), cex=0.8)
  abline(v=1/2 + c(-1, +1) * sqrt(3)/6, lty=4) # Ubeda-Flores (2022) }

\dontrun{
  n <- 1000; UV <- simCOP(n=n, cop=N4212cop, para=7.53, graphics=FALSE, seed=1)
  wolfCOP(cop=N4212cop, para=7.53)  # 0.9884666 (theoretical)
  wolfCOP(para=UV, as.sample=TRUE)  # 0.9873274 (  sample   )
  wolfCOPsamc(para=UV)$estimates[2] # 0.9861096 (Monte Carlo integration)
  # Much faster as the sample size gets large to use the Monte Carlo. }

\dontrun{
  # Redo D from Note section above
  para <- list(cop1 =PLcop,  cop2=PLcop,
               para1=0.145, para2=21.9,  alpha=0.81, beta=0.22)
  D <- simCOP(n=300, cop=composite2COP, para=para,
              cex=0.5, col=rgb(0, 0, 0, 0.2), pch=16)
  PQD <- isCOP.PQD(cop=composite2COP, para=para, uv=D)
  the.grid  <- EMPIRgrid(para=D)
  the.persp <- persp(the.grid$empcop, theta=-25, phi=20, shade=TRUE,
                     xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")
  empcop <- EMPIRcopdf(para=D) # data.frame of all points
  points(trans3d(empcop$u, empcop$v, empcop$empcop, the.persp),  cex=0.7,
         col=rgb(0, 1-sqrt(empcop$empcop), 1, sqrt(empcop$empcop)), pch=16)
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
  mtext("abs(Copula delta): Integrand of Schweizer-Wolff Sigma", cex=0.5) #}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{Schweizer and Wolff Sigma}
\keyword{extended delta index}
