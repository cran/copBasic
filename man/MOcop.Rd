\encoding{utf8}
\name{MOcop}
\alias{MOcop}
\title{The Marshall--Olkin Copula}
\description{
The \emph{Marshall--Olkin copula} (Dobrowolski and Kumar, 2014, eq. 2.6; Nelsen, 2006, p. 53), which is also known as the \emph{Generalized \enc{Cuadras–Augé}{Cuadras--Auge} copula}, having parameters \eqn{\alpha, \beta \in (0,1)} is given by

\deqn{\mathbf{C}_{(\alpha,\beta)}(u,v) = \mathrm{min}(u^{1-\alpha}v, uv^{1-\beta})\mbox{,\ }}

and efficiently computed by compound equations for \eqn{u^\alpha \ge v^\beta}

\deqn{\mathbf{C}_{(\alpha,\beta)}(u,v) = \mathbf{MO}(u,v) = u^{1-\alpha}v\mbox{,}}

and for \eqn{u^\alpha < v^\beta} is

\deqn{\mathbf{C}_{(\alpha,\beta)}(u,v) = \mathbf{MO}(u,v) = uv^{1-\beta}\mbox{.}}

As \eqn{\alpha, \beta \rightarrow 1}, the copula limits to the \emph{comonotonicity coupla} (\eqn{\mathbf{M}(u,v)}; \code{\link{M}}), and as \eqn{\alpha, \beta \rightarrow 0}, the copula limits to the \emph{independence copula} (\eqn{\mathbf{\Pi}(u,v)}; \code{\link{P}}). The copula can be expected to have a visible singularity as the parameters increase and (or) a simulation size becomes large. The copula with \eqn{\alpha = \beta} is \emph{permutation symmetric} (\code{\link{isCOP.permsym}}, \code{\link{breveCOP}}) and is known as the \emph{\enc{Cuadras–Augé}{Cuadras--Auge}} copula. \emph{Spearman Rho} (\code{\link{rhoCOP}}) of the copula is

\deqn{\rho_\mathbf{C} = 3\alpha\beta / (2\alpha + 2\beta - \alpha\beta)\mbox{,\ }}

and \emph{Kendall tau} (\code{\link{tauCOP}}) is

\deqn{\tau_\mathbf{C} = \alpha\beta / (\alpha + \beta - \alpha\beta)\mbox{.}}

Parameter estimation using \eqn{\rho_\mathbf{C}} and \eqn{\tau_\mathbf{C}} is problematic because the two statistics are so \dQuote{correlated} with each other and that either nonunique solutions or nonconverged solutions manifest with simultaneous numerical optimization. Also, application of \emph{maximum likelihood} (\code{\link{mleCOP}}) can be problematic because of varying contributions of singularities that the copula can produce. The total contribution of singularity is given by \eqn{S(u\rightarrow1,v\rightarrow1) = \alpha\beta/ (\alpha + \beta - \alpha\beta)}, which Nelsen (2006, p. 165) remarks that it is \dQuote{interesting to note that} Kendall tau is the singular component. Lastly, the \emph{L-comoments} of copulas (\code{\link{lcomCOP}}) appear an attractive mechanism for parameter estimation not effected by singularities (refer to \bold{Examples}). The \eqn{\mathbf{MO}} copula has a role in the three-parameter \emph{Gumbel--Hougaard copula} copula (\code{\link{GHcop}}).
}
\usage{
MOcop(u, v, para=NULL, lcomCOP=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (single element) of parameters---the \eqn{\alpha, \beta} parameters of the copula;}
  \item{lcomCOP}{A vector containing \eqn{\rho_\mathbf{C}} and the two L-coskews (refer to \bold{Examples}) from which the parameters are returned; and}
  \item{...}{Additional arguments to pass.}
}
\value{
Value(s) for the copula are returned. Otherwise if either \code{lcomCOP} is given, then the \eqn{\alpha, \beta} are computed by numerical optimization the the Spearman Rho and the two L-coskews (refer to package \pkg{lmomco} and function \code{lmomco::lcomoms2} for more details) (refer the demonstration of L-comoment parameter estimation in the \bold{Examples} below).
}
\references{
Dobrowolski, E., and Kumar, P., 2014, Some properties of the Marshall--Olkin and generalized Cuadras--\enc{Augé}{Auge} familes of copula: Australian Journal of Mathematical Analysis and Applications: v. 11, no. 1, art. 2, pp. 1--13, accessed on August 10, 2025, at \url{https://ajmaa.org/searchroot/files/pdf/v11n1/v11i1p2.pdf}.

Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{P}}, \code{\link{taildepCOP}}
}
\examples{
\dontrun{
  #   **** Demonstration of copula fitting by Method of L-comoments ****
  n    <- 300 # large enough sample size to clearly show L-comoment viability
  UV   <- simCOP(n, cop=MOcop, seed=1, para=c(0.3, 0.7), pch=16, col="seagreen")
  lcm  <- lmomco::lcomoms2(UV) # sample L-comoments (mean off diag. T2 == Spearman Rho)
  sara <- MOcop(lcomCOP=c(mean(diag(lcm$T2[2:1,1:2])), lcm$T3[1,2], lcm$T3[2,1]))
  sara$para <- round(sara$para, digits=8)
  uv   <- simCOP(n, cop=MOcop, seed=1, para=sara$para, col="blue", ploton=FALSE)
  message("MOcop : alpha=", sara$para[1], ", beta=", sara$para[2])
  mara <- mleCOP(UV[,1], v=UV[,2], cop=MOcop, init.para=c(0.2,0.2),
                 parafn=function(x) pnorm(x))
  mara$para <- round(mara$para, digits=8)
  uv   <- simCOP(n, cop=MOcop, seed=1, para=mara$para, col="red", ploton=FALSE)
  message("MOcop : alpha=", mara$para[1], ", beta=", mara$para[2]) #}

\dontrun{
  # This example developed as a demonstrator of the dissarray that the
  # Spearman Footrule measures as a nonunique relation to Spearman Rho.
  # The permutation asymmetry causes the relation between the two to
  # "paint" for positive association as shown in the graphic produced.
  cex <- c(1.3, 1.0, 0.7, 0.4)
  col <- c("#7D0112", "#67C7B1", "#1F28A2", "#CCC86C")
  pch <- c(1, 16, 1, 16)
  plot(c(-0.5, 1), c(0, 1), type="n", xlab="Spearman Footrule", ylab="")
  mtext(paste0("Absolute value of Spearman rho\n",
               "Actual sign is the same as for the Footrule"), side=2, line=2)
  mtext(paste0("Attainable Marshall-Olkin copula, MOcop() with permutation\n",
               "inclusion (breveCOP) and reflections (COP)"), line=1)
  for(r in 1:4) { # reflections, COP
    for(k in seq(-1, 1, by=0.1)) { # permutation asymmetry insertion, breveCOP
      for(a in seq(0, 1, by=0.11)) {   # increment slightly different from beta
        for(b in seq(0, 1, by=0.09)) { # increment slightly different from alpha
          para <- list(cop=MOcop, para=c(a,b), breve=k, reflect=r)
          fotT <- footCOP(cop=breveCOP, para=para)
          rhoT <-  rhoCOP(cop=breveCOP, para=para)
          points(fotT, abs(rhoT), lwd=0.8, pch=pch[r], cex=cex[r], col=col[r])
          #UV <- simCOP(100, cop=breveCOP, para=para)
        }
      }
    }
  } #}
}
\keyword{Marshall--Olkin copula}
\keyword{Cuadras--Auge copula}
\keyword{copula (formulas)}
\keyword{copula}
\keyword{L-comoments}
\keyword{Lcomoments}
\concept{Marshall--Olkin copula}
\concept{Marshall-Olkin copula}
\concept{Cuadras-Auge copula}
