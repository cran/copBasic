\encoding{utf8}
\name{footCOP}
\alias{footCOP}
\title{The Spearman Footrule of a Copula}
\description{
Compute the measure of association known as the \emph{Spearman Footrule} \eqn{\psi_\mathbf{C}} (Nelsen \emph{et al.}, 2001, p. 281), which is defined as

\deqn{\psi_\mathbf{C} = \frac{3}{2}\mathcal{Q}(\mathbf{C},\mathbf{M}) - \frac{1}{2}\mbox{,}}

where \eqn{\mathbf{C}(u,v)} is the copula, \eqn{\mathbf{M}(u,v)} is the \emph{\enc{Fréchet}{Frechet}--Hoeffding upper bound} (\code{\link{M}}), and \eqn{\mathcal{Q}(a,b)} is a \emph{concordance function} (\code{\link{concordCOP}}) (Nelsen, 2006, p. 158). The \eqn{\psi_\mathbf{C}} in terms of a single integration pass on the copula is
\deqn{\psi_\mathbf{C} = 1 - \int_{\mathcal{I}^2} |u-v|\,\mathrm{d}\mathbf{C}(u,v) = 6 \int_0^1 \mathbf{C}(u,u)\,\mathrm{d}u - 2\mbox{.}}
% http://www.encyclopediaofmath.org/index.php?title=Spearman_rho_metric&oldid=15466  (accessed Aug 9, 2015)
Note, Nelsen \emph{et al.} (2001) use \eqn{\phi_\mathbf{C}} but that symbol is taken in \pkg{copBasic} for the \emph{Hoeffding Phi} (\code{\link{hoefCOP}}), and Spearman Footrule does not seem to appear in Nelsen (2006). From the definition, Spearman Footrule only depends on the \emph{primary diagnonal} (alt. \emph{main diagonal}, Genest \emph{et al.}, 2010) of the copula, \eqn{\mathbf{C}(t,t)} (\code{\link{diagCOP}}).
}
\usage{
footCOP(cop=NULL, para=NULL, by.concordance=FALSE, as.sample=FALSE, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{by.concordance}{Instead of using the single integral to compute \eqn{\psi_\mathbf{C}}, use the concordance function method implemented through \code{\link{concordCOP}}; and}
  \item{as.sample}{A logical controlling whether an optional \R \code{data.frame} in \code{para} is used to compute the \eqn{\hat\psi} (see \bold{Note}); and}
  \item{...}{Additional arguments to pass, which are dispatched to the copula function \code{cop} and possibly \code{\link{concordCOP}}, such as \code{brute} or \code{delta} used by that function.}
}
\value{
  The value for \eqn{\psi_\mathbf{C}} is returned.
}
\note{
Conceptually, the sample Spearman Footrule is a standardized sum of the absolute difference in the ranks (Genest \emph{et al.}, 2010). The sample \eqn{\hat\psi} is

\deqn{\hat\psi = 1 - \frac{3}{n^2 - 1}\sum_{i=1}^n |R_i - S_i|\mbox{,}}

where \eqn{R_i} and \eqn{S_i} are the respective ranks of \eqn{X} and \eqn{Y} and \eqn{n} is sample size. The sampling variance of \eqn{\hat\psi} under \emph{assumption of independence} between \eqn{X} and \eqn{Y} is

\deqn{\mathrm{var}(\hat\psi) = \frac{2n^2 + 7}{5(n+1)(n-1)^2}\mbox{.}}

Genest \emph{et al.} (2010, p. 938) say that prior literature shows that in small samples, Spearman Footrule is less variable than the well-known \emph{Spearman Rho} (\code{\link{rhoCOP}}). For a copula having continuous partial derivatives, then as \eqn{n \rightarrow \infty}, the quantity \eqn{(\hat\psi - \psi_\mathbf{C})\sqrt{n} \sim \mathrm{Normal}(0, \mathrm{var}(\gamma_\mathbf{C}))}. Genest \emph{et al.} (2010) show variance of \eqn{\hat\psi} for the \emph{independence copula} (\eqn{\mathbf{C}(u,v) = \mathbf{\Pi}(u,v)}) (\code{\link{P}}) as \eqn{\mathrm{var}(\psi_\mathbf{C}) = 2/5}. For comparison, the \emph{Gini Gamma} for independence is larger at \eqn{\mathrm{var}(\gamma_\mathbf{C}) = 2/3} (see \code{\link{giniCOP}} \bold{Note}). Genest \emph{et al.} (2010) also present additional material for estimation of the distribution \eqn{\hat\psi} variance for conditions of dependence based on copulas. In Genest \emph{et al.} independence and two examples of dependence (p. 941), \eqn{\mathrm{var}(\hat\gamma) > \mathrm{var}(\hat\psi)}, but those authors do not appear to remark on whether this inequality holds for all copula.
}
\references{
Genest, C., \enc{Nešlehová}{Neslehova}, J., and Ghorbal, N.B., 2010, Spearman's footrule and Gini's gamma---A review with complements: Journal of Nonparametric Statistics, v. 22, no. 8, pp. 937--954, \doi{10.1080/10485250903499667}.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Nelsen, R.B., Quesada-Molina, J.J., \enc{Rodríguez-Lallena}{Rodriguez-Lallena}, J.A., \enc{Úbeda-Flores}{Ubeda-Flores}, M., 2001, Distribution functions of copulas---A class of bivariate probability integral transforms: Statistics and Probability Letters, v. 54, no. 3, pp. 277--282, \doi{10.1016/S0167-7152(01)00060-8}.
}
\author{W.H. Asquith}
\seealso{\code{\link{blomCOP}}, \code{\link{giniCOP}}, \code{\link{hoefCOP}},
         \code{\link{rhoCOP}}, \code{\link{tauCOP}},  \code{\link{wolfCOP}}
}
\examples{
  footCOP(cop=PSP)                      # 0.3177662
# footCOP(cop=PSP, by.concordance=TRUE) # 0.3178025

\dontrun{
n <- 2000; UV <- simCOP(n=n, cop=GHcop, para=2.3, graphics=FALSE)
footCOP(para=UV, as.sample=TRUE)                  # 0.5594364 (sample version)
footCOP(cop=GHcop, para=2.3)                      # 0.5513380 (copula integration)
footCOP(cop=GHcop, para=2.3, by.concordance=TRUE) # 0.5513562 (concordance function)
# where the later issued warnings on the integration}

\dontrun{
set.seed(1); nsim <- 1000
varFTunderIndpendence <- function(n) {
  (2*n^2 + 7) / (5*(n+1)*(n-1)^2) # Genest et al. (2010)
}
ns <- c(10, 15, 20, 25, 50, 75, 100)
plot(min(ns):max(ns), varFTunderIndpendence(10:max(ns)), type="l",
     xlab="Sample size", ylab="Variance of Sample Estimator", col="salmon4")
mtext("Sample Spearman Footrule Under Independence", col="salmon4")
for(n in ns) {
  sFT <- vector(length=nsim)
  for(i in seq_len(nsim)) {
    uv <- simCOP(n=n, cop=P, para=2, graphics=FALSE)
    sFT[i] <- footCOP(para=uv, as.sample=TRUE)
  }
  varFT <- varFTunderIndpendence(n)
  zz <- round(c(n, mean(sFT), var(sFT), varFT), digits=6)
  names(zz) <- c("n", "mean_sim", "var_sim", "var_Genest")
  print(zz)
  points(n, zz[3], cex=2, pch=21, col="salmon4", bg="salmon1")
} # results show proper implementation and Genest et al. (2010, sec. 3) }
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
