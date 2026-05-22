\encoding{utf8}
\name{giniCOP}
\alias{giniCOP}
\title{The Gini Gamma of a Copula}
\description{
Compute the measure of association known as the \emph{Gini Gamma} \eqn{\gamma_\mathbf{C}} (Nelsen, 2006, pp. 180--182), which is defined as
\deqn{\gamma_\mathbf{C} = \mathcal{Q}(\mathbf{C},\mathbf{M}) + \mathcal{Q}(\mathbf{C},\mathbf{W})\mbox{,}}
where \eqn{\mathbf{C}(u,v)} is the copula, \eqn{\mathbf{M}(u,v)} is the \code{\link{M}} function, and \eqn{\mathbf{W}(u,v)} is the \code{\link{W}} function. The function \eqn{\mathcal{Q}(a,b)} (\link{concordCOP}) is a \emph{concordance function} (Nelsen, 2006, p. 158). Nelsen also reports that \dQuote{Gini Gamma measures a concordance relation of \dQuote{distance} between \eqn{\mathbf{C}(u,v)} and monotone dependence, as represented by the \emph{\enc{Fréchet}{Frechet}--Hoeffding lower bound} and \emph{\enc{Fréchet}{Frechet}--Hoeffding upper bound} copulas [\eqn{\mathbf{M}(u,v)}, \code{\link{M}} and \eqn{\mathbf{W}(u,v)}, \code{\link{W}} respectively]}

A simpler method of computation and the default for \code{giniCOP} is to compute \eqn{\gamma_\mathbf{C}} by

\deqn{\gamma_\mathbf{C}  = 4\biggl[\int_\mathcal{I} \mathbf{C}(u,u)\,\mathrm{d}u +
                             \int_\mathcal{I} \mathbf{C}(u,1-u)\,\mathrm{d}u\biggr] -
                           2\mbox{,}}
or in terms of the \emph{primary diagonal} (alt. \emph{main diagonal}, Genest \emph{et al.}, 2010) \eqn{\delta(t)} and \emph{secondary diagonal} \eqn{\delta^\star(t)} (see \code{\link{diagCOP}}) by
\deqn{\gamma_\mathbf{C}  = 4\biggl[\int_\mathcal{I} \mathbf{\delta}(t)\,\mathrm{d}t +
                             \int_\mathcal{I} \mathbf{\delta^\star
                             }(t)\,\mathrm{d}t\biggr] -
                           2\mbox{.}}


The simpler method is more readily implemented because single integration is fast. Lastly, Nelsen \emph{et al.} (2001, p. 281) show that \eqn{\gamma_\mathbf{C}} also is computable by
\deqn{\gamma_\mathbf{C} = 2\,\mathcal{Q}(\mathbf{C},\mathbf{A})\mbox{,}}
where \eqn{\mathbf{A}} is a \emph{convex combination} (\code{\link{convex2COP}}, using \eqn{\alpha = 1/2}) of the copulas \eqn{\mathbf{M}} and \eqn{\mathbf{W}} or \eqn{\mathbf{A} = (\mathbf{M}+\mathbf{W})/2}. However, integral convergence errors seem to trigger occasionally, and the first definition by summation \eqn{\mathcal{Q}(\mathbf{C},\mathbf{M}) + \mathcal{Q}(\mathbf{C},\mathbf{W})} thus is used. The convex combination is demonstrated in the \bold{Examples} section.
}
\usage{
giniCOP(cop=NULL, para=NULL, by.concordance=FALSE, as.sample=FALSE, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{by.concordance}{Instead of using the single integrals (Nelsen, 2006, pp. 181--182) to compute \eqn{\gamma_\mathbf{C}}, use the concordance function method implemented through \code{\link{concordCOP}};}
  \item{as.sample}{A logical controlling whether an optional \R \code{data.frame} in \code{para} is used to compute the \eqn{\hat\gamma_\mathbf{C}} (see \bold{Note}); and}
  \item{...}{Additional arguments to pass, which are dispatched to the copula function \code{cop} and possibly \code{\link{concordCOP}} if \code{by.concordance=TRUE}, such as \code{delta} used by that function.}
}
\value{
  The value for \eqn{\gamma_\mathbf{C}} is returned.
}
\note{
Conceptually, the sample Gini Gamma (\eqn{\hat\gamma}; Genest \emph{et al.}, 2010) is

\deqn{\hat\gamma = \frac{1}{\lfloor n^2/2 \rfloor}\sum_{i=1}^n \mid (n+1-R_i) - S_i \mid- \mid R_i - S_i\mid\mbox{,}}

where \eqn{{\lfloor m \rfloor}} denotes the integer part of arbitrary \eqn{m > 0}, \eqn{R_i} and \eqn{S_i} are the respective ranks of \eqn{X} and \eqn{Y} and \eqn{n} is sample size. The sampling variance of \eqn{\hat\gamma} under \emph{assumption of independence} between \eqn{X} and \eqn{Y} is

\deqn{\mathrm{var}(\hat\gamma)_{n\ \mathrm{even}} = \frac{2}{3}\frac{(n^2 + 2)}{(n-1)n^2}\mbox{\ and}}

\deqn{\mathrm{var}(\hat\gamma)_{n\ \mathrm{odd}} = \frac{2}{3}\frac{(n^2 + 3)}{(n-1)(n^2-1)}\mbox{.}}

Genest \emph{et al.} (2010) present additional equations for estimation of the distribution \eqn{\hat\gamma} variance for conditions of dependence based on copulas. For a copula having continuous partial derivatives, then as \eqn{n \rightarrow \infty}, the quantity \eqn{(\hat\gamma - \gamma_\mathbf{C})\sqrt{n} \sim \mathrm{Normal}(0, \mathrm{var}(\gamma_\mathbf{C}))}. Genest \emph{et al.} (2010) show variance of \eqn{\hat\gamma} for the \emph{independence copula} (\eqn{\mathbf{C}(u,v) = \mathbf{\Pi}(u,v)}) (\code{\link{P}}) as \eqn{\mathrm{var}(\gamma_\mathbf{C}) = 2/3}. For comparison, the \emph{Spearman Footrule} for independence is smaller at \eqn{\mathrm{var}(\psi_\mathbf{C}) = 2/5} (see \code{\link{footCOP}} \bold{Note}). In Genest \emph{et al.} independence and two examples of dependence (p. 941), \eqn{\mathrm{var}(\hat\gamma) > \mathrm{var}(\hat\psi)}, but those authors do not appear to remark on whether this inequality holds for all copula.
}
\references{
Genest, C., \enc{Nešlehová}{Neslehova}, J., and Ghorbal, N.B., 2010, Spearman's footrule and Gini's gamma---A review with complements: Journal of Nonparametric Statistics, v. 22, no. 8, pp. 937--954, \doi{10.1080/10485250903499667}.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Nelsen, R.B., Quesada-Molina, J.J., \enc{Rodríguez-Lallena}{Rodriguez-Lallena}, J.A., \enc{Úbeda-Flores}{Ubeda-Flores}, M., 2001, Distribution functions of copulas---A class of bivariate probability integral transforms: Statistics and Probability Letters, v. 54, no. 3, pp. 277--282, \doi{10.1016/S0167-7152(01)00060-8}.
}
\author{W.H. Asquith}
\seealso{\code{\link{blomCOP}}, \code{\link{footCOP}}, \code{\link{hoefCOP}},
         \code{\link{rhoCOP}}, \code{\link{tauCOP}}, \code{\link{wolfCOP}},
         \code{\link{joeskewCOP}}, \code{\link{uvlmoms}}
}
\examples{
giniCOP(cop=PSP)                                 #                 = 0.3819757
\dontrun{
giniCOP( cop=PSP, by.concordance=TRUE)           # Q(C,M) + Q(C,W) = 0.3820045
# use convex combination ---triggers integration warning but returns anyway
cxpara <- list(alpha=1/2, cop1=M, cop2=W) # parameters for convex2COP()
2*tauCOP(cop=PSP, cop2=convex2COP, para2=cxpara) #    2*Q(C,A)     = 0.3819807
# where the later issued warnings on the integration}

\dontrun{
n <- 2000; UV <- simCOP(n=n, cop=N4212cop, para=9.3, graphics=FALSE)
giniCOP(para=UV, as.sample=TRUE)                     # 0.9475900 (sample version)
giniCOP(cop=N4212cop, para=9.3)                      # 0.9479528 (copula integration)
giniCOP(cop=N4212cop, para=9.3, by.concordance=TRUE) # 0.9480267 (concordance function)
# where the later issued warnings on the integration}

\dontrun{
# The canoncial example of theoretical and sample estimators of bivariate
# association for the package: Blomqvist Beta, Spearman Footrule, Gini Gamma,
# Hoeffding Phi, Kendall Tau, Spearman Rho, and Schweizer-Wolff Sigma
# and comparison to L-correlation via lmomco::lcomoms2().
n <- 9000; set.seed(56)
para <- list(cop1=PLACKETTcop, cop2=PLACKETTcop, para1=1.45, para2=21.9,
             alpha=0.41, beta=0.08)
D <- simCOP(n=n, cop=composite2COP, para=para, cex=0.5, col=rgb(0,0,0,0.2), pch=16)
blomCOP(cop=composite2COP, para=para)         # 0.4037908 (theoretical)
blomCOP(para=D, as.sample=TRUE)               # 0.4008889 (sample)
footCOP(cop=composite2COP, para=para)         # 0.3721555 (theoretical)
footCOP(para=D, as.sample=TRUE)               # 0.3703623 (sample)
giniCOP(cop=composite2COP, para=para)         # 0.4334687 (theoretical)
giniCOP(para=D, as.sample=TRUE)               # 0.4311698 (sample)
tauCOP(cop=composite2COP,  para=para)         # 0.3806909 (theoretical)
tauCOP(para=D,  as.sample=TRUE)               # 0.3788139 (sample)
rhoCOP(cop=composite2COP,  para=para)         # 0.5257662 (theoretical)
rhoCOP(para=D,  as.sample=TRUE)               # 0.5242380 (sample)
lmomco::lcomoms2(D)$T2      # 1               # 0.5242388 (sample matrix)
                            # 0.5245154 1
hoefCOP(cop=composite2COP, para=para)         # 0.5082776 (theoretical)
subsample <- D[sample(1:n, n/5),] # subsampling for speed
hoefCOP(para=subsample, as.sample=TRUE)       # 0.5033842 (re-sample)
#hoefCOP(para=D, as.sample=TRUE) # major CPU hog, n too big
# because the Ds are already "probabilities" just resample as shown above
wolfCOP(cop=composite2COP, para=para)         # 0.5257662 (theoretical)
#wolfCOP(para=D, as.sample=TRUE) # major CPU hog, n too big
wolfCOP(para=subsample, as.sample=TRUE)       # 0.5338009 (re-sample)}


\dontrun{
set.seed(1); nsim <- 1000
varGIunderIndpendence <- function(n) {
  if(3 \%/\% 2 == 3 / 2) { # even
    (2/3)*( (n^2 + 2) ) / ( (n-1)* n^2    ) # Genest et al. (2010)
  } else {
    (2/3)*( (n^2 + 3) ) / ( (n-1)*(n^2-1) ) # Genest et al. (2010)
  }
}
ns <- c(10, 15, 20, 25, 50, 75, 100)
plot(min(ns):max(ns), varGIunderIndpendence(10:max(ns)), type="l",
     xlab="Sample size", ylab="Variance of Sample Estimator", col="darkgreen")
mtext("Sample Gini Gamma Under Independence", col="darkgreen")
for(n in ns) {
  sGI <- vector(length=nsim)
  for(i in seq_len(nsim)) {
    uv <- simCOP(n=n, cop=P, para=2, graphics=FALSE)
    sGI[i] <- giniCOP(para=uv, as.sample=TRUE)
  }
  varGI <- varGIunderIndpendence(n)
  zz <- round(c(n, mean(sGI), var(sGI), varGI), digits=6)
  names(zz) <- c("n", "mean_sim", "var_sim", "var_Genest")
  print(zz)
  points(n, zz[3], cex=2, pch=21, col="darkgreen", bg="lightgreen")
} # results show proper implementation and Genest et al. (2010, sec. 3) }
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
