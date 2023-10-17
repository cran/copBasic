\encoding{utf8}
\name{statTn}
\alias{statTn}
\title{The Tn Statistic of a Fitted Copula to an Empirical Copula}
\description{
Compute the \eqn{T_n(p)} statistic of Genest \emph{et al.} (2011) that is defined as
\deqn{T_n(p) = \sum_{i=1}^n \big|\mathbf{C}_n(u_i, v_i) - \mathbf{C}_{\Theta_n}(u_i, v_i)\big|^p\mbox{,}}
where \eqn{\mathbf{C}_n(u,v)} is the \emph{empirical copula}, \eqn{\mathbf{C}_{\Theta_n}(u,v)} is the \emph{fitted copula} with estimated parameters \eqn{\Theta_n} from the sample of size \eqn{n}. The \eqn{T_n} for \eqn{p = 2} is reported by those authors to be of general purpose and overall performance in large scale simulation studies. The extension here for arbitary exponent \eqn{p} is made for flexibility. Alternatively the definition could be associated with the statistic \eqn{T_n(p)^{1/p}} in terms of a root \eqn{1/p} of the summation as shown above.

The \eqn{T_n} statistic is obviously a form of deviation between the empirical (nonparametric) and parametric fitted copula. The distribution of this statistic through Monte Carlo simulation could be used for inference. The inference is based on that a chosen parametric model is suitably close to the empirical copula. The \eqn{T_n(p)} statistic has an advantage of being relatively straightforward to understand and explain to stakeholders and decision makers, is attractive for being suitable in a wide variety of circumstances, but intuitively might have limited statistical power in some situations for it looks at whole copula structure and not say at tail dependency. Finally, other goodness-of-fits using the squared differences between \eqn{\mathbf{C}_n(u,v)} and \eqn{\mathbf{C}_{\Theta_m}(u, v)} are \code{\link{aicCOP}}, \code{\link{bicCOP}}, and \code{\link{rmseCOP}}.
}
\usage{
statTn(u, v=NULL, cop=NULL, para=NULL, p=2, proot=FALSE, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction. If not given, then a second column from argument \code{u} is attempted;}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{p}{The value for \eqn{p}, and the default follows that of Genest \emph{et al.} (2011);}
  \item{proot}{A logical controling whether the \eqn{T_n} returned be rooted by \eqn{1/p}, and the default follows that of Genest \emph{et al.} (2011); and}
  \item{...}{Additional arguments to pass to the copula function and (or) the empirical copula.}
}
\value{
  The value for \eqn{T_n} is returned dependent on the specification of \eqn{p} and whether rooting of the result is desired.
}
\note{
The \bold{Examples} section shows a simple computation of the \eqn{\hat T_n} statistic for a sample and a fitted copula to that sample. Ideally \code{statTn} would be wrapped in a Monte Carlo process of fitting the apparent \dQuote{parent} distribution from the sample data, then for some large replication count, generate \eqn{N} samples of size \eqn{n} from the parent and from these samples compute the empirical copula and also fit parameter(s) of the chosen copula and repeatedly solve for \eqn{T_n}. Given a total of \eqn{N} values of \eqn{T_n}, then the sample \eqn{T_n} or \eqn{\hat{T}_n} can be compared to the distribution, and if \eqn{\hat{T}_n} is greater than say the 95th percentile, then the assumed form of the copula could be rejected.

The \code{distTn} defined below and is dependent on the \code{\link{copBasic.fitpara.beta}} function can be used to demonstrate concepts. (The process is complex enough that user-level implementation of \code{distTn} in \pkg{copBasic} is not presently (2019) thought appropriate.)
\preformatted{
  "distTn" <- function(n, N=1000, statf=NULL,
                       cop=NULL, para=para, interval=NULL, ...) {
      opts <- options(warn=-1)
      message("Estimating Tn distribution: ", appendLF=FALSE)
      Tn <- vector(mode="numeric", N)
      for(i in 1:N) {
         showi <- as.logical(length(grep("0+$", i, perl=TRUE)))
         if(showi) message(i, "-", appendLF=FALSE)
         ruv <- simCOP(n=n, cop=cop, para=para, graphics=FALSE, ...)
         rpara <- copBasic.fitpara.beta(ruv, statf=statf,
                             interval=interval, cop=cop)
         Tn[i] <- ifelse(is.na(rpara), NA, statTn(ruv, cop=cop, para=rpara))
      }
      numNA <- length(Tn[is.na(Tn)])
      message("done: Number of failed parameter estimates=", numNA)
      options(opts)
      return(Tn[! is.na(Tn)])
  }
}

Let us imagine an \eqn{n=400} sample size of a \emph{Galambos copula} (\eqn{\mathbf{GL}(u,v)}; \code{\link{GLcop}}) and then treat the \emph{Plackett copula} (\eqn{\mathbf{PL}(u,v)}; \code{\link{PLACKETTcop}}) as the proper (chosen) model. The estimated parameter by the sample \emph{Blomqvist Beta} of \eqn{\hat\beta_\mathbf{C} = 0.64} using the \code{\link{blomCOP}} function called from within \code{copBasic.fitpara.beta} is then placed in variable \code{para}. The \eqn{\hat\beta_\mathbf{C}} is not the most efficient estimator but for purposes here, but it is fast.  The parameter for the given seed is estimated as about \eqn{\mathbf{PL}(\hat\Theta{=}20.75)}.
\preformatted{
  n <- 400 # sample size
  correctCopula <- GLcop; set.seed(1596)
  sampleUV <- simCOP(n=n, cop=correctCopula, para=1.9) # a random sample
  para.correctCopula <- copBasic.fitpara.beta(uv=sampleUV, statf=blomCOP,
                                interval=c(1,5),      cop=correctCopula)
  chosenCopula <- PLACKETTcop
  para <- copBasic.fitpara.beta(uv=sampleUV, statf=blomCOP,
                                interval=c(.001,200), cop=chosenCopula )
}

Next, compute the sample \eqn{\hat T_n = 0.063} from \code{sampleUV}. The distribution of the \eqn{T_n} is estimated using the \code{distTn} function, and an estimate of the \eqn{\hat T_n} p-value is in turn estimated. A large simulation run \eqn{N = 1{,}000} for a sample of size of \eqn{n = 400} is selected. The \code{distTn} function internally will simulated for \code{N}-replicates from the assumed parent and estimate the parameter. A computation run yields a p-value of approximately 0.01 (depending upon the seed) and is statistically significant at an alpha of 0.05, and therefore, the \eqn{\mathbf{PL}(\Theta{=}20.75)} should be rejected for fitting to these data.
\preformatted{
  sampleTn   <- statTn(sampleUV, cop=chosenCopula, para=para)
  Tns        <- distTn(n=n,      cop=chosenCopula, para=para,
                       interval=c(0.001, 100), statf=blomCOP)
  Tns_pvalue <- 1 - sum(Tns <= sampleTn) / length(Tns) # estimate p-value
}

The demonstration is furthered with a check on the \emph{Kullback--Leibler sample size} \eqn{n_{f\!g}} at the 5-percent significance level (alpha = 0.05) by the \code{\link{kullCOP}} function, which yields \eqn{100}. Given the parent copula as \eqn{\mathbf{GL}(\Theta{=}1.9)}, therefore, it would take approximately 100 samples to distinguish between that copula and a \eqn{\mathbf{PL}(\Theta{=}20.75)} where in this case the fit was through the \eqn{\hat\beta_\mathbf{C} = 0.64}.
\preformatted{
  kullCOP(cop1=correctCopula, para1=1.9,
          cop2=chosenCopula,  para2=para)$KL.sample.size # KL sample size = 100
  vuongCOP(sampleUV, cop1=correctCopula, para1=para.correctCopula,
                     cop2=chosenCopula,  para2=para)$message
  # [1] "Copula 1 has better fit than Copula 2 at 100x(1-alpha) level"
}
The available sample size \eqn{n = 400} is then about four times larger than \eqn{n_{f\!g}} so the sample size \eqn{n} should be sufficient to judge goodness-of-fit. This is a large value but with the sample variability of \eqn{\hat\beta_\mathbf{C}}, it seems that other measures of association such as \emph{Spearman Rho} (\code{\link{rhoCOP}}) or \emph{Kendall Tau} (\code{\link{tauCOP}}) and others cross-referenced therein might be preferable.

The prior conclusion is supported by the p-value of the \eqn{\hat T_n} being about 0.01, which suggests that the \eqn{\mathbf{PL}(u,v)} is not a good model of the available sample data in \code{sampleUV}. Lastly, these judgments are consistent with the \emph{Vuoug Procedure} performed by the \code{\link{vuongCOP}} function, which reports at the 5-percent significance level that \dQuote{copula number 1}---in this case, the \eqn{\mathbf{GL}(u,v)}---has the better fit, and this is obviously consistent with the problem setup because the random sample for investigation was drawn from the Galambos coupla (the parent form).
}
\references{
Genest, C., Kojadinovic, I., \enc{Nešlehová}{Neslehova}, J., and Yan, J., 2011, A goodness-of-fit test for bivariate extreme-value copulas: Bernoulli, v. 17, no. 1, pp. 253--275.
}
\author{W.H. Asquith}
\seealso{\code{\link{aicCOP}}, \code{\link{bicCOP}}, \code{\link{rmseCOP}}, \code{\link{vuongCOP}}, \code{\link{kullCOP}}
}
\examples{
\dontrun{
# Example here is just for Tn. For the example below, the PSP copula is quite different
# from the Gumbel-Hougaard copula and thus, the hatTn would be expected to be different
# from those of the Gumbel-Hougaard and certainly not too near to zero.
samUV  <- simCOP(n=60, cop=PSP, graphics=FALSE, seed=1)   # random sample
hatTau <- cor(samUV$U, samUV$V, method="kendall")         # Kendall Tau
hatTn  <- statTn(samUV, cop=GHcop, para=GHcop(tau=hatTau)$para,
                 ctype="bernstein", bernprogress=TRUE)    # 0.03328789
# hatTn in this case is by itself is somewhat uninformative and requires
# Monte Carlo to put an individual value into context.}
}
\keyword{copula (goodness-of-fit)}
\keyword{copula (inference)}
\keyword{inference}
\keyword{goodness-of-fit}
