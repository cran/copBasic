\encoding{utf8}
\name{wolfCOPtest_data_smlsam}
\docType{data}
\alias{wolfCOPtest_data_smlsam}
\title{Small Sample Components for Schweizer--Wolff Sigma Test}
\description{
Data for the \emph{ test} components for small samples. Empirical sampling distributions of \emph{Schweizer--Wolff Sigma} (\eqn{\hat\sigma_\mathbf{C}}, \code{\link{wolfCOP}}) for \eqn{n \in 3, \cdots, 40} of the \eqn{\mathrm{NULL} : \mathbf{C} = \mathbf{\Pi}} (\emph{Independence copula}, \code{\link{P}}), where  \eqn{\sigma_\mathbf{C}} is defined under \code{\link{wolfCOP}}, using simulation sizes of 900,000 for each \eqn{n}. So, total simulation size was 34,200,000. Continuing, this data set is a concatenated table of empirical distributions formed from the unique number of \eqn{\hat\sigma_\mathbf{C}} (discrete variables in small samples), which large-simulation attainable up to \eqn{\pm4.26} standard deviations, were computed and tabulated in the \code{wolfCOPtest_data_smlsam} table. Algorithms for the small-sample components are located at \code{inst/}\code{make_wolfCOPtest/}\code{smlsam/}\code{make_}\code{wolfCOPtestP_}\code{smlsam.R}, and this script is the origin of the \eqn{n = 40} in \code{\link{wolfCOPtest}} source code.

For the smallest of sample sizes, the \emph{empirical distribution} is a step function, and for each plateau, a nudge using \code{.Machine$double.eps} has been used to make \code{approx()} for sensible in \code{\link{wolfCOPtest}}. In the \bold{Examples} herein, the jaggedness of the empirical distribution can readily be seen and visually smoothing by about \eqn{n > 10}. Finally, the \code{\link{wolfCOPtest}} function internally loads the \code{wolfCOPtest_data_smlsam} tables, and most users will not need to access this dataset themselves unless for curiosity.
}
\usage{
data(wolfCOPtest_data_smlsam)
}
\format{
An \R \code{data.frame} titled \code{wolfCOPtest_data_smlsam} with
\describe{
  \item{n}{Sample size;}
  \item{probs}{Nonexceedance probability of empirical distribution of \eqn{\hat\sigma_\mathbf{C}} (\code{\link{wolfCOP}}). Note, \code{quantile(..., type=4)} was used for computations because of structural similarity to sample \eqn{\hat\sigma_\mathbf{C}} as implemented in \pkg{copBasic};}
  \item{wolfemp}{Value for \eqn{\hat\sigma_\mathbf{C}} (\code{\link{wolfCOP}});}
}
}
\seealso{\code{\link{wolfCOPtest}}}
\examples{
data(wolfCOPtest_data_smlsam)

\dontrun{
  # Demonstrate the empirical distributions of Schweizer-Wolff against a parametric
  # distribution model. We see quite quickly that the distribution model mimics
  # the empirical distribution at quite small sample sizes.
  data("wolfCOPtest_data_smlsam")
  ff <- pnorm(seq(-3, 3, by=0.1))
  plot(c(-3, 3), c(0,1), type="n", xlab="Standard normal variate",
       ylab="Schweizer-Wolff Sigma given C(u,v)=PI")
  ns <- c(3, 5, 10, 20, 40)
  for(n in ns) {
    sata <- wolfCOPtest_data_smlsam[wolfCOPtest_data_smlsam$n == n,]
    lines(qnorm(sata$probs), sata$wolfemp, col="red", lwd=2)
  }
  ns <- c(3, 4, 5, 7, 10, 15, 20, 30, 40, 50, 70, 100, 130, 200, 500, 1000, 5000)
  for(n in ns) {
    wt   <- wolfCOPtest(0, n) # next isolate the distribution type
    # If copy and paste from R help, then one needs to add backslashes to avoid
    # unrecognized escape errors from R in the next line of code.
    type <- gsub("\\)$", "", gsub("p.value\\(dist_", "", names(wt$p.value)[1]))
    para <- lmomco::vec2par(wt$distpara_by_lmoms, type=type) # type == pe3
    qq   <- 1/(1 + exp(-lmomco::qlmomco(ff, para)))
    lines(qnorm(ff), qq, col="blue")
    text(0, 1/(1 + exp(-lmomco::qlmomco(0.5, para))), n, cex=0.8, font=2)
  }
  par(lheight=0.8)
  txt <- c(paste0(lmomco::prettydist(type), " model by estimated L-moments\n",
                  "of Sigma distribution for select sample sizes"),
           paste0("Empirical distribution for select sample sizes\n",
                  "in wolfCOPtest_data_smlsam data object"))
  legend("topleft", txt, box.lty=0, inset=0.01, cex=0.8, adj=c(0, 0.8),
         y.intersp=1.6, lwd=c(1,2), col=c("blue", "red"), bg=NA)
  par(lheight=1) #}

\dontrun{
  W <- seq(0, 1, by=0.01) # Span all Schweizer-Wolff Sigma, when the first and second
  # arguments to wolfCOPtest are scalars, these are treated as sigma and sample size.
  for(i in 3:40) { # samples sizes 3 to 40 in wolfCOPtest_data_smlsam.
    Pd <- sapply(W, function(w) wolfCOPtest(w, i, usepade=FALSE)$p.value[1])
    Pe <- sapply(W, function(w) wolfCOPtest(w, i, usepade=FALSE)$p.value[2])
    Pp <- sapply(W, function(w) wolfCOPtest(w, i, usepade=TRUE )$p.value[1])
    # p-value by the generalized normal (parametric) distribution
    plot( W, Pd, type="l", col="grey70", lty=2, lwd=7, main=paste0("n=", i),
         xlab="Schweizer-Wolff Sigma", ylab="Exceedance probability (p-value)")
    points(W, Pp, col="skyblue2", lwd=1) # large sample p-values
    # p-value derived from wolfCOP_data_smlsam tables:
    lines(W, Pe, col="green4",    lwd=4) # small sample p-values
  } #}
}
\keyword{datasets}
\keyword{Schweizer and Wolff Sigma}
\keyword{Pearson type III distribution}
\keyword{PE3 (see Pearson type III distribution)}
\keyword{logit transform}
