\encoding{utf8}
\name{LMRvuCOP}
\alias{LMRvuCOP}
\title{Conditional L-moments of the Distribution of V given U}
\description{
Compute the \emph{conditional L-moments} of \eqn{V} given a \eqn{U} (the \eqn{X} direction) through the \emph{conditional distribution function} \eqn{F(X)} using the appropriate \emph{partial derivative} of a copula (\eqn{\mathbf{C}(u,v)}) with respect to \eqn{U}. The conditional L-moments are computed for \eqn{\lambda_1} (mean) from \code{\link{EvuCOP}} (refer to mathematics therein) and for \eqn{r \ge 2} are computed by

\deqn{\lambda_r = \int_0^1 F(k) \times (1-F(k)) \times L_r(F(k))\,\mathrm{d}k\mbox{,}}

where \eqn{F(k)} becomes replaced in \pkg{copBasic} with the conditional distribution function of a copula by Nelsen (2006, pp. 13, 40--41) with respect to \eqn{u}:

\deqn{0 \le \frac{\delta}{\delta u} \mathbf{C}(u,v) \le 1\mbox{,}}

which is \code{\link{derCOP}} and \eqn{L_r(u)} is defined (Jones, 2004; Asquith, 2011) as

\deqn{L_r(z) = \frac{1}{r-1}\sum_{j=0}^{r-2} (-1)^j {r-1 \choose j}{r-1 \choose j+1}z^{r-2-j}(1-z)^j\mbox{.}}

Asquith (2011) mistakenly lists the leading multiplier \eqn{1/(1-r)} in lieu of \eqn{1/(1-r)} (correct) as shown above; the \pkg{lmomco} package contains a suitable errata as discovered in \pkg{copBasic} development for \code{LMRvuCOP} and \code{\link{LMRuvCOP}}.
}
\usage{
LMRvuCOP(u=seq(0.01, 0.99, by=0.01), cop=NULL, para=NULL, nsim=1E5,
      subdivisions=200L, rel.tol=.Machine$double.eps^0.25, abs.tol=rel.tol, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{cop}{A copula function with vectorization as in \code{asCOP};}
  \item{para}{Vector of parameters or other data structures, if needed, to pass to the copula;}
  \item{nsim}{Number of simulations for \emph{Monte Carlo integration} if numerical integration were to fail;}
  \item{subdivisions}{Argument of same name passed to \code{integrate()};}
  \item{rel.tol}{Argument of same name passed to \code{integrate()};}
  \item{abs.tol}{Argument of same name passed to \code{integrate()}; and}
  \item{...}{Additional arguments to pass to \code{\link{derCOP}}.}
}
\value{
  Value(s) in a list for the median and conditional L-moments given \eqn{U=u} are returned including
  \item{median}{The conditional median from \code{\link{med.regressCOP}};}
  \item{L1}{The conditional mean \eqn{\lambda_1|U=u} from \code{\link{EvuCOP}};}
  \item{L2}{The conditional L-scale \eqn{\lambda_2|U=u};}
  \item{T3}{The conditional L-skew \eqn{\tau_3|U=u};}
  \item{T4}{The conditional L-kurtosis \eqn{\tau_4|U=u};}}
\references{
Asquith, W.H., 2011, Distributional analysis with L-moment statistics using the R environment for statistical computing: Createspace Independent Publishing Platform, ISBN 978--146350841--8.

Jones, M.C., 2004, On some expressions for variance, covariance, skewness and L-moments:
Journal of Statistical Planning and Inference, v.126, pp. 97--106, \doi{10.1016/j.jspi.2003.09.001}.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{W.H. Asquith}
\seealso{\code{\link{EvuCOP}}, \code{\link{LMRuvCOP}}, \code{\link{derCOP}}}
\examples{
\dontrun{
  para <- list(cop=breveCOP, para=list(cop=CLcop, para=c(10), breve=0.2, reflect=2))
  uv <- simCOP(500, cop=COP, para=para, graphics=FALSE, set.seed=1)

  ustar <- 0.96; set.seed(1)
  v_special <- replicate(10000, simCOPv(ustar, cop=COP, para=para))
  lmr <- lmomco::lmoms(v_special)

  u <- v <- seq(0,1, by=0.001)
  LMRvu <- LMRvuCOP(u, cop=COP, para=para)
  # LMRuv <- LMRuvCOP(v, cop=COP, para=para)
  # LMRvu <- LMRuv # swap here to make a quick study of U given V
  # uv <- uv[,2:1] # swap here to make a quick study of U given V
  # pdf("LMRvuCOP.pdf", height=7, width=7, useDingbats=FALSE)
  par(xpd=NA, las=1, lend=1, col.axis="black", cex.axis=0.9,
      mgp=c(2,0.6,0), mai=c(0.65,0.65,0.40,0.65))
  plot( uv[,1], uv[,2], type="n", lwd=0.8, col="grey50", bty="n", xlim=c(0,1),
        ylim=c(0,1), xlab="", ylab="", xaxs="i", yaxs="i", xaxt="n", yaxt="n")
  lines(rep(ustar, 2), par()$usr[3:4], lty=4, lwd=1.2, col="seagreen4")
  par(mgp=c(2,0.3,0), las=0, col.axis="seagreen4", cex.axis=0.8)
  txt <- latex2exp::TeX(r'($u^\star$)')
  axis(1, at=ustar, label=txt,  lwd=0, lwd.ticks=1.2, tcl=-0.25, col.ticks="seagreen4")
  axis(3, at=ustar, label=txt,  lwd=0, lwd.ticks=1.2, tcl=-0.25, col.ticks="seagreen4")
  axis(1, at=ustar, label=FALSE,lwd=0, lwd.ticks=1.2, tcl=+0.25, col.ticks="seagreen4")
  axis(3, at=ustar, label=FALSE,lwd=0, lwd.ticks=1.2, tcl=+0.25, col.ticks="seagreen4")
  par(mgp=c(2,0.6,0), las=1, col.axis="black", cex.axis=0.9)
  points(uv[,1], uv[,2], lwd=0.9, pch=21, cex=0.8, col="wheat4", bg="wheat1")
  axis(1, at=axTicks(1), label=TRUE,  lwd=1, lwd.ticks=1, col.ticks="black")
  axis(3, at=axTicks(1), label=FALSE, lwd=1, lwd.ticks=1, col.ticks="black")
  tix <- seq(0.1, 0.9, by=0.2)
  axis(1, at=tix, label=FALSE, lwd=0, lwd.ticks=1, col.ticks="black", tcl=-0.5)
  axis(2, at=tix, label=FALSE, lwd=0, lwd.ticks=1, col.ticks="black", tcl=-0.5)
  axis(3, at=tix, label=FALSE, lwd=0, lwd.ticks=1, col.ticks="black", tcl=-0.5)
  tix <- seq(0.05, 0.95, by=0.05); tix <- tix[grep("5$", as.character(tix))]
  axis(1, at=tix, label=FALSE, lwd=0, lwd.ticks=1, col.ticks="black", tcl=-0.2)
  axis(2, at=tix, label=FALSE, lwd=0, lwd.ticks=1, col.ticks="black", tcl=-0.2)
  axis(3, at=tix, label=FALSE, lwd=0, lwd.ticks=1, col.ticks="black", tcl=-0.2)
  mtext("U, nonexceedance probability", side=1, line=1.6)
  par(mgp=c(2,0.8,0))
  axis(2, at=axTicks(2), label=TRUE,  lwd=1, lwd.ticks=1, col.ticks="black")
  par(las=0)
  mtext("V, nonexceedance probability", side=2, line=2.1)
  par(las=1)
  lines(u, LMRvu$L1,     lwd=2, col="black"  ) # mean probability
  lines(u, LMRvu$median, lwd=2, col="red1"   ) # median probability
  lines(u, LMRvu$L2,     lwd=2, col="deepskyblue3") # conditional L-scale
  points(ustar,   mean(v_special), cex=1.7, pch=1,  col="black"  )
  points(ustar, median(v_special), cex=1.7, pch=1,  col="red1"   )
  points(ustar, lmr$lambdas[1],    cex=1.3, pch=16, col="black"  )
  points(ustar, lmr$lambdas[2],    cex=1.3, pch=16, col="deepskyblue3")
  par(new=TRUE)
  plot(u, LMRvu$T3, type="n", bty="n",xlim=c(0,1), ylim=c(-1, 1),
           xlab="", ylab="", xaxs="i", yaxs="i", xaxt="n", yaxt="n")
  par(col.axis="orchid3", las=0, lend=1); colo <- "orchid3"
  mtext("Lskew and Lkurtosis, dimensionless", 4, line=2.15, col=colo)
  par(las=1)
  axis(4, at=axTicks(2), label=TRUE, lwd=2, lwd.ticks=1, col=colo, col.ticks=colo)
  tix <- seq(-0.9, 0.9, by=0.1)
  tix <- tix[grep("[05]$", as.character(tix), invert=TRUE)]
  axis(4, at=tix, label=FALSE, lwd=0, lwd.ticks=1, tcl=-0.5, col.ticks=colo)
  lines(u, LMRvu$T3, col=colo, lwd=2) # conditional L-skew
  lines(u, LMRvu$T4, col=colo, lty=2, lwd=2) # conditional L-kurtosis
  points(ustar, lmr$ratios[ 3],    cex=1.2,   pch=24, col=colo, bg="orchid1")
  points(ustar, lmr$ratios[ 4],    cex=1.2,   pch=25, col=colo, bg="orchid1")
  txt <- c(paste0("Conditional (Cond.) mean at ustar = ", ustar, ", EvuCOP(...)"),
           "Cond. median at ustar, med.regressCOP(...)",
           "Cond. Lscale (variation) at ustar, LMRvuCOP(...)",
           "Cond. Lskew at ustar, LMRvuCOP(...)",
           "Cond. Lkurtosis at ustar, LMRvuCOP(...)",
           "Demonstration of n=500 sim. values for the copula",
           "Cond. simulated (sim.) (n=10,000) sample mean, mean(...)",
           "Cond. sim. sample median, median(...)",
           "Cond. sim. sample mean, lmoms(...)$lambdas[1]",
           "Cond. sim. sample Lscale, lmoms(...)$lambdas[2]",
           "Cond. sim. sample Lskew, lmoms(...)$ratios[3]",
           "Cond. sim. sample Lkurtosis, lmoms(...)$ratios[4]")
  legend("topleft", txt, bty="n", cex=0.6, seg.len=4, y.intersp=1.3,
         lwd=c(2,2,2,2,2, NA, rep(NA, 6)), lty=c(1,1,1,1,2,0,rep(0, 6)),
         pch=c(rep(NA, 5), 21, 1, 1, 16, 16, 24, 25),
         pt.lwd=c(rep(NA,5), 0.9, rep(1,6)),
         pt.cex=c(rep(NA,5), 0.8, 1.7, 1.7, 1.3, 1.3, 1.2, 1.2),
         pt.bg=c(rep(NA,5), "wheat1", rep(NA,4), "orchid1", "orchid1"),
         col=c("black", "red1", "deepskyblue3", colo, colo,
         "wheat4", "black", "red1", "black", "deepskyblue3", colo, colo))
  par(xpd=NA, las=1, lend=1, col.axis="black", cex.axis=0.9,
      mgp=c(2,0.6,0), mai=c(0.65,0.65,0.40,0.65))
  # dev.off() # }
}
\keyword{copula (utility)}
\keyword{copula operator}
\keyword{copula (conditional distribution)}
\keyword{conditional expectation}
\keyword{L-moments (conditional) of V given U}
\keyword{Conditional L-moments of V given U}
