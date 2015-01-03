\encoding{utf8}
\name{GHcop}
\alias{GHcop}
\title{ The Gumbel-Hougaard Copula }
\description{
The \emph{Gumbel-Hougaard copula} (Nelsen, 2006, p. 118 and 164) is
\deqn{\mathbf{C}_{\Theta}(u,v) = \mathbf{GH}(u,v) = \mathrm{exp}\{-[(-\log u)^\Theta+(-\log v)^\Theta]^{1/\Theta}\}\mbox{,}}
where \eqn{\Theta \in [1 , \infty)}. The parameter \eqn{\Theta} is readily estimated using \emph{Kendall's Tau} (\eqn{\tau}) by
\deqn{\tau = \frac{\Theta - 1}{\Theta} \rightarrow \Theta = \frac{1}{1-\tau}\mbox{.}}
The copula is readily extended into \eqn{d} dimensions by
\deqn{\mathbf{C}_{\Theta}(u,v) = \mathrm{exp}(-[(-\log u_1)^\Theta+\cdots+(-\log u_d)^\Theta]^{1/\Theta})\mbox{.}}
However, such an implementation in not available in the \pkg{copBasic} package.

Every Gumbel-Hougaard copula is a \emph{Multivariate Extreme Value} (MEV) copula, and hence useful in analysis of extreme value distributions. The Gumbel-Hougaard copula is the \emph{only} Archimedean MEV (Salvadori \emph{et al.}, 2007, p. 192). The Gumbel-Hougaard copula has respective \emph{lower-} and \emph{upper-tail dependency parameters} of \eqn{\lambda_L = 0} and \eqn{\lambda_U = 2 - 2^{1/\Theta}}. Nelsen (2006, p. 96) shows that \eqn{\mathbf{C}^r_\theta(u^{1/r}, v^{1/r}) = \mathbf{C}_\theta(u,v)} so that every Gumbel-Hougaard copula is \emph{max-stable}.
}
\usage{
GHcop(u, v, para=NULL, tau=NULL, tau.big=0.985, cor=NULL, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (single element) of parameters---the \eqn{\Theta} parameter of the copula;}
  \item{tau}{Kendall's Tau from which to estimate the parameter \eqn{\Theta};}
  \item{tau.big}{The largest value for \eqn{\tau} prior to switching to the \eqn{\mathbf{M}} copula;}
  \item{cor}{A \pkg{copBasic} syntax for \dQuote{the correlation coefficient} suitable for the copula---a synonym for \code{tau}; and }
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned using the \eqn{\Theta} as set by argument \code{para}. If \code{para=NULL} and \code{tau} is set, then \eqn{\tau \rightarrow \Theta} and an \R \code{list} is returned. If \code{para=NULL} and \code{tau=NULL}, then an attempt to estimate \eqn{\Theta} from the \code{u} and \code{v} is made by \eqn{\mathrm{cor}(u,v)_\tau \rightarrow \tau \rightarrow \Theta} using \code{cor(u,v, method="kendall")} in \R and an \R \code{list} is returned. The possibly returned \code{list} has the following elements:
  \item{para}{The estimated \eqn{\Theta} from Kendall's Tau; and}
  \item{tau}{The estimate of Kendall's Tau \eqn{\tau}.}
}
\details{
Numerical experiments seem to indicate that for \eqn{\tau > 0.985} that failures in the numerical partial derivatives in \code{\link{derCOP}} and \code{\link{derCOP2}} result---a \eqn{\tau^\star} this large is indeed large. As \eqn{\Theta \rightarrow \infty} the Gumbel-Hougaard copula becomes the \emph{\enc{Fréchet}{Frechet}-Hoeffding upper bound copula} \eqn{\mathbf{M}} (see \code{\link{M}}). A \eqn{\tau \approx 0.985} yields \eqn{\Theta \approx 66 + 2/3}, then for \eqn{\Theta > 1/(1-\tau^\star)} flips over to the \eqn{\mathbf{M}} copula with a warning being issued.
}
\references{
Joe, H., 2015, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{M}}
}
\examples{
a <- 2.2
del.lamU <- abs(taildepCOP(cop=GHcop, para=a)$lambdaU - (2-2^(1/a)))
as.logical(del.lamU < 1E-6) # TRUE
\dontrun{
# The simulations match Joe (2015, p. 72) for Gumbel-Hougaard
n <- 600; nsim <- 1000
SM <- sapply(1:nsim, function(i) {
                     cors <- semicorCOP(cop=GHcop, para=1.350, n=n)
                     return(c(cors$minus.semicor, cors$plus.semicor)) })
RhoM <- mean(SM[1,]); RhoP <- mean(SM[2,])
SE.RhoM <- sd(SM[1,]); SE.RhoP <- sd(SM[2,]); SE.RhoMP <- sd(SM[2,]-SM[1,])
# Minus Semi-Correlation = 0.133 (SE=0.073);  Positive= 0.413 (SE=0.075)
# SE MinusRho = 0.077, PlusRho = 0.073, SE MinusRho - PlusRho = 0.106
# Joe (2015) reports 0.132, 0.415, 0.08, 0.07, 0.10, respectively.
}
\dontrun{
file <- "Lcomom_study_of_GHcopPLACKETTcop.txt"
x <- data.frame(tau=NA, trho=NA, srho=NA, GHtheta=NA, PLtheta=NA,
                GHT2=NA, GHT3=NA, GHT4=NA,
                PLT2=NA, PLT3=NA, PLT4=NA)
write.table(x, file=file, row.names=FALSE, quote=FALSE)
n <- 25000 # Make a large number, long CPU run but seems stable
for(tau in seq(0,0.98, by=0.005)) {
   thetag <- GHcop(u=NULL, v=NULL, tau=tau)$para
   trho <- rhoCOP(cop=GHcop, para=thetag)
   GH <- simCOP(n=n, cop=GHcop, para=thetag, points=FALSE, ploton=FALSE)
   srho <- cor(GH$U, GH$V, method="spearman")
   thetap <- PLACKETTpar(rho=trho)
   PL <- simCOP(n=n, cop=PLACKETTcop, para=thetap, points=FALSE, ploton=FALSE)
   GHl <- lmomco::lcomoms2(GH, nmom=4); PLl <- lmomco::lcomoms2(PL, nmom=4)
   x <- data.frame(tau=tau, trho=trho, srho=srho,
                   GHtheta=thetag, PLtheta=thetap,
                   GHT2=mean(c(GHl$T2[1,2], GHl$T2[2,1])),
                   GHT3=mean(c(GHl$T3[1,2], GHl$T3[2,1])),
                   GHT4=mean(c(GHl$T4[1,2], GHl$T4[2,1])),
                   PLT2=mean(c(PLl$T2[1,2], PLl$T2[2,1])),
                   PLT3=mean(c(PLl$T3[1,2], PLl$T3[2,1])),
                   PLT4=mean(c(PLl$T4[1,2], PLl$T4[2,1])) )
   write.table(x, file=file, row.names=FALSE, col.names=FALSE, append=TRUE)
}
D <- read.table(file, header=TRUE); D <- D[complete.cases(D),]
plot(D$tau, D$GHT3, ylim=c(-0.08,0.08), type="n",
     xlab="KENDALL'S TAU", ylab="L-COSKEW OR NEGATED L-COKURTOSIS")
points(D$tau,  D$GHT3, col=2)
points(D$tau,  D$PLT3, col=1)
points(D$tau, -D$GHT4, col=4, pch=2)
points(D$tau, -D$PLT4, col=1, pch=2)
LM3 <- lm(D$GHT3~I(D$tau^1)+I(D$tau^2)+I(D$tau^4)-1)
LM4 <- lm(D$GHT4~I(D$tau^1)+I(D$tau^2)+I(D$tau^4)-1)
LM3c <- LM3$coe; LM4c <- LM4$coe
Tau <- seq(0,1, by=.01)
abline(0,0, lty=2, col=3)
lines(Tau,   0 + LM3c[1]*Tau^1 + LM3c[2]*Tau^2 + LM3c[3]*Tau^4,  col=4, lwd=3)
lines(Tau, -(0 + LM4c[1]*Tau^1 + LM4c[2]*Tau^2 + LM4c[3]*Tau^4), col=2, lwd=3)
}
}
\keyword{Gumbel-Hougaard copula}
\keyword{copula (formulas)}
\keyword{copula}
\keyword{Joe (2015) Examples and Exercises}

