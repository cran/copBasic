\encoding{utf8}
\name{FGMcop}
\alias{FGMcop}
\alias{FGMicop}
\title{The Generalized Farlie--Gumbel--Morgenstern Copula}
\description{
The \emph{generalized Farlie--Gumbel--Morgenstern copula} (Bekrizade \emph{et al.}, 2012) is
\deqn{\mathbf{C}_{\Theta, \alpha, n}(u,v) = \mathbf{FGM}(u,v) = uv[1 + \Theta(1-u^\alpha)(1-v^\alpha)]^n\mbox{,}}
where \eqn{\Theta \in [-\mathrm{min}\{1, 1/(n\alpha^2)\}, +1/(n\alpha)]}, \eqn{\alpha > 0}, and \eqn{n \in 0,1,2,\cdots}. The copula \eqn{\Theta = 0} or \eqn{\alpha = 0} or \eqn{n = 0} becomes the \emph{independence copula} (\eqn{\mathbf{\Pi}(u,v)}; \code{\link{P}}). When \eqn{\alpha = n = 1}, then the well-known, single-parameter Farlie--Gumbel--Morgenstern copula results, and \emph{Spearman Rho} (\code{\link{rhoCOP}}) is \eqn{\rho_\mathbf{C} = \Theta/3} but in general
\deqn{\rho_\mathbf{C} = 12\sum_{r=1}^n {n \choose r} \Theta^r \biggl[\frac{\phantom{\alpha}\Gamma(r+1)\Gamma(2/\alpha)}{\alpha\Gamma(r+1+2/\alpha)} \biggr]^2
\mbox{.}}
The support of \eqn{\rho_\mathbf{C}(\cdots;\Theta, 1, 1)} is \eqn{[-1/3, +1/3]} but extends via \eqn{\alpha} and \eqn{n} to \eqn{\approx [-0.50, +0.43]}, which shows that the generalization of the copula increases the range of dependency. The generalized version is implemented by \code{FGMcop}.

The \emph{iterated Farlie--Gumbel--Morgenstern copula} (Chine and Benatia, 2017) for the \eqn{r}th iteration is
\deqn{\mathbf{C}_{\beta}(u,v) = \mathbf{FGMi}(u,v) = uv + \sum_{j=1}^{r} \beta_j\cdot(uv)^{[j/2]}\cdot(u'v')^{[(j+1)/2]}\mbox{,}}
where \eqn{u' = 1-u} and \eqn{v' = 1-v} for \eqn{|\beta_j| \le 1} that has \eqn{r} dimensions \eqn{\beta = (\beta_1, \cdots, \beta_j, \cdots, \beta_r)} and \eqn{[t]} is the integer part of \eqn{t}. The copula \eqn{\beta = 0} becomes the \emph{independence copula} (\eqn{\mathbf{\Pi}(u,v)}; \code{\link{P}}). The support of \eqn{\rho_\mathbf{C}(\cdots;\beta)} is approximately \eqn{[-0.43, +0.43]}. The iterated version is implemented by \code{FGMicop}. Internally, the \eqn{r} is determined from the length of the \eqn{\beta} in the \code{para} argument.
}
\usage{
FGMcop( u, v, para=c(NA, 1,1), ...)
FGMicop(u, v, para=NULL,       ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector of parameters. For the generalized version, the \eqn{\Theta}, \eqn{\alpha}, and \eqn{n} of the copula where the default argument shows the need to include the \eqn{\Theta}. However, if a fourth parameter is present, it is treated as a logical to reverse the copula (\eqn{u + v - 1 + \mathbf{FGM}(1-u,1-v; \Theta, \alpha, n)}). Also if a single parameter is given, then the \eqn{\alpha = n = 1} are automatically set to produce the single-parameter Farlie--Gumbel--Morgenstern copula. For the iterated version, the \eqn{\beta} vector of \eqn{r} iterations;}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned.
}
\references{
Bekrizade, Hakim, Parham, G.A., Zadkarmi, M.R., 2012, The new generalization of Farlie--Gumbel--Morgenstern copulas: Applied Mathematical Sciences, v. 6, no. 71, pp. 3527--3533.

Chine, Amel, and Benatia, Fatah, 2017, Bivariate copulas parameters estimation using the trimmed L-moments methods: Afrika Statistika, v. 12, no. 1, pp. 1185--1197.
}
\author{W.H. Asquith}
\seealso{\code{\link{P}}, \code{\link{mleCOP}}
}
\examples{
\dontrun{
  # Bekrizade et al. (2012, table 1) report for a=2 and n=3 that range in
  # theta = [-0.1667, 0.1667] and range in rho = [-0.1806641, 0.4036458]. However,
  # we see that they have seemingly made an error in listing the lower bounds of theta:
  rhoCOP(FGMcop, para=c(  1/6, 2, 3))  #  0.4036458
  rhoCOP(FGMcop, para=c( -1/6, 2, 3))  # Following error results
  # In cop(u, v, para = para, ...) : parameter Theta < -0.0833333333333333
  rhoCOP(FGMcop, para=c(-1/12, 2, 3))  # -0.1806641 }

\dontrun{
  # Support of FGMrcop(): first for r=1 iterations and then for large r.
  sapply(c(-1, 1), function(t) rhoCOP(cop=FGMicop, para=rep(t, 1)) )
  # [1] -0.3333333  0.3333333
  sapply(c(-1, 1), function(t) rhoCOP(cop=FGMicop, para=rep(t,50)) )
  # [1] -0.4341385  0.4341385}

\dontrun{
  # Maximum likelihood estimation near theta upper bounds for a=3 and n=2.
  set.seed(832)
  UV <- simCOP(3000, cop=FGMcop, para=c(+0.16, 3, 2))
  # Define a transform function for parameter domain, though mleCOP does
  # provide some robustness anyway---not forcing n into the positive
  # domain via as.integer(exp(p[3])) seems to not always be needed.
  FGMpfunc <- function(p) {
    d <- p[1]; a <- exp(p[2]); n <- as.integer(exp(p[3]))
    lwr <- -min(c(1,1/(n*a^2))); upr <- 1/(n*a)
    d <- ifelse(d <= lwr, lwr, ifelse(d >= upr, upr, d))
    return( c(d, a, n) )
  }
  para <- c(0.16, 3, 2); init <- c(0, 1, 1)
  ML <- mleCOP(UV$U, UV$V, cop=FGMcop, init.para=init, parafn=FGMpfunc)
  print(ML$para) # [1] 0.1596361 3.1321228 2.0000000
  # So, we have recovered reasonable estimates of the three parameters
  # given through MLE estimation.
  densityCOPplot(cop=FGMcop, para=   para, contour.col="red")
  densityCOPplot(cop=FGMcop, para=ML$para, ploton=FALSE) #}
}
\keyword{Farlie--Gumbel--Morgenstern copula}
\keyword{copula (formulas)}
\keyword{copula}
\keyword{Bekrizade et al. (2012) Examples and Exercises}
\concept{Farlie-Gumbel-Morgenstern copula}
\concept{generalized Farlie-Gumbel-Morgenstern copula}
\concept{iterated Farlie-Gumbel-Morgenstern copula}
