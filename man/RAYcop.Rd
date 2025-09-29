\encoding{utf8}
\name{RAYcop}
\alias{RAYcop}
\title{The Rayleigh Copula}
\description{
The \emph{Rayleigh copula} (Bo\enc{š}{s}koskia and others, 2018) is
\deqn{\mathbf{C}_{\Theta}(u,v) = \mathbf{RAY}(u,v; \Theta) = 1 + A - B\mbox{,}}
\deqn{A = e^{\Theta a_2 - a_2}\biggl(e^{-a_1}\int_0^{\Theta a_2} e^{-s}I_0\bigl(2\sqrt{a_1 s}\bigr)\,\mathrm{d}s - 1\biggr)\,\mathrm{d}s\mbox{,}}
\deqn{B = e^{-a_1}\int_0^{a_2}e^{-s}I_0\bigl(2\sqrt{\Theta a_1 s}\bigr)\,\mathrm{d}s\mbox{,}}
where \eqn{a_1 = -\log(1-u)/(1-\Theta)}, \eqn{a_2 = -\log(1-v)/(1-\Theta)}, \eqn{I_\nu(x)} is the modified Bessel function of the first kind of order \eqn{\nu} (see \code{base::besselI()}), and \eqn{\Theta \in (0,1]}. The copula, as \eqn{\Theta \rightarrow 0^{+}}, limits to the \emph{independence coupla} (\eqn{\mathbf{\Pi}(u,v)}; \code{\link{P}}) and as \eqn{\Theta \rightarrow 1^{-}} limits to the \emph{comonotonicity copula} (\eqn{\mathbf{M}(u,v)}; \code{\link{M}}). Finally, there are formulations of the Rayleigh copula using the \emph{Marcum-Q function}, but the \pkg{copBasic} developer has not been able to make such work. If the Marcum-Q function could be used, then only one integration and not the two involving the modified Bessel function are possible. Infinite integrations begin occurring in the upper right corner for about \eqn{\Theta > 0.995} at which point the \eqn{\mathbf{M}(u,v)} copula is called in the source code.
}
\usage{
RAYcop(u, v, para=NULL, rho=NULL, method=c("default"),
             rel.tol=.Machine$double.eps^0.5, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (single element) of parameters---the \eqn{\Theta} parameter of the copula;}
  \item{rho}{Value for Spearman Rho from which parameter \eqn{\Theta} is computed by polynomial approximation and returned. The estimation appears sufficient for most pratical applications (see \bold{Examples});}
  \item{method}{The computational method of integrals associated with the definition of the copula; this is designed for the ability to switch eventually in sources to Marcum-Q function implementation. The definition in January 2023 and default is to call the two Bessel function integrals shown for the definition in this documentation;}
  \item{rel.tol}{Argument of the same name for \code{integrate()} call; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned.
}
\note{
Documentation in Zeng and others [Part II] appear to have corrected the Marcum-Q function solution to the copula. The essence of that solution is with a Chi-distribution computation the Marcum-Q. Testing indictates that they have the correct solution, but the derivative for the conditional simulation as built into the design of \pkg{copBasic} has difficulties. Perhaps this is related to numerical precision of the Marcum-Q?
\preformatted{
  sapply(seq_len(length((u))), function(i) {
    a1 <- -log(1-u[i])
    if(is.infinite(a1)) return(v[i])
    a2 <- -log(1-v[i])
    if(is.infinite(a2)) return(u[i])
    a1 <- exp(log(a1) - log(1-p))
    a2 <- exp(log(a2) - log(1-p))
    a3 <- marcumq.chi(sqrt(2*  a1), sqrt(2*p*a2)) # Zeng and others (Part II)
    a4 <- marcumq.chi(sqrt(2*p*a1), sqrt(2*  a2)) # Zeng and others (Part II)
    zz <- 1 + (1-v[i])*a3 - (1-u[i])*(1-a4)       # Zeng and others (Part II)
    zz[zz < 0] <- 0
    zz[zz > 1] <- 1
    return(zz)
  })
}
}
\references{
Bo\enc{š}{s}koskia, P., Debenjaka, A., Boshkoskab, B.M., 2018, Rayleigh copula for describing impedance data with application to condition monitoring of proton exchange membrane fuel cells: European Journal of Operational Research, v. 266, pp. 269--277, \doi{10.1016/j.ejor.2017.08.058}.

Zeng, X., Ren, J., Wang, Z., Marshall, S., and Durrani, T., 2014, Copulas for statistical signal processing (Part I)---Extensions and generalization: Signal Processing v. 14, pp. 691--702, \doi{10.1016/j.sigpro.2013.07.009}.

Zeng, X., Ren, J., Sun, M., Marshall, S., and Durrani, T., 2014, Copulas for statistical signal processing (Part II)---Simulation, optimal selection and practical applications: Signal Processing, v. 94, pp. 681--690, \doi{10.1016/j.sigpro.2013.07.006}.
}
\author{W.H. Asquith}
\seealso{\code{\link{M}}, \code{\link{P}}
}
\examples{
RAYcop(0.2, 0.8, para=0.8) # [1] 0.1994658  (by the dual Bessel functions)

RAYcop(0.8, 0.2, para=RAYcop(rho=rhoCOP(cop=RAYcop, para=0.8)))
# [1] 0.1994651 from polynomial conversion of Rho to Theta

\dontrun{
  # Recipe for assembling the Spearman Rho to Theta polynomial in sources.
  Thetas <- seq(0, 0.999, by=0.001); RHOs <- NULL
  for(p in Thetas)  RHOs <- c(RHOs, rhoCOP(cop=RAYcop, para=p))
  LM <- lm(Thetas ~ RHOs  + I(RHOs^2) + I(RHOs^4) + I(RHOs^6) - 1 )
  Rho2Theta <- function(rho) {
    coes <- c(1.32682824, -0.38876290, 0.09072305, -0.02921836)
    sapply(rho, function(r) coes[1]*r^1 + coes[2]*r^2 + coes[3]*r^4 + coes[4]*r^6)
  }
  plot(RHOs, Thetas, type="l", col=grey(0.8), lwd=12, lend=1,
       xlab="Spearman Rho", ylab="Rayleigh Copula Parameter Theta")
  lines(RHOs, Rho2Theta(RHOs), col="red", lwd=2) # }
}
\keyword{Rayleigh copula}
\keyword{copula (formulas)}
\keyword{copula}
\concept{Rayleigh copula}
