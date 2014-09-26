\name{tauCOP}
\alias{tauCOP}
\title{ The Kendall's Tau of a Copula }
\description{
Compute the measure of association known as \emph{Kendall's Tau} \eqn{\tau_\mathbf{C}} of a copula according to Nelson (2006, sec. 5.1.1 and p. 161) by
\deqn{\tau_\mathbf{C} = Q(\mathbf{C}, \mathbf{C}) = 4\int_0^1 \int_0^1
                         \mathbf{C}(u,v)\,\mathrm{d}\mathbf{C}(u,v) - 1\mbox{,}}
where \eqn{Q(\mathbf{C}, \mathbf{C})} is a concordance function of a copula with itself. However, Nelson (2006, p. 164) reports that this form is often not amenable to computation when a copula when there is a singular component to the copula and that the expression
\deqn{\tau_\mathbf{C} = 1 - 4\int_0^1 \int_0^1
                         \frac{\delta\mathbf{C}(u,v)}{\delta u}
                         \frac{\delta\mathbf{C}(u,v)}{\delta v}\,
                         \mathrm{d}u\mathrm{d}v}
is to be preferred. Such an expression hence relies on the partial numerical derivatives of the copula provided by \code{\link{derCOP}} and \code{\link{derCOP2}}. Nelson's preferred expression is used by the \code{tauCOP} function.  Nelson (2006, pp. 175--176) reports that the following relation between \eqn{\tau_\mathbf{C}} and \eqn{\rho_\mathbf{C}} (\code{\link{rhoCOP}}) exists
\eqn{-1 \le 3\tau - 2\rho \le 1} (see \code{\link{rhoCOP}} for more details).

Nelson (2006, pp. 160--161) lists some special identities involving the concordance function \eqn{Q(a,b)}:
\deqn{Q(\mathbf{M}, \mathbf{M}) = 4\int_0^1 u\,\mathrm{d}u - 1 = 1\mbox{,}}
\deqn{Q(\mathbf{M}, \mathbf{\Pi}) = 4\int_0^1 u^2\,\mathrm{d}u - 1 = 1/3\mbox{,}}
\deqn{Q(\mathbf{M}, \mathbf{W}) = 4\int_{1/2}^1 (2u-1)\,\mathrm{d}u - 1 = 0\mbox{,}}
\deqn{Q(\mathbf{W}, \mathbf{\Pi}) = 4\int_0^1 u(1-u)\,\mathrm{d}u - 1 = -1/3\mbox{,}}
\deqn{Q(\mathbf{W}, \mathbf{W}) = 4\int_0^1 0\,\mathrm{d}u - 1 = -1\mbox{, and}}
\deqn{Q(\mathbf{\Pi}, \mathbf{\Pi}) = 4\int_0^1\int_0^1 uv\,\mathrm{d}u\mathrm{d}v - 1 = 0\mbox{.}}
}
\usage{
tauCOP( cop=NULL,  para=NULL,
       cop2=NULL, para2=NULL, brute=FALSE, delta=0.002, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{cop2}{A second copula function;}
  \item{para2}{Vector of parameters or other data structure, if needed, to pass to the second copula;}
  \item{brute}{Should brute force be used instead of two nested \code{integrate()} functions in \R to perform the double integration;}
  \item{delta}{The \eqn{\mathrm{d}u} and \eqn{\mathrm{d}v} for the brute force integration using \code{brute}; and}
  \item{...}{Additional arguments to pass on to \code{\link{derCOP}} and \code{\link{derCOP2}}.}
}
\value{
  The value for \eqn{\tau_\mathbf{C}} is returned.
}
\note{
Although titled for computation of Kendall's Tau, this function can trigger the concordance function \eqn{Q(\mathbf{C}_1, \mathbf{C}_2)} (see Nelson (2006, pp. 158--159) when given two different copulas and respective parameters as arguments. A useful relation is
\deqn{\int_0^1\int_0^1 \mathbf{C}_1(u,v)\,\mathrm{d}\mathbf{C}_2(u,v) =
      \frac{1}{2} - \int_0^1\int_0^1 \frac{\delta}{\delta u}\mathbf{C}_1(u,v)\,\frac{\delta}{\delta v}\mathbf{C}_2(u,v)\,\mathrm{d}u\mathrm{d}v\mbox{,} }
where \eqn{\mathbf{C}_1(u,v)} is the first copula and \eqn{\mathbf{C}_2(u,v)} is the second copula.
}
\references{
Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.
}
\author{ W.H. Asquith}
\seealso{\code{\link{blomCOP}}, \code{\link{giniCOP}}, \code{\link{rhoCOP}}, \code{\link{derCOP}}, \code{\link{derCOP2}}}
\examples{
tauCOP(cop=PSP) # 1/3
\dontrun{
tauCOP(cop=PSP, brute=TRUE) # 0.3306625
# CPU heavy example showing that the dual-integration (fast) results in
# a Kendall's Tau that matches a sample version
dotau <- function(n) {
   uv <- simCOP(n=n, cop=PSP, ploton=FALSE, points=FALSE)
   return(cor(uv$U, uv$V, method="kendall"))
}
taus <- replicate(100, dotau(1000))
tau.sample <- mean(taus); print(tau.sample) # 0.3333763
}
\dontrun{
# The rational values or integers may be derived analytically
tauCOP(cop=M, cop2=M) #   1, correct
tauCOP(cop=M, cop2=P) # 1/3, correct
tauCOP(cop=P, cop2=M) # 1/3, correct
tauCOP(cop=M, cop2=W) #   0, correct
tauCOP(cop=W, cop2=M) # throws warning, swaps copulas, == tauCOP(M,W)
tauCOP(cop=W, cop2=P) # throws warning, swaps copulas, approx. -1/3
tauCOP(cop=P, cop2=W) # -1/3, correct
tauCOP(cop=P, cop2=P) #    0, correct
tauCOP(cop=M, cop2=W, brute=TRUE) #    0, correct
}
\dontrun{
para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=0.00395, para2=4.67, alpha=0.9392, beta=0.5699)
tauCOP(cop=composite2COP, para=para)

para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=0.14147, para2=20.96, alpha=0.0411, beta=0.6873)
tauCOP(cop=composite2COP, para=para)

para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=0.10137, para2=4492.87, alpha=0.0063, beta=0.0167)
# Theoretical attempt fails because para2 is large and thus a singularity
# is emerging and internal copula swapping does not help
tauCOP(cop=composite2COP, para=para) # fails
tauCOP(cop=composite2COP, para=para, brute=TRUE) # about 0.95
}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}

