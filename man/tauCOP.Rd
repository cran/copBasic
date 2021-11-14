\encoding{utf8}
\name{tauCOP}
\alias{tauCOP}
\alias{concordCOP}
\title{ The Kendall Tau and Concordance Function of a Copula }
\description{
Compute the measure of association known as the \emph{Kendall Tau} (\eqn{\tau_\mathbf{C}}) of a copula (\eqn{\tau_\mathbf{C}}) according to Nelsen (2006, sec. 5.1.1 and p. 161) by
\deqn{\tau_\mathbf{C} = \mathcal{Q}(\mathbf{C}, \mathbf{C}) = 4\int\!\!\int_{\mathcal{I}^2}
                         \mathbf{C}(u,v)\,\mathrm{d}\mathbf{C}(u,v) - 1\mbox{,}}
where \eqn{\mathcal{Q}(\mathbf{C}, \mathbf{C})} is a \emph{concordance function} (\code{concordCOP}) of a copula with itself. Nelsen (2006, p. 164) reports however that this form is often not amenable to computation when there is a singular component to the copula and that the expression
\deqn{\tau_\mathbf{C} = 1 - 4\int\!\!\int_{\mathcal{I}^2}
                         \frac{\delta\mathbf{C}(u,v)}{\delta u}
                         \frac{\delta\mathbf{C}(u,v)}{\delta v}\,
                         \mathrm{d}u\mathrm{d}v}
is to be preferred. Such an expression hence relies on the partial numerical derivatives of the copula provided by \code{\link{derCOP}} and \code{\link{derCOP2}}. Nelsen's preferred expression is used by the \code{tauCOP} function.  Nelsen (2006, pp. 175--176) reports that the following relation between \eqn{\tau_\mathbf{C}} and \eqn{\rho_\mathbf{C}} (\code{\link{rhoCOP}}) exists
\eqn{-1 \le 3\tau - 2\rho \le 1} (see \code{\link{rhoCOP}} for more details).

Nelsen (2006, pp. 160--161) lists some special identities involving \eqn{\mathcal{Q}(\mathbf{C}_1,\mathbf{C}_2)}:
\deqn{\mathcal{Q}(\mathbf{M}, \mathbf{M}) = 4\int_0^1 u\,\mathrm{d}u - 1 = 1\mbox{,}}
\deqn{\mathcal{Q}(\mathbf{M}, \mathbf{\Pi}) = 4\int_0^1 u^2\,\mathrm{d}u - 1 = 1/3\mbox{,}}
\deqn{\mathcal{Q}(\mathbf{M}, \mathbf{W}) = 4\int_{1/2}^1 (2u-1)\,\mathrm{d}u - 1 = 0\mbox{,}}
\deqn{\mathcal{Q}(\mathbf{W}, \mathbf{\Pi}) = 4\int_0^1 u(1-u)\,\mathrm{d}u - 1 = -1/3\mbox{,}}
\deqn{\mathcal{Q}(\mathbf{W}, \mathbf{W}) = 4\int_0^1 0\,\mathrm{d}u - 1 = -1\mbox{, and}}
\deqn{\mathcal{Q}(\mathbf{\Pi}, \mathbf{\Pi}) = 4\int\!\!\int_{\mathcal{I}^2} uv\,\mathrm{d}u\mathrm{d}v - 1 = 0\mbox{.}}


Kendall Tau also can be expressed in terms of the \emph{Kendall Function} (\eqn{F_K(z)}; \code{\link{kfuncCOP}}):
\deqn{\tau_\mathbf{C} = 3 - 4\int_0^1 F_K(t)\,\mathrm{d}t\mbox{,}}
which is readily verified by code shown in \bold{Examples}. This definition might be useful if integration errors are encountered for some arbitrary copula and arbitrary parameter set. In fact, should two attempts (see source code) at dual integration of the partial derivatives occur, the implementation switches over to integration of the Kendall Function (\emph{e.g.} \code{tauCOP(cop=N4212cop, para=2)}). Note that Durante and Sempi have erroneously dropped the multiplication by \dQuote{\eqn{4}} as shown above in their definition of \eqn{\tau_\mathbf{C}} as a function of \eqn{F_K(t)} (Durante and Sempi, 2015, eq. 3.9.4, p. 121).
}
\usage{
tauCOP( cop=NULL,  para=NULL,
       cop2=NULL, para2=NULL, as.sample=FALSE, brute=FALSE, delta=0.002, ...)

concordCOP(cop=NULL,  para=NULL, cop2=NULL, para2=NULL, ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{cop2}{A second copula function;}
  \item{para2}{Vector of parameters or other data structure, if needed, to pass to the second copula;}
    \item{as.sample}{A logical controlling whether an optional \R \code{data.frame} in \code{para} is used to compute the \eqn{\hat\tau} by dispatch to \code{cor()} function in \R with \code{method = "kendall"};}
  \item{brute}{Should brute force be used instead of two nested \code{integrate()} functions in \R to perform the double integration;}
  \item{delta}{The \eqn{\mathrm{d}u} and \eqn{\mathrm{d}v} for the brute force integration using \code{brute}; and}
  \item{...}{Additional arguments to pass on to \code{\link{derCOP}} and \code{\link{derCOP2}}.}
}
\value{
  The value for \eqn{\tau_\mathbf{C}} is returned.
}
\note{
Although titled for computation of the Kendall Tau, the \code{tauCOP} function is the implementation of the \emph{concordance function} \eqn{\mathcal{Q}(\mathbf{C}_1, \mathbf{C}_2)} (see Nelsen (2006, pp. 158--159) when given two different copulas and respective parameters as arguments. The function \code{concordCOP} just dispatches to \code{tauCOP}. A useful relation is
\deqn{\int\!\!\int_{\mathcal{I}^2} \mathbf{C}_1(u,v)\,\mathrm{d}\mathbf{C}_2(u,v) =
      \frac{1}{2} - \int\!\!\int_{\mathcal{I}^2} \frac{\delta}{\delta u}\mathbf{C}_1(u,v)\,\frac{\delta}{\delta v}\mathbf{C}_2(u,v)\,\mathrm{d}u\mathrm{d}v\mbox{,} }
where \eqn{\mathbf{C}_1(u,v)} is the first copula and \eqn{\mathbf{C}_2(u,v)} is the second copula.

Nelsen \emph{et al.} (2001, p. 281) lists several measures of association defined by the concordance function:\cr
\eqn{\mbox{}\quad\mathrm{1.}\quad\mbox{}}\eqn{\tau_\mathbf{C} = \quad\ \mathcal{Q}(\mathbf{C}, \mathbf{C})} : (Kendall Tau; \code{tauCOP});\cr
\eqn{\mbox{}\quad\mathrm{2.}\quad\mbox{}}\eqn{\rho_\mathbf{C} = 3\cdot\mathcal{Q}(\mathbf{C}, \mathbf{\Pi})} : (Spearman Rho; \code{\link{rhoCOP}});\cr
\eqn{\mbox{}\quad\mathrm{3.}\quad\mbox{}}\eqn{\gamma_\mathbf{C} = 2\cdot\mathcal{Q}(\mathbf{C}, [\mathbf{M}+\mathbf{W}]/2)} : (Gini Gamma; \code{\link{giniCOP}}); and\cr
\eqn{\mbox{}\quad\mathrm{4.}\quad\mbox{}}\eqn{\psi_\mathbf{C} = \frac{3}{2}\cdot\mathcal{Q}(\mathbf{C}, \mathbf{M}) - \frac{1}{2}} : (Spearman Footrule; \code{\link{footCOP}}).
}
\references{
Durante, F., and Sempi, C., 2015, Principles of copula theory: Boca Raton, CRC Press, 315 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Nelsen, R.B., Quesada-Molina, J.J., \enc{Rodríguez-Lallena}{Rodriguez-Lallena}, J.A., \enc{Úbeda-Flores}{Ubeda-Flores}, M., 2001, Distribution functions of copulas---A class of bivariate probability integral transforms: Statistics and Probability Letters, v. 54, no. 3, pp. 277--282.
}
\author{ W.H. Asquith}
\seealso{\code{\link{blomCOP}}, \code{\link{footCOP}}, \code{\link{giniCOP}},
         \code{\link{hoefCOP}}, \code{\link{rhoCOP}}, \code{\link{wolfCOP}},
         \code{\link{joeskewCOP}}, \code{\link{uvlmoms}},
         \code{\link{derCOP}}, \code{\link{derCOP2}}, \code{\link{kfuncCOP}}}
\examples{
\dontrun{
tauCOP(cop=PSP) # 1/3
# Now compute Kendall Tau via integration of the Kendall Function.
# 3 - 4*integrate(function(t) kfuncCOP(t, cop=PSP), 0, 1)$value # 0.3333314}

\dontrun{
tauCOP(cop=PSP, brute=TRUE) # 0.3306625
# CPU heavy example showing that the dual-integration (fast) results in
# a Kendall's Tau that matches a sample version
dotau <- function(n) {
   uv <- simCOP(n=n, cop=PSP, ploton=FALSE, points=FALSE)
   return(cor(uv$U, uv$V, method="kendall"))
}
set.seed(817600)
taus <- replicate(100, dotau(100))
tau.sample <- mean(taus); print(tau.sample) # 0.3342034}

\dontrun{
# Nelsen (2006, pp. 160-161, numeric results shown thereine)
# The rational values or integers may be derived analytically.
tauCOP(cop=M, cop2=M) #   1, correct
tauCOP(cop=M, cop2=P) # 1/3, correct
tauCOP(cop=P, cop2=M) # 1/3, correct
tauCOP(cop=M, cop2=W) #   0, correct
tauCOP(cop=W, cop2=M) # throws warning, swaps copulas, == tauCOP(M,W)
tauCOP(cop=W, cop2=P) # throws warning, swaps copulas, approx. -1/3
tauCOP(cop=P, cop2=W) # -1/3, correct
tauCOP(cop=P, cop2=P) #    0, correct
tauCOP(cop=M, cop2=W, brute=TRUE) #    0, correct}

\dontrun{
para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=0.00395, para2=4.67, alpha=0.9392, beta=0.5699)
tauCOP(cop=composite2COP, para=para) # -0.4671213

para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=0.14147, para2=20.96, alpha=0.0411, beta=0.6873)
tauCOP(cop=composite2COP, para=para) # +0.1950727

para <- list(cop1=PLACKETTcop,  cop2=PLACKETTcop,
             para1=0.10137, para2=4492.87, alpha=0.0063, beta=0.0167)
# Theoretical attempt fails because para2 is large and thus a singularity
# is emerging and internal copula swapping does not help.
tauCOP(cop=composite2COP, para=para) # fails (0.94+-.01)
tauCOP(cop=composite2COP, para=para, brute=TRUE) # about 0.94+-.01}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{Nelsen (2006) Examples and Exercises}
\keyword{concordance function}
\keyword{literature errors and inconsistencies}
\keyword{Kendall Tau}
