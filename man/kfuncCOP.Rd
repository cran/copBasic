\encoding{utf8}
\name{kfuncCOP}
\alias{kfuncCOP}
\alias{kmeasCOP}
\title{ The Kendall (Distribution) Function of a Copula }
\description{
To begin, there are at least three terms in the literature for what appear as the same function supported by the \code{kfuncCOP} function.  The Kendall Function also is known as Kendall Distribution Function (Nelsen, 2006, p. 163) and Kendall Measure (Salvadori \emph{et al.}, 2007, p. 148). Each of these is dealt with in sequel to set the manner of the rather lengthy documentation for this function.

\emph{KENDALL FUNCTION}---The \emph{Kendall Function} (\eqn{F_K}) (Joe, 2014, pp. 419--422) is the cumulative distribution function (CDF) of the vector \eqn{\mathbf{U} = (U_1, U_2, \ldots)} or \eqn{\mathbf{U} = (u,v)} (bivariate) where \eqn{\mathbf{U}} is distributed as the copula: \eqn{\mathbf{U} \sim \mathbf{C}(u,v)}. Letting \eqn{Z} be the random variable for \eqn{\mathbf{C}(u,v): Z = \mathbf{C}(u,v)}, the Kendall Function is defined as

\deqn{F_K(z; \mathbf{C}) = \mathrm{Pr}[Z \le z; \mathbf{U} \sim \mathbf{C}(u,v)]\mbox{,}}

where \eqn{F_K} is the nonexceedance probability of the joint probability \eqn{z} stemming from the \eqn{\mathbf{C}}. Note that unlike its univariate counterpart, \eqn{F_K(z)} is rarely uniformly distributed (Nelsen \emph{et al.}, 2001, p. 278). The inverse \eqn{F_K^{(-1)}(z)} is implemented by the \code{\link{kfuncCOPinv}} function, which could be used for simulation of correct joint probability using a single unformly distributed \eqn{\sim} U(0,1) random variable. A reminder is needed that \eqn{Z} is the \bold{joint probability} and \eqn{F_K(z)} is the Kendall Function.

Joe (2014) and others as cited list various special cases of \eqn{F_K(z)}, inequalities, and some useful identities suitable for validation study:\cr
\eqn{\mbox{}\quad\bullet\quad\mbox{}}For \eqn{\mathbf{M}(u,v)} (see \code{\link{M}}): \eqn{F_K(z) = z} for all \eqn{0 < z < 1} for all \eqn{d \ge 2} dimensions;\cr
\eqn{\mbox{}\quad\bullet\quad\mbox{}}For \eqn{\mathbf{W}(u,v)} (see \code{\link{W}}): \eqn{F_K(z) = 1} for all \eqn{0 < z < 1} for \eqn{d = 2} (bivariate only);\cr
\eqn{\mbox{}\quad\bullet\quad\mbox{}}For \eqn{\mathbf{\Pi}(u,v)} (see \code{\link{P}}): \eqn{F_K(z) = z - z \log z} for \eqn{0 < z < 1} for \eqn{d = 2} (bivariate only);\cr
\eqn{\mbox{}\quad\bullet\quad\mbox{}}For any \eqn{\mathbf{C}}: \eqn{z \le F_K(z)} for \eqn{0 < z < 1}; and\cr
\eqn{\mbox{}\quad\bullet\quad\mbox{}}For any \eqn{\mathbf{C}}: \eqn{\mathrm{E}[Z] = 1 - \int_0^1 F_K(t)\,\mathrm{d}t \ge z} (Nelsen, 2001, p. 281) --- Z expectation, not \eqn{F_K}!\cr
\eqn{\mbox{}\quad\bullet\quad\mbox{}}For any \eqn{\mathbf{C}}: \eqn{\tau_\mathbf{C} = 3 - 4\int_0^1 F_K(t)\,\mathrm{d}t} (Nelsen, 2006, p. 163; see \code{\link{tauCOP}} [\bold{Examples}]).\cr
\eqn{\mbox{}\quad\bullet\quad\mbox{}}For any \eqn{\mathbf{C}}: \eqn{F_K(t)} does not uniquely determine the copula.

This last item is from Durante and Sempi (2015, p. 118) and see later discussion herein concerning an example of theirs. By coincidence within a few days before receipt of the Durante and Sempi book, experiments using \code{kfuncCOP} suggested that numerically the Galambos (\code{\link{GLcop}}), Gumbel-Hougaard (\code{\link{GHcop}}), and H\enc{ü}{u}sler-Reiss (\code{\link{HRcop}}) extreme value copulas for the same \emph{Kendall Tau} (\eqn{\tau_\mathbf{C}}) all have the same \eqn{F_K(t)}. Therefore, do all \emph{EV}-copulas have the same Kendall Function? Well in fact, they do and Durante and Sempi (2015, p. 207) show that \eqn{F_K(z) = z - (1 - \tau_\mathbf{C})z \log(z)} for an \emph{EV}-copula.

Joe (2014, p. 420) also indicates that strength of \emph{lower-tail dependence} (\code{\link{taildepCOP}}) affects \eqn{F_K(z)} as \eqn{z \rightarrow 0^{+}}, whereas strength of \emph{upper-tail dependence} affects \eqn{F_K(z)} as \eqn{z \rightarrow 1^{-}}. (A demonstration of tail dependence dependence is made in section \bold{Note}.) Also compared to \emph{comonotonicity copula} [\eqn{\mathbf{M}}] there are no \emph{countermonotonicity copula} (\eqn{\mathbf{W}_{d > 2}}) for dimensions greater the bivariate (Joe, 2014, p. 214)

Joe (2014) does not explicitly list an expression of \eqn{F_K(z)} that is computable directly for any \eqn{\mathbf{C}(u,v)}, and Nelsen (2006, p. 163) only lists a form (see later in documentation) for \emph{Archimedean copulas}. Salvadori \emph{et al.} (2007, eq. 3.47, p. 147) also list the Archimedean form; however, Salvadori \emph{et al.} (2007, eq. 3.49, p. 148) \bold{also list a form computable directly for any} \eqn{\mathbf{C}(u,v)}. Considerable numerical experiments and derivations involving the \eqn{\mathbf{\Pi}(u,v)} copula and results for \eqn{K_\mathbf{C}(z)} shown later, indicate that the correct Kendall form for any \eqn{\mathbf{C}(u,v)} is

\deqn{F_K(z) \equiv z + \int_z^1 \frac{\delta\mathbf{C}(u,t)}{\delta u}\,\mathrm{d}u\mbox{,}}

where \eqn{t = \mathbf{C}^{(-1)}(u,z)} for \eqn{0 \le z \le 1}, \eqn{t} can be computed by the \code{\link{COPinv}} function, and the partial derivative \eqn{\delta\mathbf{C}(u,t)/\delta u} can be computed by the \code{\link{derCOP}} function. It is a curiosity that this form is not in Joe (2014), Nelsen \emph{et al.} (2001, 2003), Nelsen (2006), or Salvadori \emph{et al.} (2007).\cr

\emph{KENDALL MEASURE}---The actual expression for any \eqn{\mathbf{C}(u,v)} by Salvadori \emph{et al.} (2007, eq. 3.49, p. 148) is for \emph{Kendall's Measure} (\eqn{K_\mathbf{C}}) of a copula:
\deqn{K_\mathbf{C}(z) = z - \int_z^1 \frac{\delta\mathbf{C}(u,t)}{\delta u}\,\mathrm{d}u\mbox{,}}
where \eqn{t = \mathbf{C}^{(-1)}(u,z)} for \eqn{0 \le z \le 1}. Those authors report that \eqn{K_\mathbf{C}(z)} is the CDF of a random variable \eqn{Z} whose distribution is \eqn{\mathbf{C}(u,v)}. This is clearly appears to be the same meaning as Joe (2014) and Nelsen (2006). The minus \dQuote{\eqn{-}} in the above equation is very important.

Salvadori \emph{et al.} (2007, p. 148) report that \dQuote{the function \eqn{K_\mathbf{C}(z)} represents a fundamental tool for calculating the return period of extreme events.} The complement of \eqn{K_\mathbf{C}(z)} is \eqn{\overline{K}_\mathbf{C}(z) = 1 - K_\mathbf{C}(z)}, and the \eqn{\overline{K}_\mathbf{C}(z)} inverse
\deqn{\frac{1}{1 - K_\mathbf{C}(z)} = \frac{1}{\overline{K}_\mathbf{C}(z)} = T_{\mathrm{KC}}}
is referred to as a \emph{secondary return period} (Salvadori \emph{et al.}, 2007, pp. 161--170).\cr

\emph{KENDALL DISTRIBUTION FUNCTION}---Nelsen (2006, p. 163) defines the \emph{Kendall Distribution Function} (say \eqn{K^\star_\mathbf{C}(t)}) as

\deqn{K^\star_\mathbf{C}(t) = t - \frac{\phi(t)}{\phi'(t^{+})}\mbox{,}}

where \eqn{\phi(t)} is a \emph{generator function} of an \emph{Archimedean copula} and \eqn{\phi'(t^{+})} is a one-sided derivative (Nelsen, 2006, p. 125), and \eqn{\phi(t)} is \eqn{\phi(\mathbf{C}(u,v)) = \phi(u) + \phi(v)}. This same form is listed by Salvadori \emph{et al.} (2007, eq. 3.47).

Nelsen (2006) does not seem to list a more general definition for any \eqn{\mathbf{C}(u,v)}. Because there is considerable support for Archimedean copulas in \R, \pkg{copBasic} has deliberately been kept from being yet another Archimedean-based package. This is made for more fundamental theory and pedogogic reasons without the algorithmic efficiency relative to the many convenient properties of Archimedean copulas.

The similarity of \eqn{F_K(z)}, \eqn{K_\mathbf{C}(z)}, and \eqn{K^\star_\mathbf{C}(t)}, however, is obvious---research shows that there are no syntatic differences between \eqn{F_K(z)} and \eqn{K_\mathbf{C}(t)} and \eqn{K^\star_\mathbf{C}(z)}---they all are the CDF of the joint probability \eqn{Z} of the copula. Consider now that Salvadori \emph{et al.} show \eqn{K_\mathbf{C}} having the form \eqn{a - b} and not a form \eqn{a + b} as previously shown for \eqn{F_K(z)}. Which form is thus correct? The greater bulk of this documentation now deals with that question, and it must be concluded that Salvadori \emph{et al.} (2007, eq. 3.49) definition for \eqn{K_\mathbf{C}(z)} has a typesetting error.
}
\usage{
kfuncCOP(z, cop=NULL, para=NULL, wrtV=FALSE, as.sample=FALSE,verbose=FALSE,...)
kmeasCOP(z, cop=NULL, para=NULL, wrtV=FALSE, as.sample=FALSE,verbose=FALSE,...)
}
\arguments{
  \item{z}{The values for \eqn{z};}
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{wrtV}{A logical to toggle between with respect to \eqn{v} or \eqn{u} (default);}
  \item{as.sample}{A control on whether an optional \R \code{data.frame} in \code{para} is used to compute the empirical \eqn{\hat{F}_K(z)}. Let vector length of \code{para} be denoted \eqn{m} and \eqn{i = (1,\ldots,m)}, if \code{as.sample=TRUE}, then for each of the probability pairs in \code{para}, the \emph{empirical copula} (\code{\link{EMPIRcop}}) function with additional arguments \code{...} is used to generate a vector \eqn{(0, F^\sharp_{K,m}, 1)} of length \eqn{m+2}, then a vector of \eqn{(0, z^\sharp_m, 1)} again of length \eqn{m+2} where \eqn{z^\sharp = (i-0.5)/m} is created and linear interpolation by the \code{approx()} function in \R for each of the \eqn{z} values in \code{z} is used to estimate \eqn{\hat{F}_K(z)} (see source code). If \code{as.sample="genest"} then the \eqn{\hat{F}_K(z)} is estimated without interpolation using a simple empirical copula basis and \dQuote{pseudo-observations} after Genest \emph{et al.} (2006, p. 339). The default \eqn{\hat{F}_K} (\code{as.sample=TRUE}) is the linear interpolation based on the default call of Weibull form of the empirical copula, but that can be confirmed by \code{ctype="weibull"} (see \bold{Note});}
  \item{verbose}{A logical supressing warnings from \code{integrate()} in \R that are usually related to \dQuote{integral divergence} for \eqn{z \rightarrow 0^{+}}. The constructed behavior of \code{kfuncCOP} is to return \eqn{F_K(z \rightarrow 0^{+}) = 0} if numerical integration returns \code{NULL}---such a construction is made to avoid \dQuote{end points not of opposite sign} during \eqn{F_K(z)} inversion by \code{\link{kfuncCOPinv}}; and}
  \item{...}{Additional arguments to pass.}
}
\value{
  The value(s) for \eqn{F_K(z)} is returned.
}
\note{
\emph{VALIDATION STUDY}---A validation study using the \emph{Independence copula} (\eqn{\mathbf{\Pi =}} \eqn{uv}; \code{\link{P}}) with theoretical results of Joe (2014) and empiricism is readily performed using the expression for \eqn{F_K(z)}:
\preformatted{
  Z <- sort(c(0.01, seq(0,1, by=0.05), 0.99))       # ** Joint probabilities **
  UV <- simCOP(n=4000, cop=P, graphics=FALSE);      kendF <- Z - Z*log(Z)
  emp.kendF  <- kfuncCOP(Z, para=UV, as.sample="genest") # emp. Kendall func
  theo.kendF <- kfuncCOP(Z, cop=P) # theo. Kendall func, numeric integration
  plot(Z, kendF, type="l", col=3, lwd=4, lty=2, xlim=c(0,1), ylim=c(0,1),
       xlab="COPULA(u,v) VALUE [JOINT PROBABILITY]",
       ylab="KENDALL FUNCTION, A NONEXCEEDANCE PROBABILITY") # analytical
  points(Z,     kendF, col=3, lwd=1, lty=2, pch=16) # green (theo. values)
  points(Z, emp.kendF, col=4, lwd=2, cex=1.5)       # blue (empirical values)
  lines(Z, theo.kendF, col=2) # red (theoretical line by numerical integration)
  mtext("Kendall Functions: Independence Copula")
}
The figure so produced shows that the theoretical relation in Joe (2014) is correct because the empirical values from the simulated sample (\emph{Empirical Kendall Function}; Nelsen \emph{et al.}, 2003) match other curves quite well. Rerunning the above code with either \eqn{\mathbf{M}} (\code{\link{M}}) or \eqn{\mathbf{W}} (\code{\link{W}}) copulas will show that the special cases listed above are consistent with the empirical estimates. The case of \eqn{\mathbf{W}(u,v)} is degenerate at \eqn{z=0} so the empirical computation is in error for the smallest \eqn{z} given because of interpolation. The \eqn{\mathbf{M}} copula has \eqn{F_K} along the 1:1 line.

Now consider a more comprehensive demonstration using the \code{\link{N4212cop}} copula with some relatively weak dependence in \eqn{\Theta = 1.17}.
\preformatted{
  Theta <- 1.17; print(rhoCOP(cop=N4212cop, para=Theta)) # Spearman rho = 6/10
  Z <- sort(c(0.01, seq(0,1, by=0.05), 0.99))      # ** Joint probabilities **
  UV <- simCOP(n=16000,      cop=N4212cop,   para=Theta, graphics=TRUE)
  empir.kendF <- kfuncCOP(Z, as.sample=TRUE, para=UV, ctype="weibull")
  kwrtU       <- kfuncCOP(Z, cop=N4212cop,   para=Theta, wrtV=FALSE)
  kwrtV       <- kfuncCOP(Z, cop=N4212cop,   para=Theta, wrtV=TRUE )
  plot(Z, empir.kendF, type="p", col=2, lwd=7, lty=2, xlim=c(0,1),ylim=c(0,1),
       xlab="COPULA(u,v) VALUE [JOINT PROBABILITY]",
       ylab="KENDALL FUNCTION, A NONEXCEEDANCE PROBABILITY")
  abline(0,1, lty=2, lwd=0.8); mtext("Kendall Functions: N4212(1.17) Copula")
  lines(Z, kwrtU, col=4, lwd=4, lty=2); lines(Z, kwrtV, col=3, lwd=1, lty=2)
}
The figure so produced again shows congruence between the two theoretical computations and the empirical curve. Now consider another comprehensive demonstration using the \code{\link{PLACKETTcop}} copula with some strong negative dependence in \eqn{\Theta = 0.04}.
\preformatted{
  Theta <- 0.04
  Z <- sort(c(0.01, seq(0,1, by=0.05), 0.99))      # ** Joint probabilities **
  UV <- simCOP(n=2600,       cop=PLACKETTcop,   para=Theta, graphics=TRUE)
  empir.kendF <- kfuncCOP(Z, as.sample="hazen", para=UV)
  kwrtU       <- kfuncCOP(Z, cop=PLACKETTcop,   para=Theta, wrtV=FALSE)
  kwrtV       <- kfuncCOP(Z, cop=PLACKETTcop,   para=Theta, wrtV=TRUE )
  plot(Z, empir.kendF, type="p", col=2, lwd=7, lty=2, xlim=c(0,1),ylim=c(0,1),
       xlab="COPULA(u,v) VALUE [JOINT PROBABILITY]",
       ylab="KENDALL FUNCTION, A NONEXCEEDANCE PROBABILITY")
  abline(0,1, lty=2, lwd=0.8); mtext("Kendall Function: Plackett Copula")
  lines(Z, kwrtU, col=4, lwd=4, lty=2); lines(Z, kwrtV, col=3, lwd=1, lty=2)
}
The figure so produced again shows congruence between the two theoretical computations and the empirical curve.

Another comparison of \eqn{F_K(z)} is useful and concerns \emph{lower-} and \emph{upper-tail dependency} parameters (\code{\link{taildepCOP}}) with a comparison of three different copulae all having the same Kendall's Tau. The following code computes and draws the respective \eqn{F_K(z)}:
\preformatted{
  # Given a Kendall Tau of 0.4 and the GHcop, N4212, and Plackett copulas
  # parameters respectively are:
  Phi <- 1.666667; Nu <- 1.111119; Mu <- 6.60344
  Z <- seq(0.005, 0.995, by=0.005) #  ** Joint probabilities **
  GHkenf    <- kfuncCOP(Z, cop=GHcop,       para=Phi, wrtV=FALSE)
  N4212kenf <- kfuncCOP(Z, cop=N4212cop,    para=Nu,  wrtV=FALSE)
  PLkenf    <- kfuncCOP(Z, cop=PLACKETTcop, para=Mu,  wrtV=FALSE)
  plot(qnorm(GHkenf), Z, type="l", col=1, lwd=2, xlim=c(-3,3), ylim=c(0,1),
       xlab="KENDALL FUNCTION, AS STANDARD NORMAL VARIATES",
       ylab="COPULA(u,v) VALUE, AS NONEXCEEDANCE PROBABILITY") # black curve
  lines(qnorm(N4212kenf), Z, col=2, lwd=2)                     # red curve
  lines(qnorm(PLkenf),    Z, col=4, lwd=2)                     # blue curve
}
The red curve for the \eqn{\mathbf{N4212}(\Theta{=}1.1)} copula (\code{\link{N4212cop}}) is higher on the left, which shows the impact of its larger lower-tail dependency (\eqn{\lambda_\mathbf{GH}^L{=}0 < \lambda_\mathbf{N4212}^L{=}0.535}), whereas the black curve for the \eqn{\mathbf{GH}(\Theta{=}1.67)} copula (\code{\link{GHcop}}) is similarly (about same magnitude) higher on the right, which shows the impact of its larger upper-tail dependency (\eqn{\lambda_\mathbf{N4212}^L{=}0 < \lambda_\mathbf{GH}^U{=}0.484}). The blue curve for the \eqn{\mathbf{PL}(\Theta = 6.60)} copula (\code{\link{PLACKETTcop}}) nearly overwrites on the left the \eqn{\mathbf{GH}} curve, which is a reflection of both copulae having zero lower-tail dependencies (\eqn{\lambda_\mathbf{GH}^L = \lambda_\mathbf{PL}^L = 0}). Finally, as anticipated by \eqn{\lambda^U}, the curve on the right for the \eqn{\mathbf{N4212}} is just slightly larger for the \eqn{\mathbf{PL}} because the \eqn{\lambda_\mathrm{PL}^U{=}0 < \lambda_\mathbf{N4212}^U{=}0.134} (a small diference however), and again on the right, the \eqn{\mathbf{N4212}} curve is considerably smaller than the \eqn{\mathbf{GH}} because \eqn{\lambda_\mathbf{N4212}^U{=}0 < \lambda_\mathbf{GH}^U{=}0.484}.

Durante and Sempi (2015, p. 118) provide an example of two copula (\eqn{\mathbf{C}_1} and \eqn{\mathbf{C}_2}) having the same \eqn{F_K(z) = \mathrm{min}(2z, 1)}. Let us check that out:
\preformatted{
  "C1" <- function(u,v, ...) {
      if (length(u) == 1) { u <- rep(u, length(v)) } else
      if (length(v) == 1) { v <- rep(v, length(u)) }
      sapply(1:length(u), function(i) {
            min(c(u[i], max(c(v[i]/2, u[i]+v[i]-1)))) })
  }
  "C2" <- function(u,v, ...) {
      if (length(u) == 1) { u <- rep(u, length(v)) } else
      if (length(v) == 1) { v <- rep(v, length(u)) }
      sapply(1:length(u), function(i) { g <- 1/2
       max(c(0, u[i]+v[i]-1, min(c(u[i], v[i]-g)), min(c(u[i]-g, v[i])))) })
  }
  DSkf <- function(t) sapply(t, function(z) min(c(2*z, 1)))
  zs <- seq(0,1, by=.01); plot(zs, DSkf(zs), col=2, cex=3) # red dots (theory)
  lines(zs, kfuncCOP(zs, cop=C1), lwd=4, col=7) # thick yellow line
  lines(zs, kfuncCOP(zs, cop=C2), lwd=1, col=1) # thin black line
}
The plot so produced shows indeed that the numerical operations by \code{kfuncCOP} solidly work on these two strictly singular copulas \eqn{\mathbf{C}_1} and \eqn{\mathbf{C}_2} that are different from the two singular \eqn{\mathbf{M}} and \eqn{\mathbf{W}} copulas. The \eqn{F_K(z)} curves exactly matching the theoretical curve provided by Durante and Sempi are produced.

\emph{CONVERSATIONAL ASIDE}---Interestingly, Durante and Sempi (2015, pp. 115--121), like other authors of major works cited herein, do not list a general expression for the \eqn{F_K(z)} as a function of any \eqn{\mathbf{C}(u,v)}. Those authors develop the idea of Kendall Function well and show results but for the author (Asquith) the jump based of Theorem 3.9.2 to an expression, such as shown above for \eqn{F_K(z)} based on \eqn{\mathbf{C}(u,t)/\delta u}, to an usable form for any \eqn{\mathbf{C}(u,v)} is difficult.

This is a fascinating topic because if the Kendall Function is a reasonably important component of copula theory, then why so much difficulty in finding a canonical reference? For this one piece, whereas so much of \pkg{copBasic} features are quite nomenclaturely clear in say Nelsen (2006) or Joe (2014) but somehow not for the Kendall Function.

Perhaps to the mathematicians, the descriptions (nomenclature) used in all but Salvadori \emph{et al.} (2007) are clear to intended readers. But even Salvadori \emph{et al.} seemingly show theirs in error---perhaps the author (Asquith) has missed something fundamental, but the validations shown in this documentation prove at least that \code{kfuncCOP} does what it is supposed to be doing but perhaps for the wrong reasoning. Lastly, Durante and Sempi have an error in their expression for Kendall's Tau as a function of \eqn{F_K(z)} (see \code{\link{tauCOP}}).


\emph{SECONDARY RETURN PERIOD}---As for \dQuote{Kendall Measure} (after the lead of Salvadori \emph{et al.} [2007, pp. 161--170]), the following code shows for purposes of discussing \emph{secondary return period} that \eqn{F_K(z)} is correct and once and for all as defined in this documentation \eqn{F_K(z) \not\equiv  K_\mathbf{C}(z)}. The secondary return period (\eqn{T_K}) is the expected interarrival time between events exceeding a \eqn{T}-year joint probability either from \eqn{U}, from \eqn{V}, or both. Let us use the 100-year level (primary return period; \eqn{T = 100}), the Gumbel-Hougaard copula (\code{\link{GHcop}}) \eqn{F^{\mathbf{GH}(3.055)}_K(z)} where the choice of \eqn{\Theta = 3.055} is made to match discussion in \code{\link{copBasic-package}} and Salvadori \emph{et al.} (2007, table 3.3, p. 166).
\preformatted{
  # Gumbel-Hougaard [Kendall Tau=0.67267 (Salvadori et al. [2007])]
  Tyear <- 100; ANEP <- 1-1/Tyear; nsim <- 30000; ix <- 1:nsim
  1/(1-kfuncCOP(ANEP, cop=GHcop, para=3.055)) # 148.28 years
  BarT <- sapply(1:20, function(i) {
                   UV <- simCOP(n=nsim, cop=GHcop, para=3.055, graphics=FALSE)
                   nsim/sum(GHcop(UV$U, UV$V, para=3.055) > ANEP) })
  message("# BarBarT=",               round(mean(BarT), digits=2),
          " years and StdDev(BarT)=", round(  sd(BarT), digits=2)," years")
  # BarBarT=149.61 years and StdDev(BarT)=11.12 years
}
The mean of 20 repeats of a large sample simulation run for \eqn{F^\mathbf{GH}(\Theta{=}3.055)_K{(z{=}0.99)}} demonstrates empirical results that closely approximate theory \eqn{149.61 \approx 148.28}, and thus congruence is shown that the definition for \eqn{F_K(z)} must be correct and that for \eqn{K_\mathbf{C}(z)} is incorrect. The table 3.3 in Salvadori \emph{et al.} (2007) lists the secondary return period as 148.3 years, which obviously matches the output of \code{kfuncCOP} and empricial results shown.

Some additional details on secondary return period from Salvadori \emph{et al.} (2007). Letting \eqn{t_\star} be some critical joint exceedance probability level, \eqn{\overline{F}_K(z)} be the complement of \eqn{F_K(z)}, those authors (p. 166) name \emph{super-critical events} having \eqn{\overline{F}_K(t_\star) < 1 - t_\star}. Thus, the secondary return period  (\eqn{\overline{F}_K(t_\star)^{-1} = T_K}) must be greater than the primary return period (\eqn{t_\star^{-1}}), which is the case here (\eqn{T_K{=}148.3 > T{=}100}) (Salvadori \emph{et al.}, 2007, p. 166).

Salvadori \emph{et al.} (2007) provide considerable discussion of \eqn{T_K} and some highlights are:\cr
\eqn{\mbox{}\quad\bullet\quad\mbox{}}(p. 162) events equally or exceeding probability \eqn{F_K(1 - 1/t_\star)} or having return intervals \eqn{\ge T_K} \dQuote{represent [a] class of potentially dangerous events, the \emph{outliers}, and [\eqn{F_K} can be used to] introduce an \emph{ad hoc} return period for such destructive events.}\cr
\eqn{\mbox{}\quad\bullet\quad\mbox{}}(p. 162) \dQuote{primary return period [\eqn{T}] \ldots only takes into account the fact that a prescribed critical event is expected to appear once in a given time interval [\eqn{T}] \ldots \eqn{\overline{F}_K(t_\star)} provides the \emph{exact probability} that a potentially destructive event will happen at any given realization of \eqn{Z} \ldots and \eqn{T_K} gives the expected time for such an outlier to occur.}\cr
\eqn{\mbox{}\quad\bullet\quad\mbox{}}(p. 164) \dQuote{[\eqn{T}] only predicts that a critical event is \emph{expected} to appear once in a given time interval \ldots would be more important to be able to calculate (1) the probability that a super-critical [sic.] event will occur at any given realization of [\eqn{Z}], and (2) how long it takes, \emph{on average}, for a super-critical event to appear.}\cr
\eqn{\mbox{}\quad\bullet\quad\mbox{}}(p. 164) \dQuote{the function \eqn{\overline{F}_K} turns the difficult analysis of bivariate dynamics of \eqn{X} and \eqn{Y} into a simpler one-dimensional problem.}

}
\source{
The comprehensive demonstrations are shown in the \bold{Note} because of a sign convention and (or) probability convention incompatibility with the equation shown by Salvadori \emph{et al.} (2007, p. 148).  Initial source code development for \pkg{copBasic} was based on an hypothesis that the terms \dQuote{Kendall's Function} and \dQuote{Kendall Measure} might somehow have separate meanings---that the author must be blamed for misunderstanding the requisite nomenclature---this is evidently not true.

The \eqn{K_\mathbf{C}(z)} as shown herein simply can not reproduce \eqn{F_K(z; \mathbf{\Pi}) = z - z\log z} for the \eqn{\mathbf{\Pi}} copula unless the \dQuote{\eqn{-}} sign in the \eqn{K_\mathbf{C}(z)} equation is changed to a \dQuote{\eqn{+}} to become the \eqn{F_K(z)} definition as shown. The detective work needed for a valid function \code{kmeasCOP} was further complicated by fact that neither Durante and Sempi (2015), Joe (2014), Nelsen (2006), and others do not actually present a general equation for \eqn{F_K(z)} computation for any \eqn{\mathbf{C}(u,v)}.

Because of the subtle differences evidently between \dQuote{Kendall functions} (lower case), an explict derivation for \eqn{F_K(z; \mathbf{\Pi})} is informative to confirm what is meant by the \emph{Kendall Function} as defined by \eqn{F_K(z)}. Starting with \eqn{z = \mathbf{\Pi}(u,v) = uv}, then \deqn{v(z) = t = \mathbf{\Pi}^{(-1)}(u, z) = z/u\mbox{,\ and}}
\deqn{\frac{\delta}{\delta u} \mathbf{\Pi}(u,t) = \frac{\delta }{\delta u} u\,v \rightarrow v = \frac{z}{u}\mbox{,}}
substitution can now proceed:
\deqn{F_K(z; \mathbf{\Pi}) = z + \int_z^1 \frac{\delta}{\delta u}\mathbf{\Pi}(u,t)\,\mathrm{d}u\mbox = z + \int_z^1 \frac{z}{u}\,\mathrm{d}u{,}}
which simplfies to
\deqn{F_K(z; \mathbf{\Pi}) = z + [z\log(u)]\bigg|^1_z  = z + z[\log(1) - \log(z)] = z - z\log z\mbox{,}}
which matches the special case shown by Joe (2014) for the independence copula (\eqn{\mathbf{\Pi}}; \code{\link{P}}). It is obvious thus that the \dQuote{\eqn{+}} is needed in the \eqn{F_K(z)} definition in order to stay consistent with the basic form and integration limits shown by Salvadori \emph{et al.} (2007) for \eqn{K_\mathbf{C}(z)}.
}
\references{
Durante, F., and Sempi, C., 2015, Principles of copula theory: Boca Raton, CRC Press, 315 p.

Genest, C., Quessy, J.F., \enc{Rémillard}{Remillard}, B., 2006, Goodness-of-fit procedures for copula models based on the probability integral transformation: Scandinavian Journal of Statistics, v. 33, no. 2, pp. 337--366.

Joe, H., 2014, Dependence modeling with copulas: Boca Raton, CRC Press, 462 p.

Nelsen, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Nelsen, R.B., Quesada-Molina, J.J., \enc{Rodríguez-Lallena}{Rodriguez-Lallena}, J.A., \enc{Úbeda-Flores}{Ubeda-Flores}, M., 2001, Distribution functions of copulas---A class of bivariate probability integral transforms: Statistics and Probability Letters, v. 54, no. 3, pp. 277--282.

Nelsen, R.B., Quesada-Molina, J.J., \enc{Rodríguez-Lallena}{Rodriguez-Lallena}, J.A., \enc{Úbeda-Flores}{Ubeda-Flores}, M., 2003, Kendall distribution functions: Statistics and Probability Letters, v. 65, no. 3, pp. 263--268.

Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007, Extremes in nature---An approach using copulas: Dordrecht, Netherlands, Springer, Water Science and Technology Library 56, 292 p.
}
\author{ W.H. Asquith}
\seealso{ \code{\link{kfuncCOPinv}}, \code{\link{tauCOP}}, \code{\link{derCOP}}, \code{\link{derCOP2}}, \code{\link{derCOPinv}}, \code{\link{derCOPinv2}} }
\examples{
\dontrun{
# Salvadori et al. (2007, p. 148, fig. 3.5 [right])
zs <- seq(0,1, by=.01)
plot(zs, kmeasCOP(zs, cop=GHcop, para=5), log="y", type="l", lwd=4,
     xlab="Z <= z", ylab="Kendall Function", xlim=c(0,1), ylim=c(0.001,1))#}
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{return period (secondary)}
\keyword{Salvadori et al. (2007) Examples and Exercises}
\keyword{Joe (2014) Examples and Exercises}
\keyword{Durante and Sempi (2015) Examples and Exercises}
\keyword{Kendall Measure}
\keyword{Kendall Function}
\concept{Kendall distribution function}
\concept{Kendall function}
\concept{Kendall measure}
\concept{decomposition of Kendall tau}
\concept{decomposition of Kendall's tau}
\keyword{literature errors and inconsistencies}
