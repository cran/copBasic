\encoding{utf8}
\name{blomCOPss}
\alias{blomCOPss}
\title{Blomqvist (Schmid--Schmidt) Betas of a Copula}
\description{
Compute the \emph{Blomqvist (Schmid--Schmidt) Betas} \eqn{\beta^\diamond_\mathbf{C}} (Schmid and Schmidt, 2007) defined for arbitrary dimension \eqn{d} of a copula \eqn{\mathbf{C}_(u_1, \cdots, u_d; \Theta)} (\code{\link{COP}}) for parameters \eqn{\Theta}. The copula survival function is \eqn{\overline{\mathbf{C}}(u_1, \cdots, u_d; \Theta)} (\code{\link{surfuncCOP}}). The Beta, though the \pkg{copBasic} package is built around bivariate copula only, is defined as
\deqn{\beta^\diamond_\mathbf{C} = h_d(\mathbf{u}, \mathbf{v})\bigl[
\bigl(\mathbf{C}(\mathbf{u}) + \overline{\mathbf{C}}(\mathbf{v})\bigr) - g_d(\mathbf{u}, \mathbf{v})
\bigr]\mbox{,}}
where \eqn{h_d} and \eqn{g_d} are norming constants defined below. The superscript \eqn{\diamond} (diamond) is chosen for this package because of the alliteration to \dQuote{dimension.} The bold face for \eqn{\mathbf{u}} and \eqn{\mathbf{v}} are denote that these arguments are vectors of length \eqn{d} reflecting \dQuote{cutting points} on nonexceedance probabilities in each of the dimensions. The \eqn{\mathbf{u}} functions as the arguments \eqn{(u,v)} pair used in copula of this package and represents the first cutting point for a \eqn{\mathrm{Pr}[U \le u, V \le v] = \mathbf{C}(u,v)}, and \eqn{\mathbf{v}} functions as the arguments \eqn{u,v} pair for this package and represents the second cutting point for a \eqn{\mathrm{Pr}[U > u, V > v] = 1 - u - v + \mathbf{C}(u,v) = \overline{\mathbf{C}}(u,v)}. This notation of vectored (bold face) and nonvectored \dQuote{u} and \dQuote{v} is a little obtuse but as the properties of \eqn{\beta^\diamond_\mathbf{C}} are summarized clarity for the reader is anticipated. In short, the \eqn{\mathbf{u}} will reference the coordinate pairs in the lower right quadrant and the \eqn{\mathbf{v}} will reference the coordinate pairs in the upper right quadrant.

The norming constant \eqn{h_d} is defined as
\deqn{h_d(\mathbf{u}, \mathbf{v}) = \frac{1}{\bigl( \mathrm{min}(u_1,   \cdots,   u_d) + \mathrm{min}(1-v_1, \cdots, 1-v_d) - g_d(\mathbf{u}, \mathbf{v})\bigr)}\mbox{,}}
and \eqn{g_d} is defined as
\deqn{g_d(\mathbf{u}, \mathbf{v}) = \prod^d_{i=1}u_i + \prod^d_{i=1}(1-v_i)\mbox{,}}
where the cutting points \eqn{\mathbf{u}} and \eqn{\mathbf{v}} are in a domain \eqn{D : \{(\mathbf{u}, \mathbf{v})\} \in [0,1]^{2d}} given \eqn{\mathbf{u} \le \mathbf{v}} and \eqn{\mathbf{u} > 0} or \eqn{\mathbf{v} < \mathbf{1}}. The reader must careful remember that these \eqn{\mathbf{u}} and \eqn{\mathbf{v}} are vectors of probabilities.

The norming constants provide for \eqn{-1 \le \beta^\diamond_\mathbf{C} \le +1}. Using the function argument defaults for \eqn{d=2} dimensions \eqn{\mathbf{u} = (1,1)/2} for \code{uu} and \eqn{\mathbf{v} = (1,1)/2} for \code{vv}, results in (1) \eqn{\beta^\diamond_\mathbf{C} = 1} if \eqn{\mathbf{C} = \mathbf{M}} \emph{comonotonicity copula} (\code{\link{M}}) (\code{blomCOPss(cop=M) == 1}), (2) \eqn{\beta^\diamond_\mathbf{C} = 0} if \eqn{\mathbf{C} = \mathbf{P}} \emph{independence copula} (\code{\link{P}}) (\code{blomCOPss(cop=P) == 0}), and (3) if \eqn{\mathbf{C} = \mathbf{W}} \emph{countermonotonicity copula} (\code{\link{W}})\eqn{\beta^\diamond_\mathbf{C} = 1} (\code{blomCOPss(cop=W) == -1}).

Schmid and Schmidt (2007) list three important cases extending the \eqn{\mathbf{M}} and \eqn{\mathbf{P}} examples. First, \eqn{\beta^\diamond_\mathbf{C}(\mathbf{1/2}, \mathbf{1/2}) = \beta_\mathbf{C}(1/2, 1/2)}, which is \emph{Blomqvist Beta} (\eqn{\beta_\mathbf{C}(1/2, 1/2)}) (\code{\link{blomCOP}}) and measures overall dependence.

Second, \eqn{\beta^\diamond_\mathbf{C}(\mathbf{u}, \mathbf{v})} with \eqn{\mathbf{u} < 1/2 < \mathbf{v}}, which measures dependence in the tail regions. (Note, the author of \pkg{copBasic} thinks \dQuote{regions} as a plural is need in the previous sentence; Schmid and Schmidt use the singular \dQuote{region.} This is potentially important as seemingly simultaneous tail dependency in the lower and upper perspectives would be provided. More discussion is provided in \bold{Examples}.)

Third and presumably very important in practical applications, \eqn{\mathrm{lim}_{p\downarrow 0}\, \beta^\diamond_\mathbf{C}(\mathbf{p}, \mathbf{1}) = \lambda^L_{\beta^\diamond_\mathbf{C}}} for \eqn{\mathbf{p} = \mathbf{u} = (p,\cdots,p)} measures lower-tail dependence. This measure is equal to the \emph{lower-tail dependence parameter} \eqn{\lambda^L_\mathbf{C} = \lambda^L_{\beta^\diamond_\mathbf{C}}} without some of the computational nuances required as  \eqn{\lambda^L_\mathbf{C}} is defined at \code{\link{taildepCOP}}.

Schmid and Schmidt (2007) do not list how the \emph{upper-tail dependence parameter} \eqn{\lambda^U_\mathbf{C}} could be computed in terms of \eqn{\beta^\diamond_\mathbf{C}}. The expression for study of the upper-tail dependency is \eqn{\lambda^U_{\beta^\diamond_\mathbf{C}} = \beta^\diamond_\mathbf{C}(\mathbf{0}, \mathbf{p})} for \eqn{\mathbf{p} = \mathbf{v} = (p,\cdots,p)} as \eqn{p \rightarrow 0^+}, and \eqn{\lambda^U_\mathbf{C} = \lambda^U_{\beta^\diamond_\mathbf{C}}} without some of the computational nuances required as \eqn{\lambda^U_\mathbf{C}} is defined at \code{\link{taildepCOP}}. These tail dependencies are computed and compared in the \bold{Examples} and confirmation of this function being used to estimate both tail-dependency parameters is confirmed.
}
\usage{
blomCOPss(cop=NULL, para=NULL, uu=rep(0.5, 2), vv=rep(0.5, 2), trap.nan=TRUE,
          as.sample=FALSE, ctype=c("weibull", "hazen", "1/n",
                                   "bernstein", "checkerboard"), ...)
}
\arguments{
  \item{cop}{A copula function;}
  \item{para}{Vector of parameters or other data structure, if needed, to pass to the copula;}
  \item{uu}{The vector for \eqn{\mathbf{u}} and the defaults with \code{vv} as such for same operation as \code{\link{blomCOP}} (\eqn{\beta^\diamond_\mathbf{C}(\mathbf{1/2}, \mathbf{1/2})});}
  \item{vv}{The vector for \eqn{\mathbf{v}} and the defaults with \code{uu} as such for same operation as \code{\link{blomCOP}} (\eqn{\beta^\diamond_\mathbf{C}(\mathbf{1/2}, \mathbf{1/2})});}
  \item{trap.nan}{A logical to trigger 0 if \eqn{(0,0)} is \code{NaN} or if \eqn{(1,1)} is \code{NaN}. This feature is present on a package-specific purpose because the \code{\link{PSP}} copula deliberately retains edge \code{NaN} as a stress case;}
  \item{as.sample}{A logical controlling whether an optional \R \code{data.frame} in \code{para} is used to compute the  \eqn{\hat\beta^\diamond_\mathbf{C}} at which point the \code{ctype} argument will be passed to \code{\link{EMPIRcop}};}
    \item{ctype}{Argument of the same as \code{\link{EMPIRcop}}; and}
  \item{...}{Additional arguments to pass to the copula.}
}
\value{
  The \eqn{\beta^\diamond_\mathbf{C}} is returned.
}
\note{
Sample estimation of the \eqn{\beta^\diamond_\mathbf{C}} is possible. The \code{as.sample} triggers internally a call to the \emph{empirical copula} (\eqn{\mathbf{C}_n}) (\code{\link{EMPIRcop}}) for the \code{ctype} for the copula and its survival function form. Expansive more details are provided by \code{\link{taildepCOP}} (section \bold{Note}::\emph{DEMONSTRATION (Tail Dependence)}). A comparison of the \eqn{\hat\lambda^U_{\beta^\diamond_\mathbf{C}}} and \eqn{\hat\lambda^U_{\beta^\diamond_\mathbf{C}}} is made.
}
\references{
Schmid, Friedrich, and Schmidt, Rafael, 2007, Nonparametric inference on multivariate versions of Blomqvist's beta and related measures of tail dependence: Metrika, v. 66, pp. 323--354, \doi{10.1007/s00184-006-0114-3}.
}
\author{W.H. Asquith}
\seealso{\code{\link{blomCOP}}, \code{\link{blomatrixCOP}}
}
\examples{
blomCOP(  cop=PSP) # [1] 0.3333333
blomCOPss(cop=PSP) # [1] 0.3333333

\dontrun{
# The calls below for blomCOPss() are technically the same for sample versions.
UV <- simCOP(1000, cop=PSP, graphics=FALSE)  # HatBeta(0.1,0.9) = 0.277___
blomCOPss(para=UV, cop=EMPIRcop,   uu=c(0.1,0.1), vv=c(0.90,0.90))
blomCOPss(para=UV, as.sample=TRUE, uu=c(0.1,0.1), vv=c(0.90,0.90)) #}

\dontrun{
set.seed(1)
para <- c(3, 6) # define parameters of two-parameter GHcop
UV <- simCOP(1000, cop=GHcop, para=para) # simulate to show general structure

# compute the tail dependencies from havling into the limits
taildepCOP(cop=GHcop, para=para, plot=TRUE)
# lower tail dependency = 0.96222
# upper tail dependency = 0.74008
# The two parameters influence how strongly the tail dependencies are.

# Schmid and Schmidt (2007, eq. 24) define the lower-tail dependency in terms of
# the Beta and p-->0 Beta(c(p,p), c(1,1)). Lets compute these and produce content
# suitable to show on the tail-dependency plot that the assertion for the lower
# dependency by Beta() is correct, which it is and then extend to the upper-tail
# dependency parameter that the authors seem to not have defined.
usr <- par()$usr[1:2]         # grab horizontal edges of the plot, and set up the
uuLO <- rep(pnorm(usr[1]), 2) # the uu for the lower tail and the vv for the upper
vvUP <- rep(pnorm(usr[2]), 2) # tail and then plot both with overplotting symbols
# lower-tail estimate and see how it plots along the value from taildepCOP()
SchmidsL <- blomCOPss(cop=GHcop, para=para, uu=uuLO, vv=c(1,1))
points(usr[1], SchmidsL, col="darkgreen", cex=2, pch=1, lwd=2)
points(usr[1], SchmidsL, col="darkgreen", cex=2, pch=3, lwd=2)
points(usr[1], SchmidsL, col="darkgreen", cex=2, pch=4, lwd=2)
# upper-tail estimate and see how it plots along the value from taildepCOP()
SchmidsU <- blomCOPss(cop=GHcop, para=para, uu=c(0,0), vv=vvUP)
points(usr[2], SchmidsU, col="darkgreen", cex=2, pch=1, lwd=2)
points(usr[2], SchmidsU, col="darkgreen", cex=2, pch=3, lwd=2)
points(usr[2], SchmidsU, col="darkgreen", cex=2, pch=4, lwd=2)
# SchmidsL lower tail dependency = 0.962224
# SchmidsU upper tail dependency = 0.740079
# The author has an expectation that the SchmidsL and SchmidsU values are
# more reliable than those stemming from taildepCOP() because of the limiting
# behavior (or its implementation therein) compared to direct computation by
# blomCOPss().

# Mow for sake of curiosity, let us see how the trajectory of the Blomqvist
# (Schmid--Schmidt) Betas at arriving at the tail dependencies as p-->0|1.
# It is very informative that the trajectories of blomCOPss() and taildepCOP()
# as each hones towards the two dependency parameters are different and this
# highlights the fact that the computational underpinnings are different.
psl <- pnorm(seq(0, usr[1], by=-diff(range(c(0, usr[1])))/1000))
lines(qnorm(psl), sapply(psl, function(p) {
              blomCOPss(cop=GHcop, para=para, uu=rep(p, 2), vv=c(1,1)) }),
      col="darkgreen", lty=2, lwd=2)
psu <- pnorm(seq(0, usr[2], by= diff(range(c(0, usr[2])))/1000))
lines(qnorm(psu), sapply(psu, function(p) {
              blomCOPss(cop=GHcop, para=para, uu=c(0,0), vv=rep(p, 2)) }),
      col="darkgreen", lty=2, lwd=2) # }
}
\keyword{copula (characteristics)}
\keyword{copula (properties)}
\keyword{Blomqvist Beta}
\keyword{Blomqvist (Schmid-Schmidt) Beta}
\keyword{copula (tail characteristics)}
\keyword{copula (tail properties)}
\keyword{tail dependence}
