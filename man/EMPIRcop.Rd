\encoding{utf8}
\name{EMPIRcop}
\alias{EMPIRcop}
\title{ The Bivariate Empirical Copula }
\description{
The bivariate empirical copula (Nelson, 2006, p. 219) for a bivariate sample of length \eqn{n} is in words

\deqn{\mathbf{C}_n\biggl(\frac{i}{n}, \frac{j}{n}\biggr) = \frac{\mathrm{number\ of\ pairs\ (}x,y\mathrm{)\ with\ }x \le x_{(i)}\mathrm{\ and\ }y \le y_{(j)}}{n}\mbox{,}}

where \eqn{x_{(i)}} and \eqn{y_{(i)}}, \eqn{1 \le i,j \le n} and in tighter notation

\deqn{\mathbf{C}_{n}(u,v) =
     \frac{1}{n}\sum_{i=1}^n \mathbf{1}\bigl(u_{obs} \le u, v_{obs} \le v \bigr)\mbox{,}}
where \eqn{u_{obs}} and \eqn{v_{obs}} are the estimated nonexceedance probabilities in unranked form as computed by Weibull plotting positions (\eqn{i/(n+1)}, for example) or other plotting position formula. The example shown in this documentation sufficiently clarifies this concept. The \code{sort=FALSE} of the function \code{pp} of the \pkg{lmomco} package is present (and was added to that package) just for the very operation needed here for \eqn{u_{obs}} and \eqn{v_{obs}}. Weibull plotting positions seem favorable because they provide unbiased nonexceedance probabilities for all distributions.


The bivariate empirical copula can be extended nonparametrically as the Bernstein copula \cr (\enc{Hernández-Maldonado}{Hernandez-Maldonado}, \enc{Díaz-Viera}{Diaz-Viera}, and Erdely, 2012) and is formulated as

\deqn{\mathbf{\hat{C}_{n}}(u,v) = \sum_{i=1}^n\sum_{j=1}^n \mathbf{C}_{n}\biggl(\frac{i}{n},\frac{j}{n}\biggr) \times \eta(i,j; u,v)\mbox{,}}

where the individual Bernstein weights \eqn{\eta(i,j)} for the \eqn{k}th paired value of the \eqn{u} and \eqn{v} vectors  are

\deqn{\eta(i,j; u,v) = {n \choose i} u^i (1-u)^{n-i} \times {n \choose j} u^j (1-u)^{n-j}\mbox{.}}

The Bernstein extension, albeit conceptually pure in its shuffling by binomial coefficients and left- and right-tail weightings, can burn serious CPU time as inspection of the equations above seemingly indicates a nest of four \code{for()} loops in \R. (The native \R code of \pkg{copBasic} uses the \code{sapply()} function in \R liberally for substantial but not blazing fast speed increase.) The Bernstein extension results in a somewhat smoother empirical copula surface.
}
\usage{
EMPIRcop(u, v, para=NULL, bernstein=FALSE, bernsteinprogress=TRUE, ...)
}
\arguments{
  \item{u}{Nonexceedance probability \eqn{u} in the \eqn{X} direction;}
  \item{v}{Nonexceedance probability \eqn{v} in the \eqn{Y} direction;}
  \item{para}{A vector (single element) of parameters---the U-statistics of the data (see example). Alternatively, \code{para} can be a \code{list} holding a \code{para} as would be done if it were a vector, but the arguments \code{bernstein} and \code{bernsteinprogress} can be optionally included---this feature is provide so that the Bernstein refinement can be controlled within the context of other functions calling \code{EMPIRcop} such as by \code{\link{level.curvesCOP}};}
  \item{bernstein}{A logical as to whether the Bernstein weights should be incorporated;}
  \item{bernsteinprogress}{The Bernstein copula extension is CPU intensive, so a splash counter is pushed to the console via \code{message()} function in \R so as to not discourage the user, and}
  \item{...}{Additional arguments to pass.}
}
\value{
  Value(s) for the copula are returned.
}
\references{
\enc{Hernández-Maldonado}{Hernandez-Maldonado}, V., \enc{Díaz-Viera}{Diaz-Viera}, M., and Erdely, A., 2012, A joint stochastic simulation method using the Berstein copula as a flexible tool for modeling nonlinear dependence structures between petrophysical properties: Journal of Petroleum Science and Engineering, v. 90--91, pp. 112--123.

Nelson, R.B., 2006, An introduction to copulas: New York, Springer, 269 p.

Salvadori, G., De Michele, C., Kottegoda, N.T., and Rosso, R., 2007,
Extremes in Nature---An approach using copulas: Springer, 289 p.
}
\author{ W.H. Asquith }
\seealso{ \code{\link{simCOP}}, \code{\link{PSP}}, \code{\link{level.curvesCOP}} }
\examples{
psp <- simCOP(n=34, cop=PSP, ploton=FALSE, points=FALSE) * 150
# Pretend psp is real data, the * 150 is to clearly get into an arbitrary unit system.

# The sort=FALSE is critical in the following two calls
fakeU <- lmomco::pp(psp[,1], sort=FALSE) # Weibull plotting position i/(n+1)
fakeV <- lmomco::pp(psp[,2], sort=FALSE) # Weibull plotting position i/(n+1)
uv <- data.frame(U=fakeU, V=fakeV); # our U-statistics

# The next two values should be REAL close if n above were say 1000.
print(EMPIRcop(0.4,0.6,para=uv))
print(PSP(0.4,0.6))
# But do not run with n much larger than above if the Bernstein comparison is wanted.
\dontrun{
# Now let us construct as many as three sets of level curves
level.curvesCOP(cop=PSP); # TRUE, parametric, fast,  BLACK CURVES
}
\dontrun{
# Empirical copulas can consume lots of CPU.
# RED CURVES, if n is too small, uniroot() errors might be triggered
level.curvesCOP(cop=EMPIRcop, para=uv, delu=0.04, col=2, ploton=FALSE)

# GREEN CURVES (large CPU committment)
# Bernsteinprogress is uninformative because level.curves has taken over control.
para <- list(para=uv, bernstein=TRUE, bernsteinprogress=FALSE)
level.curvesCOP(cop=EMPIRcop, para=para, delu=0.05, col=3, ploton=FALSE)
# The delu is increased for faster execution but more important,
# notice the greater smoothness of the Bernstein refinement
}
\dontrun{
diagCOP(cop=EMPIRcop, para=uv)
}
\dontrun{
# Experimental from R Graphics by Murrell (2005, p.112)
"trans3d" <- function(x,y,z, pmat) {
   tmat <- cbind(x,y,z,1) \%*\% pmat; return(tmat[,1:2] / tmat[,4])
}

the.grid <- EMPIRgrid(para=uv)
the.diag <- diagCOP(cop=EMPIRcop, para=uv, ploton=FALSE, lines=FALSE)

the.persp <- persp(the.grid$empcop, theta=-25, phi=20,
                   xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")
the.trace <- trans3d(the.diag$t, the.diag$t, the.diag$diagcop, the.persp)
lines(the.trace, lwd=2, col=2)

# the following could have been used as an alternative to call persp()
the.persp <- persp(x=the.grid$u, y=the.grid$v, z=the.grid$empcop, theta=-25, phi=20,
                   xlab="U VARIABLE", ylab="V VARIABLE", zlab="COPULA C(u,v)")
}
}
\keyword{empirical copula}
\keyword{copula (empirical)}
\keyword{copula}

